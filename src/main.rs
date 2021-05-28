use crossbeam_channel::{select, Receiver, Sender};
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use ropey::Rope;
use std::{borrow::Cow, collections::HashMap, convert::TryInto, error::Error, io, panic};
use tui::{
    backend::{Backend, CrosstermBackend},
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Widget},
    Terminal,
};

use parse::{Kin, KinIndex, ParseEvent, ParseTree, Snapshot};
use text::{EditCommand, Go, ModeSelect};
use tree_sitter::Node;

pub mod parse;
mod setup;
mod text;

fn main() -> Result<(), Box<dyn Error>> {
    better_panic::install();

    let backend = CrosstermBackend::new(io::stdout());
    let terminal = Terminal::new(backend)?;

    setup::setup_panic_hook();
    setup::setup_terminal()?;

    let rope = Rope::from_str(include_str!("initial.txt"));

    let (snap_tx, tree_rx) = parse::setup_parser();
    let ui_events = setup::setup_ui_events();

    // does it make sense to queue up the initial parse right away?
    // or should we wait for the thread to be ready?
    let first_snapshot = Snapshot { text: rope.clone().into() };
    snap_tx.send(first_snapshot)?;

    let mut editor = Editor {
        rope,
        viewport: Viewport::default(),
        writer: Writer::default(),
        parse_tree: None,
        snap_tx,
        tree_rx,
        ui_events,
        terminal,
        kin_index: None,
    };
    let result = editor.main_loop();
    setup::cleanup_terminal()?;
    result
}

pub struct Editor<T: Backend> {
    rope: Rope,
    viewport: Viewport,
    writer: Writer,
    /// The latest parse tree we received from the parse thread
    parse_tree: Option<ParseTree>,
    snap_tx: Sender<Snapshot>,
    tree_rx: Receiver<ParseEvent>,
    ui_events: Receiver<Event>,
    terminal: Terminal<T>,
    kin_index: Option<KinIndex>,
}

impl<T: Backend> Editor<T> {
    pub fn main_loop(&mut self) -> Result<(), Box<dyn Error>> {
        let mut text_viewport: Rect;
        loop {
            // FUTURE WORK: we have to change our approach if large files are loaded.
            // don't remove this assert until some suitable solution is implemented
            // (patch our code to only reparse the items that changed, not the entire buffer)
            // (alternatively, if we could pass a rope into parser.parse, we'd save the string allocation)
            assert!(
                self.rope.len_bytes() < 1_000_000,
                "text buffer is WAYTOOLONG ({}b)",
                self.rope.len_bytes()
            );

            if let (Some(kin_index), Some(parse_tree)) = (self.kin_index.as_ref(), self.parse_tree.as_ref()) {
                let root_node = parse_tree.tree.root_node();
                assert_eq!(root_node.kind(), "program");
                assert_eq!(root_node.start_position().column, 0);

                // now print the tree
                let tree_widget = TreeWidget {
                    root: root_node,
                    source_code: &parse_tree.snapshot.text,
                    kin_index,
                };
                text_viewport = render_layout(
                    &mut self.terminal,
                    &self.rope,
                    &self.viewport,
                    self.writer.status(),
                    Some(tree_widget),
                )?;
            } else {
                // parser not loaded yet?
                // let's just print the text buffer, no tree
                text_viewport = render_layout(
                    &mut self.terminal,
                    &self.rope,
                    &self.viewport,
                    self.writer.status(),
                    None
                )?;
            };

            if !self.handle_events(text_viewport)? {
                return Ok(());
            }
        }
    }

    pub fn handle_events(&mut self, text_viewport: Rect) -> Result<bool, Box<dyn Error>> {
        // first, block on input
        select! {
            recv(self.ui_events) -> message => {
                match message? {
                    Event::Key(key_event) => {
                        if !self.handle_key_event(key_event, text_viewport)? {
                            return Ok(false);
                        }
                    }
                    Event::Mouse(_) => (),
                    Event::Resize(_w, _h) => (),
                }
            }
            recv(self.tree_rx) -> message => {
                match message? {
                    ParseEvent::LanguageIndexed(index) => {
                        assert!(self.kin_index.is_none());
                        self.kin_index = Some(index);
                    }
                    ParseEvent::TreeParsed(parse_tree) => {
                        // should we check `parse_tree.snapshot.id` against `self.rope_id`?
                        // these tree parses are all queued in-order so it doesn't seem necessary yet...
                        self.parse_tree = Some(parse_tree);
                    }
                }
            }
        }

        // now if any other events have been enqueued, process them too.

        // careful. if our update operations are slow and they type quickly,
        // we could end up spending a lot of time in here without repainting...
        loop {
            select! {
                recv(self.ui_events) -> message => {
                    match message? {
                        Event::Key(key_event) => {
                            if !self.handle_key_event(key_event, text_viewport)? {
                                return Ok(false);
                            }
                        }
                        Event::Mouse(_) => (),
                        Event::Resize(_w, _h) => (),
                    }
                }
                recv(self.tree_rx) -> message => {
                    match message? {
                        ParseEvent::LanguageIndexed(_) => unreachable!(),
                        ParseEvent::TreeParsed(parse_tree) => {
                            self.parse_tree = Some(parse_tree);
                        }
                    }
                }
                default => return Ok(true),
            }
        }
    }

    pub fn handle_key_event(&mut self, event: KeyEvent, tv: Rect) -> Result<bool, Box<dyn Error>> {
        // first, convert the input into an EditCommand
        let command = match self.interpret_key_event(event) {
            Some(c) => c,
            None => return Ok(true),
        };

        if !event.modifiers.is_empty() {
            return Ok(true);
        }

        let Viewport {
            ref mut char_offset,
            ref mut cursor,
            ref mut scroll,
        } = self.viewport;
        let right_edge = if self.writer.insert_mode { 1 } else { 2 };

        // Q: exactly which screen size do we use below?
        // A: we should use `viewport` corresponding to whatever is still painted on the screen.
        // since we're in the middle of an event processing loop, we might run into a resize event
        // but the event should not apply until we repaint.
        let text_size = (tv.width as isize, tv.height as isize);
        assert!(text_size.0 > 0 && text_size.1 > 0);

        let total_line_count = self.rope.len_lines().try_into()?;
        let at_eof = cursor.1 >= total_line_count;

        // must mark the rope dirty if you edit it
        // TODO unit test with hashing to ensure we didn't miss anything
        let mut dirty = false;

        match command {
            EditCommand::Move(Go::Left) => {
                if cursor.0 > 0 {
                    cursor.0 -= 1;
                    *char_offset -= 1;
                }
            }
            EditCommand::Move(Go::Right) => {
                let line_idx = cursor.1.try_into()?;
                if let Some(line) = self.rope.lines_at(line_idx).next() {
                    // is the cursor at the end of the line?
                    let eol = cursor.0 + right_edge >= line.len_chars().try_into()?;
                    if !eol {
                        cursor.0 += 1;
                        *char_offset += 1;
                    }
                }
            }
            EditCommand::Move(Go::Up) => {
                if cursor.1 > 0 {
                    debug_assert_eq!(cursor.1 as usize, self.rope.char_to_line(*char_offset));
                    // TODO editor state should remember which column we moved vertically from
                    // so that holding up/down keeps you aligned horizontally despite the ragged right
                    // are we past the end of the line?
                    let line_idx = (cursor.1 - 1).try_into()?;
                    if let Some(line) = self.rope.lines_at(line_idx).next() {
                        let len: isize = line.len_chars().try_into()?;
                        // newline counts as a char, GUH
                        let right = (len - right_edge).max(0);
                        cursor.0 = cursor.0.min(right);
                        cursor.1 -= 1;
                        // okay, we've moved the cursor up; compute the new char offset
                        let line_offset = self.rope.line_to_char(line_idx);
                        *char_offset = line_offset + (cursor.0 as usize);
                        // basic check
                        debug_assert_eq!(cursor.1 as usize, self.rope.char_to_line(*char_offset));
                    }
                }
            }
            EditCommand::Move(Go::Down) => {
                if !at_eof {
                    debug_assert_eq!(cursor.1, self.rope.char_to_line(*char_offset) as isize);
                    // DRY
                    // are we past the end of the line?
                    let line_idx = (cursor.1 + 1).try_into()?;
                    if let Some(line) = self.rope.lines_at(line_idx).next() {
                        let len: isize = line.len_chars().try_into()?;
                        let right = (len - right_edge).max(0);
                        cursor.0 = cursor.0.min(right);
                        cursor.1 += 1;
                        // compute the new char offset
                        let line_offset = self.rope.line_to_char(line_idx);
                        *char_offset = line_offset + (cursor.0 as usize);
                        // basic check
                        debug_assert_eq!(cursor.1, self.rope.char_to_line(*char_offset) as isize);
                    }
                }
            }
            EditCommand::Move(Go::BOL) => unimplemented!(),
            EditCommand::Move(Go::EOL) => unimplemented!(),
            EditCommand::Move(Go::NextLine) => unimplemented!(),
            EditCommand::Quit => return Ok(false),
            EditCommand::Backspace => unimplemented!(),
            EditCommand::Delete => unimplemented!(),
            EditCommand::Append(char) => {
                assert!(self.writer.insert_mode, "trying to insert in normal mode?");
                self.rope.insert_char(*char_offset, char);
                cursor.0 += 1;
                *char_offset += 1;
                dirty = true;
            }
            EditCommand::SetMode(mode) => {
                let insert_mode = mode != ModeSelect::Normal;
                let mode_changed = self.writer.insert_mode != insert_mode;
                self.writer.insert_mode = insert_mode;

                assert!(mode_changed, "SetMode: mode was already {:?}", insert_mode);

                // press 'a' to insert after the cursor, 'A' after the line
                // press 'i' to insert before the cursor, 'I' before the line
                // press Esc to return to normal mode
                match mode {
                    ModeSelect::InsertAfterCursor => {
                        let at_eol = match self.rope.lines_at(cursor.1.try_into()?).next() {
                            Some(line) => cursor.0 + right_edge >= line.len_chars().try_into()?,
                            None => true,
                        };
                        // if we are before EOL, then advance one as we switch into insert mode
                        if !at_eol {
                            cursor.0 += 1;
                            *char_offset += 1;
                        }
                    }
                    ModeSelect::InsertAfterLine => unimplemented!(),
                    ModeSelect::InsertBeforeCursor => (),
                    ModeSelect::InsertBeforeLine => unimplemented!(),
                    ModeSelect::Normal => {
                        // move cursor one to the left
                        if cursor.0 > 0 {
                            cursor.0 -= 1;
                            *char_offset -= 1;
                        }
                    }
                }
            }
        }

        // have the text buffer scroll with the cursor
        if scroll.0 > cursor.0 {
            scroll.0 = cursor.0; // cursor pushes screen leftwards
        } else if scroll.0 + text_size.0 <= cursor.0 {
            scroll.0 = cursor.0 - text_size.0 + 1; // right
        }

        if scroll.1 > cursor.1 {
            scroll.1 = cursor.1; // up
        } else if scroll.1 + text_size.1 <= cursor.1 {
            scroll.1 = cursor.1 - text_size.1 + 1; // down
        }

        // was the rope affected? notify the parser
        if dirty {
            // for now, convert the WHOLE rope into a heap-allocated String.
            let text_snapshot: String = self.rope.clone().into();
            // and send it to the parser thread
            // (obviously the goal here is to send just the edits instead)
            let snapshot = Snapshot {
                text: text_snapshot,
            };
            self.snap_tx.send(snapshot)?;
        }

        Ok(true)
    }

    pub fn interpret_key_event(&self, event: KeyEvent) -> Option<EditCommand> {
        // universal movement commands
        use EditCommand::Move;
        match event.code {
            KeyCode::Left => return Some(Move(Go::Left)),
            KeyCode::Right => return Some(Move(Go::Right)),
            KeyCode::Up => return Some(Move(Go::Up)),
            KeyCode::Down => return Some(Move(Go::Down)),
            _ => (),
        }

        use KeyCode::Char;

        if self.writer.insert_mode {
            // most key codes will append a char here
            match event.code {
                Char(c) => Some(EditCommand::Append(c)),
                KeyCode::Enter => Some(EditCommand::Append('\n')),
                KeyCode::Esc => Some(EditCommand::SetMode(ModeSelect::Normal)),
                _ => None,
            }
        } else if event.modifiers.is_empty() {
            match event.code {
                Char('a') => Some(EditCommand::SetMode(ModeSelect::InsertAfterCursor)),
                Char('h') => Some(Move(Go::Left)),
                Char('i') => Some(EditCommand::SetMode(ModeSelect::InsertBeforeCursor)),
                Char('j') => Some(Move(Go::Down)),
                Char('k') => Some(Move(Go::Up)),
                Char('l') => Some(Move(Go::Right)),
                Char('q') => Some(EditCommand::Quit),
                Char('x') => Some(EditCommand::Delete),
                Char('$') => Some(Move(Go::EOL)),
                Char('^') => Some(Move(Go::BOL)),
                KeyCode::Enter => Some(EditCommand::Move(Go::NextLine)),
                _ => None,
            }
        } else if event.modifiers == KeyModifiers::SHIFT {
            match event.code {
                Char('A') => Some(EditCommand::SetMode(ModeSelect::InsertAfterLine)),
                Char('I') => Some(EditCommand::SetMode(ModeSelect::InsertBeforeLine)),
                _ => None,
            }
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Viewport {
    pub cursor: (isize, isize),
    pub scroll: (isize, isize),
    pub char_offset: usize,
}

#[derive(Clone, Debug, Default)]
pub struct Writer {
    pub insert_mode: bool,
}

impl Writer {
    pub fn status(&self) -> StatusLine {
        let Writer { insert_mode } = *self;
        StatusLine { insert_mode }
    }
}

#[derive(Clone, Debug)]
pub struct StatusLine {
    pub insert_mode: bool,
}

/// Returns the viewport [Rect] of the upper text window.
pub fn render_layout<T: Backend>(
    terminal: &mut Terminal<T>,
    rope: &Rope,
    viewport: &Viewport,
    status: StatusLine,
    tree_widget: Option<TreeWidget>,
) -> Result<Rect, Box<dyn Error>> {
    let mut text_viewport = None;
    let Viewport { cursor, scroll, .. } = *viewport;

    terminal.draw(|frame| {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .margin(1)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
            .split(frame.size());

        // top half: render text buffer
        let mut text_border = Block::default().borders(Borders::ALL);
        let text_area = text_border.inner(chunks[0]);
        text_viewport = Some(text_area);
        let status = format!(
            "({}, {})─{}x{}{}",
            cursor.0, cursor.1, text_area.width, text_area.height,
            match status.insert_mode {
                true => "─[INSERT]",
                false => "",
            }
        );
        text_border = text_border.title(status);
        frame.render_widget(text_border, chunks[0]);
        let text_content = RopeWindowWidget { rope, scroll };
        frame.render_widget(text_content, text_area);

        // bottom half: show tree
        let tree_border = Block::default().title("Tree").borders(Borders::ALL);
        let tree_area = tree_border.inner(chunks[1]);
        frame.render_widget(tree_border, chunks[1]);
        if let Some(widget) = tree_widget {
            frame.render_widget(widget, tree_area);
        }
    })?;

    let Rect {
        x,
        y,
        width,
        height,
        ..
    } = text_viewport.unwrap();
    // calculate the cursor position
    let cursor_inside_window = cursor.0 >= scroll.0
        && cursor.1 >= scroll.1
        && cursor.0 < scroll.0 + width as isize
        && cursor.1 < scroll.1 + height as isize;

    // actually reposition the cursor
    if cursor_inside_window {
        let off_x = (cursor.0 - scroll.0) as u16;
        let off_y = (cursor.1 - scroll.1) as u16;
        terminal.set_cursor(x + off_x, y + off_y)?;
        terminal.show_cursor()?;
    } else {
        terminal.hide_cursor()?;
    }

    Ok(text_viewport.unwrap())
}

pub struct RopeWindowWidget<'a> {
    pub rope: &'a Rope,
    /// Number of (columns, rows) to skip
    pub scroll: (isize, isize),
}

impl<'a> Widget for RopeWindowWidget<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let style = Style::default();
        let scroll = self.scroll;

        let rope_lines: isize = self.rope.len_lines().try_into().unwrap();
        for y in 0..area.height {
            let line_y = y as isize + scroll.1;
            if line_y >= rope_lines {
                break;
            }
            if line_y < 0 {
                continue; // O(n) line skipping, guh
            }
            let mut line = self.rope.line(line_y as usize);
            let mut line_len = line.len_chars(); // look out for characters with weird widths...?
            let mut x = 0u16;
            if scroll.0 > 0 {
                // chop the left side of the line off if need be
                let clip_left = line_len.min(scroll.0 as usize);
                line = line.slice(clip_left..);
                line_len -= clip_left;
            } else if scroll.0 < 0 {
                // push the line rightwards
                x = area.width;
                if let Ok(n) = (-scroll.0).try_into() {
                    x = x.min(n);
                }
            }

            // and chop off the right side to fit
            line_len = line_len.min((area.width - x).try_into().unwrap());

            if line_len > 0 {
                let cow: Cow<str> = line.into();
                let (_x, _y) = buf.set_stringn(x + area.x, y + area.y, cow, line_len, style);
            }
        }
    }
}

/// Prints a hierarchial tree description of a tree-sitter [Node] subtree.
pub struct TreeWidget<'a> {
    pub root: Node<'a>,
    pub source_code: &'a str,
    /// Generated by [parse::index_kins].
    pub kin_index: &'a HashMap<u16, Kin>,
}

impl<'a> Widget for TreeWidget<'a> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let area_width = area.width as usize;
        let mut cursor = self.root.walk();
        let mut stack: Vec<Node> = vec![];

        'end_of_tree: for y in 0..area.height {
            let node = cursor.node();
            let opt_kin: Option<Kin> = self.kin_index.get(&node.kind_id()).copied();

            let x = stack.len() * 2;
            if x < area_width {
                let node_desc = self.describe_node(node, opt_kin);
                let style = self.style(opt_kin);
                let (_x, _y) = buf.set_stringn(
                    x as u16 + area.x,
                    y + area.y,
                    node_desc,
                    area_width - x,
                    style,
                );
            }

            // don't recurse into strings
            let mut recurse = true;
            if opt_kin == Some(Kin::String) {
                recurse = false;
            }

            if recurse && cursor.goto_first_child() {
                // climb the tree
                stack.push(node);
            } else {
                'walk_to_next_node: loop {
                    if cursor.goto_next_sibling() {
                        break 'walk_to_next_node;
                    } else if stack.is_empty() {
                        break 'end_of_tree;
                    } else {
                        // crawl back down
                        let parent = stack.pop().unwrap();
                        cursor.goto_parent();
                        assert_eq!(parent, cursor.node());
                    }
                }
            }
        }
    }
}

impl<'a> TreeWidget<'a> {
    fn describe_node(&self, node: Node, kin: Option<Kin>) -> String {
        if node.is_named() {
            let tree_sitter::Range {
                start_byte,
                end_byte,
                ..
            } = node.range();
            let name = &self.source_code[start_byte..end_byte];
            if node.is_error() {
                return format!("ERROR {} {:?}", node.kind(), name);
            }
            match kin {
                Some(_) => format!("{}", name),
                None => format!("[{}]", node.kind()),
            }
        } else if node.is_error() {
            format!("ERROR {} #{}", node.kind(), node.id() % 1000)
        } else {
            // plain token or delimiter
            format!("{}", node.kind())
        }
    }

    fn style(&self, kin: Option<Kin>) -> Style {
        let style = Style::default();
        match kin {
            Some(Kin::Delimiter) => style.fg(Color::DarkGray),
            Some(Kin::Brace) => style.fg(Color::DarkGray),
            Some(Kin::Comment) => style.fg(Color::Yellow),
            Some(Kin::Identifier) | Some(Kin::PropertyIdentifier) => style.fg(Color::Cyan),
            Some(Kin::String) => style.fg(Color::Green),
            None => style,
        }
    }
}
