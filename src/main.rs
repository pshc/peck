use crossbeam_channel::{select, Receiver};
use crossterm::event::{Event, KeyCode, KeyEvent};
use log::{debug, info};
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

use tree_sitter::{Language, Node, Parser};

mod setup;

extern "C" {
    /** Parses Javascript. Generated C code. */
    fn tree_sitter_javascript() -> Language;
}

fn main() -> Result<(), Box<dyn Error>> {
    better_panic::install();

    let backend = CrosstermBackend::new(io::stdout());
    let terminal = Terminal::new(backend)?;

    setup::setup_panic_hook();
    setup::setup_terminal()?;

    // load the Javascript parser
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_javascript() };
    parser.set_language(language).map_err(|e| e.to_string())?;
    info!("parser.set_language(JS) OK");

    let rope = Rope::from_str(
        r#"/* 🙃 */
function BOO_YA_KA_SHA() {
    console.log('now we done it');
    return 2 + 3;
}
"#,
    );

    let kin_index = index_kins(language)?;
    let ui_events = setup::setup_ui_events();

    let state = EditState::default();
    let mut editor = Editor {
        rope,
        parser,
        terminal,
        kin_index,
        state,
    };
    let result = editor.main_loop(ui_events);
    setup::cleanup_terminal()?;
    result
}

pub struct Editor<T: Backend> {
    rope: Rope,
    parser: Parser,
    terminal: Terminal<T>,
    kin_index: HashMap<u16, Kin>,
    state: EditState,
}

impl<T: Backend> Editor<T> {
    pub fn main_loop(&mut self, ui_events: Receiver<Event>) -> Result<(), Box<dyn Error>> {
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

            // first, let's just print the text buffer, no tree
            render_layout(&self.rope, &mut self.terminal, &self.state, None)?;

            // for now, convert the WHOLE rope into a heap-allocated String.
            let source_snapshot: String = self.rope.clone().into();
            // parse that string
            // TODO: use the `old_tree` to speed this up
            let tree = self
                .parser
                .parse(&source_snapshot, None)
                .expect("no tree parsed at all");
            debug!("parser.parse OK");

            let root_node = tree.root_node();
            assert_eq!(root_node.kind(), "program");
            assert_eq!(root_node.start_position().column, 0);

            // now print the tree
            let tree_widget = TreeWidget {
                root: root_node,
                source_code: &source_snapshot,
                kin_index: &self.kin_index,
            };
            self.state.text_viewport = render_layout(
                &self.rope,
                &mut self.terminal,
                &self.state,
                Some(tree_widget),
            )?;

            if !self.handle_events(&ui_events)? {
                return Ok(());
            }
        }
    }

    pub fn handle_events(&mut self, ui_events: &Receiver<Event>) -> Result<bool, Box<dyn Error>> {
        // first, block on input
        select! {
            recv(ui_events) -> message => {
                match message? {
                    Event::Key(key_event) => {
                        if !self.handle_key_event(key_event)? {
                            return Ok(false);
                        }
                    }
                    Event::Mouse(_) => (),
                    Event::Resize(_w, _h) => (),
                }
            }
        }

        // now if any other events have been enqueued, process them too.

        // careful. if our update operations are slow and they type quickly,
        // we could end up spending a lot of time in here without repainting...
        loop {
            select! {
                recv(ui_events) -> message => {
                    match message? {
                        Event::Key(key_event) => {
                            if !self.handle_key_event(key_event)? {
                                return Ok(false);
                            }
                        }
                        Event::Mouse(_) => (),
                        Event::Resize(_w, _h) => (),
                    }
                }
                default => return Ok(true),
            }
        }
    }

    pub fn handle_key_event(&mut self, event: KeyEvent) -> Result<bool, Box<dyn Error>> {
        // first, convert the input into an EditCommand
        let command = match self.interpret_key_event(event) {
            Some(c) => c,
            None => return Ok(true),
        };

        if !event.modifiers.is_empty() {
            return Ok(true);
        }

        let EditState {
            ref mut cursor,
            ref mut scroll,
            insert_mode,
            text_viewport,
        } = self.state;
        let right_edge = if insert_mode { 1 } else { 2 };

        // Q: exactly which screen size do we use below?
        // A: we should use `viewport` corresponding to whatever is still painted on the screen.
        // since we're in the middle of an event processing loop, we might run into a resize event
        // but the event should not apply until we repaint.
        let text_size = (text_viewport.width as isize, text_viewport.height as isize);
        assert!(text_size.0 > 0 && text_size.1 > 0);

        let total_line_count = self.rope.len_lines().try_into()?;
        let at_eof = cursor.1 >= total_line_count;

        match command {
            EditCommand::Move(ArrowKey::Left) => {
                if cursor.0 > 0 {
                    cursor.0 -= 1;
                } else if cursor.1 > 0 {
                    let prev_line_idx = (cursor.1 - 1).try_into()?;
                    if let Some(prev_line) = self.rope.lines_at(prev_line_idx).next() {
                        // move to end of previous line
                        cursor.1 -= 1;
                        let prev_length: isize = prev_line.len_chars().try_into()?;
                        cursor.0 = 0.max(prev_length - right_edge);
                    }
                }
            }
            EditCommand::Move(ArrowKey::Right) => {
                let line_idx = cursor.1.try_into()?;
                if let Some(line) = self.rope.lines_at(line_idx).next() {
                    // is the cursor at the end of the line?
                    let eol = cursor.0 + right_edge >= line.len_chars().try_into()?;
                    if eol {
                        if !at_eof {
                            // move to the next line
                            cursor.0 = 0;
                            cursor.1 += 1;
                        }
                    } else {
                        cursor.0 += 1;
                    }
                }
            }
            EditCommand::Move(ArrowKey::Up) => {
                if cursor.1 > 0 {
                    cursor.1 -= 1;
                }
            }
            EditCommand::Move(ArrowKey::Down) => {
                if !at_eof {
                    cursor.1 += 1;
                }
            }
            EditCommand::Quit => return Ok(false),
            EditCommand::Newline => unimplemented!(),
            EditCommand::Backspace => unimplemented!(),
            EditCommand::Append(char) => {
                assert!(self.state.insert_mode, "trying to insert in normal mode?");

                println!("APPEND: {}", char); // TODO insert into rope
            }
            EditCommand::SetMode(mode) => {
                let insert_mode = mode != ModeSelect::Normal;
                let mode_changed = self.state.insert_mode != insert_mode;
                self.state.insert_mode = insert_mode;

                assert!(mode_changed, "SetMode: mode was already {:?}", insert_mode);

                // press 'a' to insert after the cursor
                // press 'i' to insert before the cursor
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
                        }
                    }
                    ModeSelect::InsertBeforeCursor => (),
                    ModeSelect::Normal => {
                        // move cursor one to the left
                        if cursor.0 > 0 {
                            cursor.0 -= 1;
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

        Ok(true)
    }

    pub fn interpret_key_event(&self, event: KeyEvent) -> Option<EditCommand> {
        if !event.modifiers.is_empty() {
            return None;
        }

        // universal movement commands
        use EditCommand::Move;
        match event.code {
            KeyCode::Left => return Some(Move(ArrowKey::Left)),
            KeyCode::Right => return Some(Move(ArrowKey::Right)),
            KeyCode::Up => return Some(Move(ArrowKey::Up)),
            KeyCode::Down => return Some(Move(ArrowKey::Down)),
            _ => (),
        }

        use KeyCode::Char;

        if self.state.insert_mode {
            // most key codes will append a char here
            match event.code {
                Char(c) => Some(EditCommand::Append(c)),
                KeyCode::Enter => Some(EditCommand::Newline),
                KeyCode::Esc => Some(EditCommand::SetMode(ModeSelect::Normal)),
                _ => None,
            }
        } else {
            match event.code {
                Char('a') => Some(EditCommand::SetMode(ModeSelect::InsertAfterCursor)),
                Char('i') => Some(EditCommand::SetMode(ModeSelect::InsertBeforeCursor)),
                Char('q') => Some(EditCommand::Quit),
                _ => None,
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct EditState {
    pub cursor: (isize, isize),
    pub scroll: (isize, isize),
    pub insert_mode: bool,
    /// This is not free state really. Rather it's `frame.size` copied from the most recent `render_layout`.
    text_viewport: Rect,
}

#[derive(Debug, Clone)]
pub enum EditCommand {
    SetMode(ModeSelect),
    Append(char),
    Move(ArrowKey),
    Newline,
    Backspace,
    Quit,
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ModeSelect {
    /// turns on insert_mode
    InsertBeforeCursor,
    /// also turns on insert_mode
    InsertAfterCursor,
    /// turns off insert mode
    Normal,
}

#[derive(Debug, Clone)]
pub enum ArrowKey {
    Up,
    Down,
    Left,
    Right,
}

/// Returns the viewport [Rect] of the upper text window.
pub fn render_layout<T: Backend>(
    rope: &Rope,
    terminal: &mut Terminal<T>,
    state: &EditState,
    tree_widget: Option<TreeWidget>,
) -> Result<Rect, Box<dyn Error>> {
    let mut text_viewport = None;
    let EditState { cursor, scroll, .. } = *state;

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
            match state.insert_mode {
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

/// The kinds we are interested in.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Kin {
    Identifier,
    PropertyIdentifier,
    Comment,
    String,
    Brace,
    Delimiter,
}

impl Kin {
    pub fn from_str(str: &'static str) -> Option<Self> {
        Some(match str {
            "identifier" => Kin::Identifier,
            "property_identifier" => Kin::PropertyIdentifier,
            "comment" => Kin::Comment,
            "string" => Kin::String,
            "(" | ")" | "{" | "}" => Kin::Brace,
            "." | ";" => Kin::Delimiter,
            _ => return None,
        })
    }
}

/// Scans all the 16-bit node kind IDs from `lang` and maps them to [Kin]s.
pub fn index_kins(lang: Language) -> Result<HashMap<u16, Kin>, Box<dyn Error>> {
    let mut hash = HashMap::new();
    // linear scan of all the node kinds
    // ideally, we scan these at build time...
    let count: u16 = lang.node_kind_count().try_into()?;
    for id in 0..count {
        if let Some(str) = lang.node_kind_for_id(id) {
            if let Some(kin) = Kin::from_str(str) {
                let overwritten = hash.insert(id, kin);
                if overwritten.is_some() {
                    return Err(format!("{:?} has two IDs", kin).into());
                }
            }
        }
    }
    Ok(hash)
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
    /// Generated by [index_kins].
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
