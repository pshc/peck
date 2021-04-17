//! Datatypes for text editing.

#[derive(Debug, Clone)]
pub enum EditCommand {
    SetMode(ModeSelect),
    Append(char),
    Move(Go),
    Backspace,
    Delete,
    Quit,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ModeSelect {
    /// turns on insert_mode
    InsertBeforeCursor,
    InsertBeforeLine,
    InsertAfterCursor,
    InsertAfterLine,
    /// turns off insert mode
    Normal,
}

#[derive(Debug, Clone)]
pub enum Go {
    Up,
    Down,
    Left,
    Right,
    BOL,
    EOL,
    NextLine,
}
