//! Datatypes for text editing.

#[derive(Debug, Clone)]
pub enum EditCommand {
    SetMode(ModeSelect),
    Append(char),
    Move(ArrowKey),
    Newline,
    Backspace,
    Delete,
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
