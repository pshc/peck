//! Copy pasted from ytop/main.rs

use crossbeam_channel::{unbounded, Receiver};
use crossterm::{cursor, event::Event, execute, terminal};
use std::{
    error::Error,
    io,
    panic, thread,
};

pub fn setup_ui_events() -> Receiver<Event> {
    let (sender, receiver) = unbounded();
    thread::spawn(move || loop {
        sender.send(crossterm::event::read().unwrap()).unwrap();
    });

    receiver
}

pub fn setup_panic_hook() {
    panic::set_hook(Box::new(|panic_info| {
        cleanup_terminal().unwrap();
        better_panic::Settings::auto().create_panic_handler()(panic_info);
    }));
}

pub fn setup_terminal() -> Result<(), Box<dyn Error>> {
    let mut stdout = io::stdout();

    execute!(stdout, terminal::EnterAlternateScreen)?;
    execute!(stdout, cursor::Show)?;
    execute!(stdout, terminal::Clear(terminal::ClearType::All))?;

    terminal::enable_raw_mode()?;
    Ok(())
}

pub fn cleanup_terminal() -> Result<(), Box<dyn Error>> {
    let mut stdout = io::stdout();

    execute!(stdout, cursor::MoveTo(0, 0))?;
    execute!(stdout, terminal::Clear(terminal::ClearType::All))?;

    execute!(stdout, terminal::LeaveAlternateScreen)?;
    execute!(stdout, cursor::Show)?;

    terminal::disable_raw_mode()?;
    Ok(())
}
