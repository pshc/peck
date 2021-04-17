//! Drives tree_sitter in its own thread.

use crossbeam_channel::{unbounded, Receiver, Sender};
use log::info;
use std::{convert::TryInto, error::Error, collections::HashMap, thread};

use tree_sitter::{Language, Parser, Tree};

extern "C" {
    /** Parses Javascript. Generated C code. */
    fn tree_sitter_javascript() -> Language;
}

#[derive(Debug)]
pub enum ParseEvent {
    LanguageIndexed(KinIndex),
    TreeParsed(ParseTree),
}

/// The result of one parsing session.
#[derive(Debug)]
pub struct ParseTree {
    pub snapshot: Snapshot,
    pub tree: Tree,
}

/// A copy of the editor rope at a given time
#[derive(Debug)]
pub struct Snapshot {
    pub text: String,
}

pub fn setup_parser() -> (Sender<Snapshot>, Receiver<ParseEvent>) {
    let (snap_tx, snap_rx) = unbounded();
    let (event_tx, event_rx) = unbounded();

    thread::spawn(move || parse_loop(snap_rx, event_tx).unwrap());

    (snap_tx, event_rx)
}

fn parse_loop(snap_rx: Receiver<Snapshot>, event_tx: Sender<ParseEvent>) -> Result<(), Box<dyn Error>> {
    // load the Javascript parser
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_javascript() };
    parser.set_language(language).map_err(|e| e.to_string())?;
    info!("parser.set_language(JS) OK");
    let kin_index = index_kins(language)?;

    // notify the editor thread that we're ready to go
    event_tx.send(ParseEvent::LanguageIndexed(kin_index))?;

    loop {
        let snapshot = snap_rx.recv()?;
        // TODO: use the `old_tree` to speed this up
        let tree = parser
            .parse(&snapshot.text, None)
            .expect("no tree parsed at all");
        event_tx.send(ParseEvent::TreeParsed(ParseTree { snapshot, tree }))?;
    }
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

pub type KinIndex = HashMap<u16, Kin>;

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
pub fn index_kins(lang: Language) -> Result<KinIndex, Box<dyn Error>> {
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
