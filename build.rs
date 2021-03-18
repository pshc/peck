use std::path::PathBuf;

fn main() {
    let dir: PathBuf = ["tree-sitter-javascript", "src"].iter().collect();

    cc::Build::new()
        .include(&dir)
        .file(dir.join("parser.c"))
        .file(dir.join("scanner.c"))
        .warnings(false)
        .extra_warnings(false)
        .compile("tree-sitter-javascript");
}
