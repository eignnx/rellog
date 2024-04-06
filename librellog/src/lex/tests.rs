use insta::{assert_debug_snapshot, glob};
use std::fs;

use crate::{init_interner, utils::my_nom::Span};

#[test]
fn test_tokenize() {
    init_interner();
    glob!("snapshot_tests/*.rellog", |path| {
        let input = fs::read_to_string(path).unwrap();
        let span = Span::new(&input);
        let output = super::lex::tokenize(span, path.into());
        assert_debug_snapshot!(output);
    });
}
