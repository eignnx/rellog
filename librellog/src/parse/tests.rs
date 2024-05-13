use super::*;
use crate::{lex::tokenize, utils::my_nom::Span};
use insta::{assert_debug_snapshot, glob};

#[track_caller]
fn parse_to_tm(src: &str) -> Result<Tm, crate::rt::Err> {
    let tokens = tokenize(Span::from(src), "blah.rellog".into())?;
    let (rest, t) = tm.parse(tokens[..].into()).finish()?;

    if !rest.is_empty() {
        Err(crate::rt::Err::ParseError(format!(
            "\nCould not parse entire input: ```\n{src}\n```\n\
                 Remaining input begins with: [{}]\n",
            rest.iter()
                .take(5)
                .map(|tok| format!("`{}`", tok.as_ref()))
                .collect::<Vec<_>>()
                .join(", "),
        )))
    } else {
        Ok(t)
    }
}

#[test]
fn simple_nested_relation_tm() -> Result<(), crate::rt::Err> {
    crate::init_interner();
    let src = r#"[goal [List][Pred]][initial Sublist][final empty_list]"#;
    assert_debug_snapshot!(parse_to_tm(src));
    Ok(())
}

#[test]
fn simple_block() -> Result<(), crate::rt::Err> {
    crate::init_interner();
    let src = "
    - [Blah]
    - [Blah]
    - [Blah]\n";
    assert_debug_snapshot!(parse_to_tm(src));
    Ok(())
}

#[test]
fn nested_block() -> Result<(), crate::rt::Err> {
    crate::init_interner();
    let src = "
    - [Blah]
    -
        | [Blah]
        | [Blah]
    - [Blah]\n";
    assert_debug_snapshot!(parse_to_tm(src));
    Ok(())
}

#[test]
fn empty_list() {
    crate::init_interner();
    assert_debug_snapshot!(parse_to_tm("{}"));
}

#[test]
fn singleton_list() {
    crate::init_interner();
    assert_debug_snapshot!(parse_to_tm("{123}"));
}

#[test]
fn multi_element_list() {
    crate::init_interner();
    let src = r#"{123 "asdf" {} [aardvark][Zebra] socrates Unknown {1 2 3}}"#;
    assert_debug_snapshot!(parse_to_tm(src));
}

#[test]
fn list_with_spread() {
    crate::init_interner();
    let src = r#"{X ..Xs}"#;
    assert_debug_snapshot!(parse_to_tm(src));
}

#[test]
fn txt_tmpl_with_interpolation() {
    crate::init_interner();
    let src = r#"  "asdf[{Letter}]qwer"  "#;
    assert_debug_snapshot!(parse_to_tm(src));
}

#[test]
fn snapshot_tests() {
    crate::init_interner();
    glob!("snapshot_tests/*.rellog", |path| {
        let src = std::fs::read_to_string(path).unwrap();
        assert_debug_snapshot!(parse_to_tm(&src));
    });
}
