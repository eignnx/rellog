use nu_ansi_term::{Color, Style};
use reedline::{Highlighter, StyledText};

use super::RellogReplConfigHandle;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexCtx {
    Expr,
    Quoted,
    TxtInterp,
    MaybeTxtInterp,
    MaybeQuoted,
}

impl Highlighter for RellogReplConfigHandle {
    fn highlight(&self, mut input: &str, _cursor: usize) -> reedline::StyledText {
        let mut out = StyledText::new();
        let mut stack: Vec<(usize, char)> = vec![];
        let mut ctx = LexCtx::Expr;

        while let Some(idx) = input.find(['[', ']', '{', '}', '(', ')', '"', '"']) {
            let text_style = if ctx == LexCtx::Quoted {
                str_style()
            } else {
                Style::default()
            };

            out.push((text_style, input[..idx].to_owned()));

            let ch = input.chars().nth(idx).unwrap();
            let style = match (ch, ctx) {
                ('[' | '{' | '(', LexCtx::Expr | LexCtx::TxtInterp) => {
                    stack.push((out.buffer.len(), ch));
                    rainbow_bracket(stack.len())
                }
                ('[', LexCtx::Quoted) => {
                    ctx = LexCtx::MaybeTxtInterp;
                    stack.push((out.buffer.len(), ch));
                    rainbow_bracket(stack.len())
                }
                ('{', LexCtx::MaybeTxtInterp) => {
                    ctx = LexCtx::TxtInterp;
                    stack.push((out.buffer.len(), ch));
                    rainbow_bracket(stack.len())
                }
                (_, LexCtx::MaybeTxtInterp) => {
                    ctx = LexCtx::Quoted;
                    str_style()
                }
                ('"', LexCtx::Expr | LexCtx::TxtInterp) => {
                    ctx = LexCtx::Quoted;
                    str_style()
                }
                ('"', LexCtx::Quoted) => {
                    ctx = LexCtx::Expr;
                    str_style() // Style the last quote.
                }
                ('}', LexCtx::TxtInterp) => match stack.pop() {
                    Some((open_idx, open)) => {
                        if matching_brackets(open, ch) {
                            ctx = LexCtx::MaybeQuoted;
                            rainbow_bracket(stack.len() + 1)
                        } else {
                            out.buffer[open_idx] = (error_style(), open.to_string());
                            error_style()
                        }
                    }
                    _ => error_style(),
                },
                (']', LexCtx::MaybeQuoted) => match stack.pop() {
                    Some((open_idx, open)) => {
                        if matching_brackets(open, ch) {
                            ctx = LexCtx::Quoted;
                            rainbow_bracket(stack.len() + 1)
                        } else {
                            out.buffer[open_idx] = (error_style(), open.to_string());
                            error_style()
                        }
                    }
                    _ => error_style(),
                },
                (_, LexCtx::MaybeQuoted) => {
                    ctx = LexCtx::Expr;
                    error_style()
                }
                (']' | '}' | ')', LexCtx::Expr) => match stack.pop() {
                    Some((open_idx, open)) => {
                        if matching_brackets(open, ch) {
                            rainbow_bracket(stack.len() + 1)
                        } else {
                            out.buffer[open_idx] = (error_style(), open.to_string());
                            error_style()
                        }
                    }
                    _ => error_style(),
                },
                (_, LexCtx::Quoted) => str_style(),
                (']', LexCtx::TxtInterp) => {
                    ctx = LexCtx::Quoted;
                    error_style()
                }
                other => unreachable!("{:?}", other),
            };

            out.push((style, ch.to_string()));

            input = &input[idx + 1..];
        }

        for (open_idx, open) in stack {
            out.buffer[open_idx] = (error_style(), open.to_string());
        }

        let text_style = if ctx == LexCtx::Quoted {
            str_style()
        } else {
            Style::default()
        };

        out.push((text_style, input.to_owned()));

        out
    }
}

fn matching_brackets(open: char, close: char) -> bool {
    matches!((open, close), ('(', ')') | ('[', ']') | ('{', '}'))
}

fn error_style() -> Style {
    Style::default().bold().fg(Color::Red)
}

fn str_style() -> Style {
    Style::default().bold().fg(Color::Cyan)
}

fn rainbow_bracket(depth: usize) -> Style {
    match depth % 3 {
        0 => Style::default().bold().fg(Color::LightBlue),
        1 => Style::default().bold().fg(Color::Yellow),
        2 => Style::default().bold().fg(Color::Magenta),
        _ => unreachable!(),
    }
}
