use nu_ansi_term::{Color, Style};
use reedline::{Highlighter, StyledText};

use super::RellogReplConfigHandle;

impl Highlighter for RellogReplConfigHandle {
    fn highlight(&self, mut input: &str, _cursor: usize) -> reedline::StyledText {
        let mut out = StyledText::new();
        let mut stack: Vec<(usize, char)> = vec![];
        let mut inside_str = false;

        while let Some(idx) = input.find(['[', ']', '{', '}', '(', ')', '"', '"']) {
            let text_style = if inside_str {
                str_style()
            } else {
                Style::default()
            };

            out.push((text_style, input[..idx].to_owned()));

            let ch = input.chars().nth(idx).unwrap();
            let style = match (ch, inside_str) {
                ('[' | '{' | '(', false) => {
                    stack.push((out.buffer.len(), ch));
                    rainbow_bracket(stack.len())
                }
                ('"', false) => {
                    inside_str = true;
                    str_style()
                }
                ('"', true) => {
                    inside_str = false;
                    str_style()
                }
                (']' | '}' | ')', false) => match stack.pop() {
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
                _ if inside_str => str_style(),
                _ => unreachable!(),
            };

            out.push((style, ch.to_string()));

            input = &input[idx + 1..];
        }

        for (open_idx, open) in stack {
            out.buffer[open_idx] = (error_style(), open.to_string());
        }

        let text_style = if inside_str {
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
