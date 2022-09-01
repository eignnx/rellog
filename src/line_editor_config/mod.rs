use reedline::{FileBackedHistory, Reedline};

mod highlighter;
mod prompt;
mod validator;

pub static PROMPT: prompt::RellogPrompt = prompt::RellogPrompt::new();

pub fn default_line_editor() -> reedline::Reedline {
    Reedline::create()
        .with_validator(Box::new(validator::RellogReplValidator))
        .with_history(Box::new(FileBackedHistory::new(500)))
        .with_highlighter(Box::new(highlighter::RellogHighlighter::new()))
}
