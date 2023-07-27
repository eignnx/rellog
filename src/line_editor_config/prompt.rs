use std::borrow::Cow;

use reedline::{DefaultPrompt, Prompt, PromptEditMode, PromptHistorySearch, PromptViMode};

use super::{RellogReplConfigHandle, ReplMode};

impl Prompt for RellogReplConfigHandle {
    fn render_prompt_left(&self) -> Cow<str> {
        "".into()
    }

    fn render_prompt_right(&self) -> Cow<str> {
        let cfg = self.read().unwrap();

        match &cfg.repl_mode {
            ReplMode::TopLevel => match &cfg.prompt_edit_mode {
                PromptEditMode::Default => "".into(),
                PromptEditMode::Emacs => "(emacs)".into(),
                PromptEditMode::Vi(PromptViMode::Insert) => "(vi:insert)".into(),
                PromptEditMode::Vi(PromptViMode::Normal) => "(vi:normal)".into(),
                PromptEditMode::Custom(c) => format!("({c})").into(),
            },
            ReplMode::PrintingSolns => "[ENTER to show next solution, CTRL-C to break]".into(),
        }
    }

    fn render_prompt_indicator(&self, edit_mode: PromptEditMode) -> Cow<str> {
        let mut cfg = self.write().unwrap();
        cfg.prompt_edit_mode = edit_mode;
        match &cfg.repl_mode {
            ReplMode::TopLevel => "-- ".into(),
            ReplMode::PrintingSolns => "".into(),
        }
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        ".. ".into()
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: PromptHistorySearch,
    ) -> Cow<str> {
        DefaultPrompt.render_prompt_history_search_indicator(history_search)
    }
}
