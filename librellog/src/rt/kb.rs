//! Defines a rellog Knowledge Base ('kb').

use std::{collections::BTreeMap, io::Read, path::Path};

use crate::{
    ast::{self, Clause, Item, RcTm, Rel, Sig},
    lex, parse,
};

use super::{err, UnifierSet};

#[derive(Debug, Default)]
pub struct KnowledgeBase {
    pub directives: Vec<Directive>,
    pub relations: BTreeMap<Sig, Vec<Clause>>,
}

#[derive(Debug, Clone)]
pub struct Directive {
    pub query: Rel,
    /// The next `Clause` appearing in the source file after this directive.
    pub next_rel_def: Option<Clause>,
}

impl KnowledgeBase {
    pub fn include(&mut self, path: impl AsRef<Path>) -> Result<(), err::Err> {
        let src = {
            let mut f = std::fs::File::open(path.as_ref()).map_err(|e| {
                err::Err::IoError(format!(
                    "Could not open file `{}`.\n(io error: {:?})",
                    path.as_ref().display(),
                    e.kind()
                ))
            })?;
            let mut src = String::new();
            f.read_to_string(&mut src).map_err(|e| {
                err::Err::IoError(format!(
                    "Could not read file `{}`.\n(io error: {:?})",
                    path.as_ref().display(),
                    e.kind()
                ))
            })?;
            src
        };

        let mut tok_buf = Vec::new();
        let tokens = lex::tokenize_into(&mut tok_buf, src.as_ref(), path.as_ref().into())?;
        let m = parse::entire_module(tokens, Some(path))?;

        self.import(m);

        Ok(())
    }

    pub fn import(&mut self, other: impl Into<KnowledgeBase>) {
        // TODO: detect name clashes from other modules.
        let mut other = other.into();
        self.directives.append(&mut other.directives);
        for (sig, clauses) in other.relations {
            self.relations.entry(sig).or_default().extend(clauses);
        }
    }

    /// Returns an `ExactSizeIterator` of all the clauses whose heads match the given query.
    /// If the relation does not exist, `None` is returned.
    pub fn index_match<'m>(
        &'m self,
        query_head: &Rel,
        u: &UnifierSet,
        debug: bool,
    ) -> Option<impl ExactSizeIterator<Item = &'m Clause> + 'm> {
        let sig = query_head.keys().cloned().collect();
        let sig_based_index = self.relations.get(&sig)?;

        if debug {
            println!(
                "\nSIG_BASED_INDEX: {} ~ {}",
                RcTm::from(query_head.clone()),
                RcTm::list_from_iter(
                    sig_based_index
                        .iter()
                        .cloned()
                        .map(|clause| RcTm::from(clause.head))
                )
            );
        }

        let arg_indexed = sig_based_index
            .iter()
            .filter(|clause| {
                let clause_head = clause.head.clone().into();
                let query_head = query_head.clone().into();
                u.unify(&clause_head, &query_head).is_some()
            })
            .collect::<Vec<_>>();

        if debug {
            println!(
                "ARG_INDEXED: {}",
                RcTm::list_from_iter(arg_indexed.iter().map(|clause| clause.head.clone().into()))
            );
        }

        Some(arg_indexed.into_iter())
    }

    pub fn clear(&mut self) {
        self.directives.clear();
        self.relations.clear();
    }
}

impl From<ast::Module> for KnowledgeBase {
    fn from(m: ast::Module) -> Self {
        let mut directives = Vec::new();
        let mut relations: BTreeMap<Sig, Vec<Clause>> = BTreeMap::new();

        for (i, item) in m.items.iter().enumerate() {
            match item {
                Item::Directive(query) => {
                    let next_rel_def = m.items.iter().skip(i).find_map(|item| match item {
                        Item::RelDef(head, body) => Some(Clause {
                            head: head.clone(),
                            body: body.clone(),
                        }),
                        _ => None,
                    });

                    directives.push(Directive {
                        query: query.clone(),
                        next_rel_def,
                    });
                }
                Item::RelDef(head, body) => relations
                    .entry(head.keys().cloned().collect())
                    .or_default()
                    .push(Clause {
                        head: head.clone(),
                        body: body.clone(),
                    }),
            }
        }

        Self {
            directives,
            relations,
        }
    }
}
