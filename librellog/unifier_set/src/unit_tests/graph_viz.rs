use super::{Node, Root, UnifierSet};
use dot_graph as dg;
use std::{
    collections::hash_map::DefaultHasher,
    fmt::Display,
    fs,
    hash::Hash,
    hash::Hasher,
    io::{self, Write},
    path::Path,
};

impl<Var, Term> UnifierSet<Var, Term>
where
    Var: Eq + Hash + Display,
    Term: Display + Hash,
{
    fn to_dot_graph(&self) -> dg::Graph {
        let mut g = dg::Graph::new("unifier_set", dg::Kind::Digraph);

        for (var, referenced) in self.map.borrow().iter() {
            let var_id = var.to_string();
            let (n, e) = referenced.to_dg_node_and_edge(&var_id);
            g.add_node(dg::Node::new(&var_id).shape(Some("circle")));
            g.add_node(n);
            g.add_edge(e);
        }

        g
    }

    pub fn print_to_dot_file(&self, output_file_path: impl AsRef<Path>) -> io::Result<()> {
        let mut f = fs::File::create(output_file_path)?;
        let g = self.to_dot_graph();
        let dot_string = g.to_dot_string().expect("Invalid graph specified!");
        write!(f, "{dot_string}")
    }
}

impl<Var, Term> Node<Var, Term>
where
    Var: Display,
    Term: Display + Hash,
{
    fn to_dg_node_and_edge(&self, from_node_id: &str) -> (dg::Node, dg::Edge) {
        match self {
            Node::Child(parent) => {
                let r = parent.to_string();
                let n = dg::Node::new(&r);
                let e = dg::Edge::new(&from_node_id, &r, "");
                (n, e)
            }
            Node::Root(Root::Var(size)) => {
                let r = format!("var_root_of_{from_node_id}");
                let n = dg::Node::new(&r)
                    .shape(Some("record"))
                    .style(dg::Style::Bold)
                    .label(&format!("{size}"));
                let e = dg::Edge::new(&from_node_id, &r, "");
                (n, e)
            }
            Node::Root(Root::NonVar(term, size)) => {
                let r = {
                    let mut r = term.to_string();
                    r.retain(|c| c.is_alphanumeric() || ['.', '_'].contains(&c));
                    let hash = {
                        let mut hasher = DefaultHasher::new();
                        term.hash(&mut hasher);
                        hasher.finish()
                    };
                    format!("non_var_root.{r}.{hash:x}")
                };

                let n = dg::Node::new(&r)
                    .shape(Some("record"))
                    .style(dg::Style::Bold)
                    .label(&format!("{term} | {size}"));

                let e = dg::Edge::new(&from_node_id, &r, "");

                (n, e)
            }
        }
    }
}
