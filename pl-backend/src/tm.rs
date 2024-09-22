use std::io;

use librellog::{
    ast::{BinOpSymbol, RcTm, Sig, Tm},
    data_structures::{Sym, Var},
};

use crate::{ArgOrder, Compile, RelId, RelInfo, SwiProlog};

impl Compile<SwiProlog> for RcTm {
    fn compile(&self, f: &mut dyn io::Write, compiler: &mut SwiProlog) -> io::Result<()> {
        match &**self {
            Tm::Nil => write!(f, "[]")?,
            Tm::Cons(..) => {
                let old = compiler.tm_is_callable;
                compiler.tm_is_callable = false;
                compiler.compile_list(f, self)?;
                compiler.tm_is_callable = old;
            }
            Tm::Rel(rel) => {
                let old = compiler.tm_is_callable;
                let sig = Sig::from(rel.keys().cloned());
                let rel_id = RelId::from_sig(&sig);
                let default_rel_info = RelInfo {
                    sig: sig.clone(),
                    pred_arg_order: ArgOrder::RellogOrder,
                };

                let rel_info = match compiler.rel_map.get(&rel_id) {
                    Some(rel_info) => rel_info,
                    None => {
                        if compiler.tm_is_callable {
                            eprintln!("WARNING: RelInfo not found for {}", sig);
                        }
                        // Assume it's a builtin; provide default value.
                        &default_rel_info
                    }
                };

                let arg_order = rel_info.pred_arg_order.clone();

                compiler.tm_is_callable = false; // subterms are generally not callable
                match arg_order {
                    ArgOrder::RellogOrder => {
                        write!(f, "{}(", rel_info.pred_name())?;
                        for (i, (_key, val)) in rel.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            val.compile(f, compiler)?;
                        }
                        write!(f, ")")?;
                    }
                    ArgOrder::Translated {
                        pred_name,
                        arg_order,
                    } => {
                        write!(f, "{pred_name}(")?;
                        for (i, key) in arg_order.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            let val = rel.get(key).unwrap();
                            val.compile(f, compiler)?;
                        }
                        write!(f, ")")?;
                    }
                }
                compiler.tm_is_callable = old;
            }
            Tm::Sym(sym) => sym.compile(f, compiler)?,
            Tm::Block(functor, members) => {
                writeln!(f, "'$block'('{functor}', [")?;
                for (i, member) in members.iter().enumerate() {
                    if i > 0 {
                        writeln!(f, ",")?;
                    }
                    write!(f, "\t\t")?;
                    member.compile(f, compiler)?;
                }
                write!(f, "\n\t])")?;
            }
            Tm::Var(var) => var.compile(f, compiler)?,
            Tm::Int(i) => write!(f, "{i}")?,
            Tm::TxtSeg(..) | Tm::TxtCons(..) => {
                let old = compiler.tm_is_callable;
                compiler.tm_is_callable = false;
                compiler.compile_txt(f, self)?;
                compiler.tm_is_callable = old;
            }

            Tm::BinOp(bsym, lhs, rhs) => match bsym {
                BinOpSymbol::Equal => {
                    write!(f, "(")?;
                    lhs.compile(f, compiler)?;
                    write!(f, " = ")?;
                    rhs.compile(f, compiler)?;
                    write!(f, ")")?;
                }
                BinOpSymbol::PathSep => {
                    lhs.compile(f, compiler)?;
                    write!(f, ":")?; // Trust that user wrote term having thought about precedence.
                    rhs.compile(f, compiler)?;
                }
                BinOpSymbol::Tilde => {
                    write!(f, "'~'(")?;
                    lhs.compile(f, compiler)?;
                    write!(f, ", ")?;
                    rhs.compile(f, compiler)?;
                    write!(f, ")")?;
                }
                BinOpSymbol::Semicolon => {
                    write!(f, "(")?;
                    lhs.compile(f, compiler)?;
                    write!(f, ", ")?; // Rellog's `;` is Prolog's `,`.
                    rhs.compile(f, compiler)?;
                    write!(f, ")")?;
                }
            },
        }
        Ok(())
    }
}

impl SwiProlog {
    fn compile_txt(&mut self, f: &mut dyn io::Write, txt: &RcTm) -> io::Result<()> {
        let mut buf = String::new();
        if txt.try_collect_txt_to_string(&mut buf).is_ok() {
            write!(f, "\"{buf}\"")?;
            return Ok(());
        }

        let mut tail = txt;
        let mut first = true;
        write!(f, "[")?;
        loop {
            match tail.as_ref() {
                Tm::Nil => break,
                Tm::TxtCons(ch, rest) => {
                    if !first {
                        write!(f, ", ")?;
                    }
                    ch.compile(f, self)?;
                    tail = rest;
                }
                Tm::TxtSeg(seg) => {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = true;
                    for ch in seg.segment_as_str().chars() {
                        if !first {
                            write!(f, ", ")?;
                        }
                        ch.compile(f, self)?;
                        first = false;
                    }
                    tail = seg.segment_tail();
                }
                _ => {
                    write!(f, " | ")?;
                    tail.compile(f, self)?;
                    break;
                }
            }
            first = false;
        }
        write!(f, "]")?;
        Ok(())
    }

    fn compile_list(&mut self, f: &mut dyn io::Write, list: &RcTm) -> io::Result<()> {
        let mut tail = list;
        let mut first = true;
        write!(f, "[")?;
        loop {
            match tail.as_ref() {
                Tm::Nil => break,
                Tm::Cons(head, rest) => {
                    if !first {
                        write!(f, ", ")?;
                    }
                    head.compile(f, self)?;
                    tail = rest;
                }
                _ => {
                    write!(f, " | ")?;
                    tail.compile(f, self)?;
                    break;
                }
            }
            first = false;
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl Compile<SwiProlog> for Sym {
    fn compile(&self, f: &mut dyn std::io::Write, _: &mut SwiProlog) -> io::Result<()> {
        let str = self.to_str();
        if str.starts_with(|ch: char| ch.is_ascii_lowercase())
            && !str.contains(|ch: char| matches!(ch, 'a'..='z' | '_' | '0'..='9'))
        {
            write!(f, "'{str}'")?;
        } else {
            write!(f, "{str}")?;
        }
        Ok(())
    }
}

impl Compile<SwiProlog> for char {
    fn compile(&self, f: &mut dyn std::io::Write, _: &mut SwiProlog) -> io::Result<()> {
        match self {
            '\\' => write!(f, r"'\\'")?,
            '\'' => write!(f, r"'\''")?,
            '\n' => write!(f, r"'\n'")?,
            '\r' => write!(f, r"'\r'")?,
            'a'..='z' => write!(f, "{}", self)?,
            ch => write!(f, "'{ch}'")?,
        }
        Ok(())
    }
}

impl Compile<SwiProlog> for Var {
    fn compile(&self, f: &mut dyn std::io::Write, _: &mut SwiProlog) -> io::Result<()> {
        let Var { name, suffix, .. } = self.clone();
        match suffix {
            None => write!(f, "{name}"),
            Some(suffix) => write!(f, "{name}_{suffix}"),
        }
    }
}
