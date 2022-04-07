#![feature(never_type)]
use std::collections::HashMap;
use std::io::{stdin, stdout, Read, Write};
use std::iter::Peekable;

type Bytes = Peekable<&'static mut dyn Iterator<Item = u8>>;

fn expect(bs: &mut Bytes, s: &str) -> Option<()> {
    let mut it = s.bytes();
    while let Some(ref e) = it.next() {
        if Some(e) != bs.peek() {
            return None;
        }
        bs.next();
    }
    Some(())
}
fn until(bs: &mut Bytes, e: u8) -> Option<String> {
    let mut ret = String::new();
    while let Some(c) = bs.next() {
        if c == e {
            return Some(ret);
        }
        ret.push(c as char);
    }
    return Some(ret);
}
fn ws(bs: &mut Bytes) -> Option<()> {
    while let Some(b' ' | b'\t' | b'\n') = bs.peek() {
        bs.next();
    }
    Some(())
}

fn ident(bs: &mut Bytes) -> Option<String> {
    let mut ret = String::new();
    while let Some(c) = bs.peek() {
        match c {
            b'>' | b'=' | b' ' | b'\n' | b'/' => break,
            _ => {
                ret.push(bs.next()? as char);
            }
        }
    }
    if ret.len() > 0 {
        return Some(ret);
    } else {
        return None;
    }
}
fn dqstring(bs: &mut Bytes) -> Option<String> {
    expect(bs, "\"")?;
    until(bs, b'"')
}
fn sqstring(bs: &mut Bytes) -> Option<String> {
    expect(bs, "'")?;
    until(bs, b'\'')
}
fn header(bs: &mut Bytes) -> Option<()> {
    expect(bs, "<?xml ")?;
    ws(bs)?;
    println!("HEADER-START");
    while bs.peek() != Some(&b'?') {
        let attr_name = until(bs, b'=')?;
        let attr_val = dqstring(bs)?;
        ws(bs)?;
        println!("ATTR {} {}", attr_name, attr_val);
    }
    expect(bs, "?>")?;
    println!("HEADER-END");
    ws(bs)?;
    Some(())
}

#[derive(Debug)]
enum Node {
    Xml {
        name: String,
        attrs: Vec<(String, String)>,
        children: Vec<Node>,
    },
    Comment(String),
    Text(String),
    Close(String),
}

fn node(bs: &mut Bytes) -> Option<Node> {
    if *bs.peek()? == b'<' {
        bs.next()?;
        match *bs.peek()? {
            b'/' => {
                bs.next()?;
                let name = ident(bs)?;
                expect(bs, ">")?;
                return Some(Node::Close(name));
            }
            b'!' => {
                expect(bs, "!--")?;
                let mut comment = String::new();
                while comment.len() < 3 || &comment[comment.len() - 3..] != "-->" {
                    comment.push(bs.next()? as char);
                }
                comment.truncate(comment.len() - 3);
                println!("COMMENT");
                return Some(Node::Comment(comment));
            }
            _ => {}
        }
        let name = ident(bs)?;
        println!("NODE-BEGIN {}", name);
        println!("ATTRS-BEGIN");
        let mut attrs = vec![];
        let mut children = vec![];
        loop {
            ws(bs)?;
            match *bs.peek()? {
                b'/' => {
                    expect(bs, "/>")?;
                    return Some(Node::Xml {
                        name,
                        attrs,
                        children,
                    });
                }
                b'>' => {
                    bs.next()?;
                    break;
                }
                _ => {
                    let attr_name = ident(bs)?;
                    expect(bs, "=")?;
                    let attr_val = dqstring(bs)?;
                    println!("ATTR {} {}", attr_name, attr_val);
                    attrs.push((attr_name, attr_val));
                }
            }
        }
        println!("ATTRS-END");
        loop {
            match node(bs)? {
                Node::Close(name2) => {
                    if name2 == name {
                        println!("NODE-END");
                        return Some(Node::Xml {
                            name,
                            attrs,
                            children,
                        });
                    } else {
                        return None;
                    }
                }
                node => {
                    children.push(node);
                }
            }
        }
    } else {
        let mut tnode = String::new();
        let mut text = String::new();
        while *bs.peek()? != b'<' {
            match bs.next() {
                Some(b'\n') => {
                    println!("TEXT {}", text);
                    println!("TEXT-NEWLINE");
                    tnode.push('\n');
                    tnode.push_str(&text);
                    text.truncate(0);
                }
                Some(c) => {
                    text.push(c as char);
                }
                None => {
                    if text.len() > 0 {
                        println!("TEXT {}", text);
                    }
                    return None;
                }
            }
        }
        return Some(Node::Text(tnode));
    }
}

fn visit_protocol(node: Node, env: &mut HashMap<String, String>) {
    let (mut attrs, children) = expect_xml_node(node, "protocol");
    let name = attr_rm(&mut attrs, "name").expect("protocol has no name");
    println!("// PROTOCOL: {}", name);
    println!();
    let mut ifaces = vec![];
    let mut rust_name = env;
    for child in children.into_iter().filter(isnt_empty_node) {
        match child {
            Node::Xml { name, children, .. } if name == "copyright" => {
                visit_textnodes(children, "/// ");
            }
            node => {
                let iface = visit_interface(node);
                let mod_name = format!("super::{}_v{}", iface.name, iface.version);
                rust_name.insert(
                    iface.name.clone(),
                    format!("{}::{}", mod_name.clone(), reformat_name(&iface.name)),
                );
                for e in iface.enms.iter() {
                    let name = format!("{}::{}", mod_name.clone(), reformat_name(&e.name));
                    rust_name.insert(e.name.clone(), name.clone());
                    rust_name.insert(format!("{}.{}", iface.name, e.name), name.clone());
                }
                ifaces.push(iface);
            }
        }
    }

    for WlInterface {
        name,
        version,
        description,
        reqs,
        evts,
        enms,
    } in ifaces
    {
        println!("pub mod {}_v{} {{", name, version);
        println!("    use super::*;");
        if name == "wl_display" {
            println!();
            println!("    /// the main entrypoint into wayland");
            println!("    pub fn get() -> WlDisplay {{ WlDisplay {{ id: 1 }} }}");
            println!();
        }
        let iface_name = name;
        let name = reformat_name(&iface_name);
        for e in enms {
            for line in e.description {
                println!("    /// {}", line);
            }
            println!("    #[derive(Debug, Clone, Copy)]");
            println!("    #[repr(u32)]");
            println!("    pub enum {} {{", reformat_name(&e.name));
            for v in e.variants {
                if let Some(summary) = v.summary {
                    for line in summary.lines() {
                        println!("        // {}", line);
                    }
                }
                println!("        {} = {},", reformat_name(&v.name), v.value);
            }
            println!("    }}");
            println!();
            println!(
                "    impl std::fmt::Display for {} {{",
                reformat_name(&e.name)
            );
            println!("        fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {{");
            println!("            write!(f, \"{{:x}}\", *self as u32)?;");
            println!("            Ok(())");
            println!("        }}");
            println!("    }}");
            println!();
        }

        fn escape_name(name: &str) -> String {
            match name {
                "drop" | "move" | "box" | "fn" | "impl" | "async" => format!("{}_", name),
                name => String::from(name),
            }
        }

        println!();
        println!("    pub trait {}Listener {{", name);
        for e in evts.iter() {
            for line in &e.description {
                println!("        /// {}", line);
            }
            println!("        fn {}(", escape_name(&e.name));
            println!("            &self,");
            for a in &e.args {
                if let Some(summary) = &a.summary {
                    println!("            // {}", summary);
                }
                print!("            {}: ", a.name);
                if let Some(ename) = &a.en {
                    println!("{},", rust_name.get(ename).unwrap());
                } else {
                    let ty = native_type_for(&a.ty);
                    if let Some(interface) = &a.interface {
                        println!("{}<{}>,", ty, rust_name.get(interface).unwrap());
                    } else if a.ty == "object" || a.ty == "new_id" {
                        println!("{}<!>,", ty);
                    } else {
                        println!("{},", ty);
                    }
                }
            }
            println!("        ) {{}}");
            println!();
        }
        println!("    }}");
        println!();
        println!(
            "    impl WlDispatcher for (u32, Vec<Box<dyn {}Listener>>) {{",
            name
        );
        println!("        fn dispatch(&mut self, opcode: u16, data: &[u8], control: &mut dyn Iterator<Item=RawFd>) {{");
        println!("            match opcode {{");
        let mut opcode = 0u16;
        for e in evts.iter() {
            println!("                {} => {{", opcode);
            for a in &e.args {
                print_deser_arg(a, &rust_name);
            }
            println!("                    if std::env::var(\"WAYLAND_DEBUG\").is_ok() {{");
            print!(
                "                        println!(\"[time]: <- {}.{}(",
                iface_name, e.name
            );
            print!("self: {{}}");
            for a in e.args.iter() {
                if a.ty == "array" {
                    print!(", {}: {{:?}}", a.name);
                } else {
                    print!(", {}: {{}}", a.name);
                }
            }
            print!(")\", self.0");
            for a in e.args.iter() {
                if a.ty == "array" {
                    print!(", &{}[..std::cmp::min(5, {}.len())]", a.name, a.name);
                } else {
                    print!(", {}", a.name);
                }
            }
            println!(");");
            println!("                    }}");
            println!("                    for l in self.1.iter() {{");
            print!("                        l.{}(", escape_name(&e.name));
            for a in &e.args {
                print!("{}.clone(), ", a.name);
            }
            println!(");");
            println!("                    }}");
            println!("                }}");
            opcode += 1;
        }
        println!("                _ => unreachable!(\"invalid opcode\"),");
        println!("            }}");
        println!("        }}");
        println!("    }}");

        println!();
        for line in description {
            println!("    /// {}", line);
        }
        println!("    #[repr(transparent)]");
        println!("    pub struct {} {{", name);
        println!("        id: u32,");
        println!("    }}");
        println!();
        println!("    impl {} {{", name);
        println!(
            "        pub fn add_listener(&mut self, listener: Box<dyn {}Listener>) {{",
            name
        );
        println!("            let lock = super::get_listeners();");
        println!("            let mut m = lock.write().unwrap();");
        println!("            if let Some(ls) = m.get_mut(&self.id) {{");
        println!("                let ls : &mut (u32, Vec<Box<dyn {}Listener>>) = (ls as &mut dyn std::any::Any).downcast_mut().unwrap();", name);
        println!("                ls.1.push(listener);");
        println!("            }} else {{");
        println!(
            "                m.insert(self.id, Box::new((self.id, vec![listener])) as Box<dyn WlDispatcher>);"
        );
        println!("            }}");
        println!("        }}");

        fn print_deser_arg(a: &WlArg, rust_name: &HashMap<String, String>) {
            match a.ty.as_ref() {
                "uint" | "int" | "fixed" | "object" | "new_id" => {
                    println!(
                        "                    let ({}, data) = (&data[..4], &data[4..]);",
                        a.name,
                    );
                }
                "fd" => {}
                "string" | "array" => {
                    println!("                    let (len, data) = (&data[..4], &data[4..]);");
                    println!("                    let len = u32::from_ne_bytes(len.try_into().unwrap()) as usize;");
                    println!("                    let size = (len / 4) * 4;");
                    println!(
                        "                    let ({}, data) = (&data[..len], &data[size..]);",
                        a.name
                    );
                }
                _ => println!(
                    "                    let {} : &[u8] = todo!(\"deser {}\");",
                    a.name, a.ty
                ),
            }
            match a.ty.as_ref() {
                "uint" | "int" | "fixed" => {
                    println!(
                        "                    let {} = {}::from_ne_bytes({}.try_into().unwrap());",
                        a.name,
                        native_type_for(&a.ty),
                        a.name
                    );
                    if let Some(ename) = &a.en {
                        println!(
                            "                    let {} : {} = unsafe {{ std::mem::transmute({}) }};",
                            a.name,
                            rust_name.get(ename).unwrap(),
                            a.name,
                        );
                    }
                }
                "object" | "new_id" => {
                    println!(
                        "                    let {} = WlObject{{ obj_id: u32::from_ne_bytes({}.try_into().unwrap()), _p: std::marker::PhantomData }};",
                        a.name,
                        a.name
                    );
                }
                "string" => {
                    println!(
                        "                    let {} = {}::from({});",
                        a.name,
                        native_type_for(&a.ty),
                        a.name
                    );
                }
                "array" => {
                    println!(
                        "                    let {} = Vec::from({});",
                        a.name, a.name
                    );
                }
                "fd" => {
                    println!(
                        "                    let {} : RawFd = control.next().unwrap();",
                        a.name
                    );
                }
                _ => println!(
                    "                    let {} = todo!(\"deser {}\");",
                    a.name, a.ty
                ),
            }
        }

        println!();
        let mut opcode = 0u16;
        for r in reqs {
            println!();
            for line in r.description {
                println!("        /// {}", line);
            }
            println!("        pub fn {}(", escape_name(&r.name));
            println!("            &mut self,");
            for a in r.args.iter() {
                if let Some(summary) = &a.summary {
                    println!("            // {}", summary);
                }
                print!("            {}: ", a.name);
                if let Some(ename) = &a.en {
                    println!("{},", rust_name.get(ename).unwrap());
                } else {
                    let ty = native_type_for(&a.ty);
                    if let Some(interface) = &a.interface {
                        if a.ty == "new_id" {
                            println!(
                                "&mut MaybeUninit<{}<{}>>,",
                                ty,
                                rust_name.get(interface).unwrap()
                            );
                        } else {
                            println!("{}<{}>,", ty, rust_name.get(interface).unwrap());
                        }
                    } else if a.ty == "object" {
                        println!("{}<!>,", ty);
                    } else if a.ty == "new_id" {
                        println!("&mut MaybeUninit<{}<!>>,", ty);
                    } else {
                        println!("{},", ty);
                    }
                }
            }
            println!("        ) {{");

            for a in r.args.iter() {
                if a.ty == "new_id" {
                    println!("            {}.write(WlObject{{ obj_id: WL_MAX_OBJ_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst), _p: std::marker::PhantomData }});", a.name);
                    println!("            // SAFETY: we just initialized it and we have an exclusize referenc");
                    println!(
                        "            let {} = unsafe {{ {}.assume_init_ref() }};",
                        a.name, a.name
                    );
                }
            }

            println!("            if std::env::var(\"WAYLAND_DEBUG\").is_ok() {{");
            print!(
                "                println!(\"[time]: -> {}.{}(",
                iface_name, r.name
            );
            print!("self: {{}}");
            for a in r.args.iter() {
                print!(", {}: {{}}", a.name);
            }
            print!(")\", self.id");
            for a in r.args.iter() {
                print!(", {}", a.name);
            }
            println!(");");
            println!("            }}");

            println!("            let mut buf : Vec<u8> = vec![0; 8];");
            println!("            buf[..4].copy_from_slice(&self.id.to_ne_bytes()[..]);");
            for a in r.args {
                match a.ty.as_str() {
                    "string" => {
                        println!(
                            "            for b in {}.as_bytes() {{ buf.push(*b); }}",
                            a.name
                        );
                    }
                    "array" => {
                        println!(
                            "            for b in {}.as_ref() {{ buf.push(*b); }}",
                            a.name
                        );
                    }
                    "int" | "uint" | "fixed" | "object" | "new_id" => {
                        if a.en.is_none() {
                            println!(
                                "            for b in {}.to_ne_bytes() {{ buf.push(b); }}",
                                a.name
                            );
                        } else {
                            println!(
                                "            for b in ({} as u32).to_ne_bytes() {{ buf.push(b); }}",
                                a.name
                            );
                        }
                    }
                    "fd" => {
                        println!(
                            "            for b in ({} as i32).to_ne_bytes() {{ buf.push(b); }}",
                            a.name
                        );
                    }
                    _ => {
                        panic!("don't know how to serialize {}", a.ty);
                    }
                }
            }
            println!(
                "            let sz_op : u32 = ((buf.len() as u32) << 16) | ({} & 0xffff);",
                opcode
            );
            println!("            buf[4..8].copy_from_slice(&sz_op.to_ne_bytes()[..]);");
            opcode += 1;
            println!("            send(buf.as_ref(), &[]).unwrap();");
            println!("        }}");
        }
        println!("    }}");
        println!();
        println!("    impl WlDeref for {} {{}}", name);
        println!();
        println!("    impl std::cmp::PartialEq for {} {{", name);
        println!("        fn eq(&self, other: &Self) -> bool {{ self.id == other.id }}");
        println!("        fn ne(&self, other: &Self) -> bool {{ self.id != other.id }}");
        println!("    }}");
        println!("}}");
        println!();
    }
    println!("// PROTOCOL END");
}

fn expect_xml_node(node: Node, expected_name: &str) -> (Vec<(String, String)>, Vec<Node>) {
    match node {
        Node::Xml {
            name,
            attrs,
            children,
        } if name == expected_name => (attrs, children),
        node => panic!("expected {}, got: {:?}", expected_name, node),
    }
}

fn attr_rm(attrs: &mut Vec<(String, String)>, name: &str) -> Option<String> {
    let ind = (0..attrs.len()).filter(|&i| attrs[i].0 == name).next()?;
    Some(attrs.swap_remove(ind).1)
}

fn isnt_empty_node(node: &Node) -> bool {
    match node {
        Node::Text(s) => !s.chars().all(|c| c.is_ascii_whitespace()),
        Node::Comment(_) => false,
        _ => true,
    }
}

fn reformat_name(name: &str) -> String {
    let mut ret = String::with_capacity(name.len());
    if name.len() == 0 {
        return ret;
    }
    // wayland has enum variants named "90"...
    if name.starts_with(|c: char| c.is_numeric()) {
        ret.push('N');
    }
    let mut next_upper = true;
    for c in name.chars() {
        if c == '_' {
            next_upper = true;
        } else if next_upper {
            for c in c.to_uppercase() {
                ret.push(c);
            }
            next_upper = false;
        } else {
            ret.push(c);
        }
    }
    ret
}

fn native_type_for(ty: &str) -> &str {
    match ty {
        "int" => "i32",
        "uint" => "u32",
        "fixed" => "WlFixed",
        "object" => "WlObject",
        "new_id" => "WlObject",
        "string" => "WlString",
        "array" => "Vec<u8>",
        "fd" => "RawFd",
        _ => "u32",
    }
}

fn visit_textnodes(nodes: Vec<Node>, prefix: &str) {
    for node in nodes {
        match node {
            Node::Text(text) => {
                for line in text.lines() {
                    println!("{}{}", prefix, line);
                }
            }
            _ => panic!("expected text, got: {:?}", node),
        }
    }
}

fn collect_description(nodes: Vec<Node>) -> Vec<String> {
    let mut ret: Vec<String> = Vec::with_capacity(nodes.len());
    for node in nodes {
        match node {
            Node::Text(text) => {
                for line in text.trim_matches('\n').lines() {
                    ret.push(line.trim_start().to_string());
                }
            }
            _ => panic!("expected text, got: {:?}", node),
        }
    }
    ret
}

struct WlArg {
    name: String,
    ty: String,
    interface: Option<String>,
    en: Option<String>,
    summary: Option<String>,
}
struct WlRequest {
    name: String,
    description: Vec<String>,
    args: Vec<WlArg>,
}
fn visit_request(node: Node) -> WlRequest {
    let (mut attrs, children) = expect_xml_node(node, "request");
    let name = attr_rm(&mut attrs, "name").expect("request has no name");

    let mut description = vec![];
    let mut args = vec![];
    for child in children.into_iter().filter(isnt_empty_node) {
        match child {
            Node::Xml { name, children, .. } if name == "description" => {
                // ignore summary
                description = collect_description(children);
            }
            node => {
                let (mut attrs, children) = expect_xml_node(node, "arg");
                let summary = attr_rm(&mut attrs, "summary");
                let name = attr_rm(&mut attrs, "name").expect("arg has no name");
                let ty = attr_rm(&mut attrs, "type").expect("arg has no type");
                let interface = attr_rm(&mut attrs, "interface");
                let en = attr_rm(&mut attrs, "enum");
                assert!(children.len() == 0, "arg shouldn't have any child nodes");
                args.push(WlArg {
                    name,
                    ty,
                    interface,
                    en,
                    summary,
                });
            }
        }
    }

    WlRequest {
        name,
        description,
        args,
    }
}

struct WlEvent {
    name: String,
    description: Vec<String>,
    args: Vec<WlArg>,
}
fn visit_event(node: Node) -> WlEvent {
    let (mut attrs, children) = expect_xml_node(node, "event");
    let name = attr_rm(&mut attrs, "name").expect("event has no name");

    let mut description = vec![];
    let mut args = vec![];
    for child in children.into_iter().filter(isnt_empty_node) {
        match child {
            Node::Xml { name, children, .. } if name == "description" => {
                description = collect_description(children);
            }
            node => {
                let (mut attrs, children) = expect_xml_node(node, "arg");
                let summary = attr_rm(&mut attrs, "summary");
                let name = attr_rm(&mut attrs, "name").expect("arg has no name");
                let ty = attr_rm(&mut attrs, "type").expect("arg has no type");
                let interface = attr_rm(&mut attrs, "interface");
                let en = attr_rm(&mut attrs, "enum");
                assert!(children.len() == 0, "arg shouldn't have any child nodes");
                args.push(WlArg {
                    name,
                    ty,
                    interface,
                    en,
                    summary,
                });
            }
        }
    }

    WlEvent {
        name,
        description,
        args,
    }
}

struct WlVariant {
    name: String,
    value: String,
    summary: Option<String>,
}
struct WlEnum {
    name: String,
    description: Vec<String>,
    variants: Vec<WlVariant>,
}
fn visit_enum(node: Node) -> WlEnum {
    let (mut attrs, children) = expect_xml_node(node, "enum");
    let name = attr_rm(&mut attrs, "name").expect("enum has no name");

    let mut description = vec![];
    let mut vars = vec![];
    for child in children.into_iter().filter(isnt_empty_node) {
        match child {
            Node::Xml { name, children, .. } if name == "description" => {
                description = collect_description(children);
            }
            node => {
                let (mut attrs, _) = expect_xml_node(node, "entry");
                let name = attr_rm(&mut attrs, "name").expect("enum entry has no name");
                let value = attr_rm(&mut attrs, "value").expect("enum entry has no value");
                let summary = attr_rm(&mut attrs, "summary");
                vars.push(WlVariant {
                    name,
                    value,
                    summary,
                });
            }
        }
    }

    WlEnum {
        name,
        description,
        variants: vars,
    }
}

struct WlInterface {
    name: String,
    version: String,
    description: Vec<String>,
    reqs: Vec<WlRequest>,
    evts: Vec<WlEvent>,
    enms: Vec<WlEnum>,
}
fn visit_interface(node: Node) -> WlInterface {
    let (mut attrs, children) = expect_xml_node(node, "interface");
    let name = attr_rm(&mut attrs, "name").expect("interface has no name");
    let version = attr_rm(&mut attrs, "version").expect("interface has no version");

    let mut description = vec![];
    let mut reqs = vec![];
    let mut evts = vec![];
    let mut enms = vec![];
    for child in children.into_iter().filter(isnt_empty_node) {
        match child {
            Node::Xml {
                name,
                attrs: _,
                children,
            } if name == "description" => {
                // ignore the summary
                description = collect_description(children);
            }
            Node::Xml { ref name, .. } if name == "request" => {
                reqs.push(visit_request(child));
            }
            Node::Xml { ref name, .. } if name == "event" => {
                evts.push(visit_event(child));
            }
            Node::Xml { ref name, .. } if name == "enum" => {
                enms.push(visit_enum(child));
            }
            _ => {}
        }
    }

    WlInterface {
        name,
        version,
        description,
        reqs,
        evts,
        enms,
    }
}

fn print_common() {
    stdout()
        .write_all(
            br#"
#![feature(never_type)]
#![feature(unix_socket_ancillary_data)]
#![allow(unused)]
use std::io::{self, Write, Read};
use std::mem::MaybeUninit;
use std::os::unix::io::{RawFd, FromRawFd};
use std::os::unix::net::{UnixStream, SocketAncillary, AncillaryData};

static WL_MAX_OBJ_ID : std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(2);
static mut WL_LISTENERS : std::mem::MaybeUninit<std::sync::RwLock<std::collections::HashMap<u32, Box<dyn WlDispatcher>>>> = std::mem::MaybeUninit::uninit();
static WL_LISTENERS_INIT : std::sync::Once = std::sync::Once::new();
static mut WL_SOCKET : std::mem::MaybeUninit<std::sync::Mutex<WlSocket>> = std::mem::MaybeUninit::uninit();
static WL_SOCKET_INIT : std::sync::Once = std::sync::Once::new();

fn get_listeners<'a>() -> &'a std::sync::RwLock<std::collections::HashMap<u32, Box<dyn WlDispatcher>>> {
    // SAFETY: use of mutable static
    // mutate happens inside std::sync::once
    // reads only happens through this function and *after* the call_once mutate
    WL_LISTENERS_INIT.call_once(|| unsafe {
        WL_LISTENERS.write(std::sync::RwLock::new(std::collections::HashMap::new()));
    });
    unsafe { WL_LISTENERS.assume_init_ref() }
}

struct WlSocket {
    sock: UnixStream,
    data: [u8; 1024],
    data_len: usize,
    fds: [RawFd; 128],
    fds_len: usize,
}

fn get_socket<'a>() -> &'a std::sync::Mutex<WlSocket> {
    use std::str::FromStr;
    use std::os::unix::io::AsRawFd;
    WL_SOCKET_INIT.call_once(|| {
        // logic stolen from https://wayland-book.com/protocol-design/wire-protocol.html
        let mut sock = if let Ok(socket) = std::env::var("WAYLAND_SOCKET") {
            let fd = i32::from_str(&socket).expect(&format!("WAYLAND_SOCKET is not a fd: {}", socket));
            // SAFETY: if parent process gave us this fd, then it is up to them to not give it to
            // anyone else, so we own this fd
            unsafe { UnixStream::from_raw_fd(fd.as_raw_fd()) }
        } else if let Ok(rundir) = std::env::var("XDG_RUNTIME_DIR") {
            let display = std::env::var("WAYLAND_DISPLAY").unwrap_or(String::from("wayland-0"));
            if std::env::var("WAYLAND_DEBUG").is_ok() {
                println!("[time]: open socket (WAYLAND_DISPLAY): {}/{}", rundir, display);
            }
            UnixStream::connect(format!("{}/{}", rundir, display)).expect(&format!("cannot open socket: {}/{}", rundir, display))
        } else {
            panic!("Can't find socket with WAYLAND_SOCKET or WAYLAND_DISPLAY");
        };
        let sock = WlSocket {
            sock,
            data: [0; 1024],
            data_len: 0,
            fds: [0; 128],
            fds_len: 0,
        };
        // SAFETY: write to static happens inside std::once
        unsafe {
            WL_SOCKET.write(std::sync::Mutex::new(sock));
        }
    });
    // SAFETY: read from static maybe uninit happens only *after* std::once initializes it
    unsafe { WL_SOCKET.assume_init_ref() }
}

fn send(data: &[u8], fds: &[RawFd]) -> std::io::Result<()> {
    let sock = get_socket();
    let data = &[std::io::IoSlice::new(data)][..];
    let mut abuf = [0; 128];
    let mut a = SocketAncillary::new(&mut abuf[..]);
    a.add_fds(fds);
    sock.lock().unwrap().sock.send_vectored_with_ancillary(data, &mut a)?;
    Ok(())
}

fn recv(sock: &mut UnixStream, data: &mut [u8], data_len: &mut usize, fds: &mut [RawFd], fds_len: &mut usize) -> std::io::Result<()> {
    let data = &mut [std::io::IoSliceMut::new(data)][..];
    let mut abuf = [0; 128];
    let mut a = SocketAncillary::new(&mut abuf[..]);
    *data_len = sock.recv_vectored_with_ancillary(data, &mut a)?;
    for ares in a.messages() {
        if let AncillaryData::ScmRights(scm_rights) = ares.expect("failed to receive ancillary socket data") {
            for fd in scm_rights {
                if *fds_len < fds.len() {
                    fds[*fds_len] = fd;
                    *fds_len += 1;
                }
            }
        }
    }
    Ok(())
}

pub fn poll() {
    let mut sock = get_socket().lock().unwrap();
    let WlSocket {
        ref mut sock,
        ref mut data,
        ref mut data_len,
        ref mut fds,
        ref mut fds_len
    } = *sock;
    {
        let data = &mut data[*data_len..];
        let mut dlen = 0;
        let fds = &mut fds[*fds_len..];
        let mut flen = 0;
        recv(sock, data, &mut dlen, fds, &mut flen).unwrap();
        *data_len += dlen;
        *fds_len += flen;
    }

    let mut i = 0;
    let fd_iter = &mut fds[..*fds_len].to_vec().into_iter();
    let mut listeners = get_listeners().write().unwrap();
    while *data_len >= i+8 {
        let obj_id = &data[i..i+4];
        let obj_id = u32::from_ne_bytes(obj_id.try_into().unwrap());
        let msg_op = &data[i+4..i+8];
        let msg_op = u32::from_ne_bytes(msg_op.try_into().unwrap());
        let msg_len = msg_op >> 16;
        let msg_len = msg_len as usize;
        let opcode = (msg_op & 0xffff) as u16;
        if *data_len >= i+msg_len {
            let msg_data = &data[i+8..i+msg_len];
            if let Some(dispatcher) = listeners.get_mut(&obj_id) {
                dispatcher.dispatch(opcode, msg_data, fd_iter);
            }
        } else {
            break;
        }
        i += msg_len;
    }
    if i < *data_len {
        data.copy_within(i..*data_len, 0);
        *data_len -= i;
    }
    let fds_left = fd_iter.count();
    if fds_left > 0 {
        fds.copy_within((*fds_len-fds_left)..*fds_len, 0);
        *fds_len = fds_left;
    }
}

trait WlDispatcher {
    fn dispatch(&mut self, opcode: u16, data: &[u8], control: &mut dyn Iterator<Item=RawFd>);
}

#[repr(transparent)]
pub struct WlObject<T> { obj_id: u32, _p: std::marker::PhantomData<T> }

impl <T> WlObject<T> {
    fn to_ne_bytes(&self) -> [u8; 4] {
        self.obj_id.to_ne_bytes()
    }

}

impl <T: WlDeref> WlObject<T> {
    pub fn get_ref(&self) -> &T {
        unsafe { std::mem::transmute(self) }
    }

    pub fn get_mut(&mut self) -> &mut T {
        unsafe { std::mem::transmute(self) }
    }
}

impl <T> std::clone::Clone for WlObject<T> {
    fn clone(&self) -> WlObject<T> {
        return *self
    }
}

impl <T> std::marker::Copy for WlObject<T> {}

impl <T> std::fmt::Display for WlObject<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "0x{:x}", self.obj_id)?;
        Ok(())
    }
}

pub trait WlDeref {}
/*
impl <T: WlDeref> std::ops::Deref for WlObject<T> {
    type Target = T;
    fn deref(&self) -> &T { unsafe { std::mem::transmute(self) } }
}
*/

#[derive(Clone, Copy)]
pub struct WlFixed(u32);

impl WlFixed {
    fn to_ne_bytes(&self) -> [u8; 4] {
        self.0.to_ne_bytes()
    }
    fn from_ne_bytes(bs: [u8; 4]) -> WlFixed {
        WlFixed(u32::from_ne_bytes(bs))
    }
}

impl std::fmt::Display for WlFixed {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}.{:0}", self.0 >> 8, self.0 & 0xff)?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct WlString(Vec<u8>);

impl WlString {
    fn as_bytes(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl <T: Into<Vec<u8>>> From<T> for WlString {
    fn from(other: T) -> WlString {
        WlString(other.into())
    }
}

impl std::fmt::Display for WlString {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", std::str::from_utf8(&self.0[..]).unwrap())?;
        Ok(())
    }
}

"#,
        )
        .unwrap();
}

fn main() {
    let stdin = stdin();
    let stdin = stdin.lock();
    let stdin = stdin.bytes().flat_map(Result::ok);
    // gotta leak it even though it's static :shrug:
    let stdin: &mut dyn Iterator<Item = u8> = Box::leak(Box::new(stdin));
    let mut stdin = stdin.peekable();

    header(&mut stdin).unwrap();
    let mut protocols = vec![];
    while let Some(body) = node(&mut stdin) {
        if let Node::Close(name) = body {
            panic!("unexpected closing node {}", name);
        }
        protocols.push(body);
        ws(&mut stdin);
        header(&mut stdin);
    }
    if stdin.peek().is_some() {
        panic!(
            "unfinished:{}...",
            stdin.take(10).map(|b| b as char).collect::<String>()
        )
    }
    println!("// GEN BEGIN");
    print_common();
    let mut env = HashMap::new();
    for p in protocols {
        visit_protocol(p, &mut env);
    }
    println!("// GEN DONE");
}
