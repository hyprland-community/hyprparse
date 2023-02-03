use hex::decode;
use lazy_static::lazy_static;
use regex::{Captures, Regex};
use std::collections::BTreeMap;

use std::string::ToString;
use strum_macros::Display;
#[derive(Display, Debug, Clone)]
pub enum Mod {
    SUPER,
    ALT,
    CTRL,
    SHIFT,
}

#[derive(Debug, Clone)]
pub struct Color(u8, u8, u8, u8);
impl Color {
    fn from_rgb(r: u8, g: u8, b: u8) -> Color {
        Self(r, g, b, 255)
    }
    fn from_rgba(r: u8, g: u8, b: u8, a: u8) -> Color {
        Self(r, g, b, a)
    }
}

impl std::fmt::Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "rgba({}, {}, {}, {})", self.0, self.1, self.2, self.3)
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    Int(i64),
    Bool(bool),
    Float(f64),
    Color(Color),
    Vec2(f64, f64),
    Mod(Mod),
    Gradient(Vec<Color>, u16),
    Variable(String),
    Str(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Token::*;
        write!(
            f,
            "{}",
            match self {
                Int(i) => i.to_string(),
                Bool(t) => t.to_string(),
                Float(f) => f.to_string(),
                Color(c) => c.to_string(),
                Vec2(v1, v2) => format!("{v1} {v2}"),
                Mod(m) => m.to_string(),
                Gradient(colors, degrees) => format!("{} {degrees}deg", join(colors.to_vec(), ' ')),
                Str(s) => s.clone(),
                Variable(v) => format!("${v}"),
            }
        )
    }
}

//#[derive(Debug)]
//pub enum AstObj<T, D> {
//    Token(T),
//    Tree(D),
//}
pub type AstObj = Token;
/*impl<T, D> AstObj<T, D> {
    fn unwrap_token(self) -> T {
        match self {
            Token(v) => v,
            Tree(_) => panic!("Tried to unwrap `token`, when value was `tree`")
        }
    }
    fn unwrap_tree(self) -> D {
        match self {
            Tree(v) => v,
            Token(_) => panic!("Tried to unwrap `tree`, when value was `token`")
        }
    }
}*/
/*
fn ser_ast_obj(tree: AstObj) -> String {
    let mut buf = String::new();
    for (k, v) in self.0.iter() {
        buf += match i {
            Self::Tree(tree) => {
                buf += k + " {";
                buf += ser_ast_obj(tree);
                buf += "}";
            }
            Self::Token(item) => item.to_string()
        };
    }
    buf
}

impl std::fmt::Display for AstObj {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut buf = String::new();
        buf += ser_ast_obj(self);
        write!(f, "{buf}")
    }
}
*/
#[derive(Debug, Clone, Default)]
pub struct AstRoot {
    config: BTreeMap<String, Vec<Token>>,
    keywords: BTreeMap<String, Vec<Token>>,
}

fn join<T: ToString, Str: ToString>(vec: Vec<T>, sep: Str) -> String {
    let mut buf = "".to_string();
    for (index, i) in vec.iter().enumerate() {
        buf += &i.to_string();
        if index != vec.len() - 1 {
            buf += &sep.to_string();
        }
    }
    buf
}

impl std::fmt::Display for AstRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let key_ser = {
            let keys = self.keywords.clone();
            let mut buf = "".to_string();
            for (k, v) in keys.iter() {
                buf += &format!("{k} = {}", join(v.to_vec(), ','));
            }
            buf
        };
        let config_ser = {
            let confs = self.config.clone();
            let mut buf = "".to_string();
            for (k, v) in confs.iter() {
                buf += &format!("{k} = {}", join(v.to_vec(), ','));
            }
            buf
        };
        write!(f, "{}\n{}", config_ser, key_ser)
    }
}

fn get_val_as<T: std::str::FromStr>(cs: &Captures, v: &str) -> T
where
    <T as std::str::FromStr>::Err: std::fmt::Debug,
{
    cs[v].parse().expect("error parsing string as")
}

fn parse_color(cs: &Captures, num: u8) -> Color {
    match num {
        0 => {
            // Rgb
            let color = Color::from_rgb(
                get_val_as::<u8>(&cs, "r"),
                get_val_as::<u8>(&cs, "g"),
                get_val_as::<u8>(&cs, "b"),
            );
            color
        }
        1 => {
            // Rgba
            let color = Color::from_rgba(
                get_val_as::<u8>(&cs, "r"),
                get_val_as::<u8>(&cs, "g"),
                get_val_as::<u8>(&cs, "b"),
                get_val_as::<u8>(&cs, "a"),
            );
            color
        }
        2 => {
            // Rgba
            let color = Color::from_rgba(
                decode(&cs["r"]).unwrap()[0],
                decode(&cs["g"]).unwrap()[0],
                decode(&cs["b"]).unwrap()[0],
                decode(&cs["a"]).unwrap()[0],
            );
            color
        }
        _ => unreachable!(),
    }
}
// [rgb][(3,4,3)] [,hello,bye]
fn split_args(s: String) -> Vec<String> {
    let mut buf = String::new();
    for i in s.split(")").map(|s| {
        if s.contains("(") {
            s.to_string() + ")"
        } else {
            s.to_string()
        }
    }) {
        let text = if i.contains("rgba") {
            Some("rgba(")
        } else if i.contains("rgb") {
            Some("rgb(")
        } else {
            None
        };
        for j in i.split("(").map(|s| {
            if s.contains(")") {
                let test = match text {
                    Some(v) => v.to_string() + s,
                    None => s.to_string(),
                };
                test
            } else {
                s.to_string()
            }
        }) {
            if j.ends_with(")") {
                buf += &(j.replace(",", "!@!") + " ");
            } else if (j.trim() == "rgb") || (j.trim() == "rgba") {
                buf += "";
            } else {
                buf += &j;
            }
        }
    }
    let new = buf
        .trim()
        .split(",")
        .map(|s| s.to_string().replace("!@!", ","))
        .collect();
    new
}

pub fn line_parse(line: &str) -> (String, Vec<Token>) {
    use regex::RegexSet;
    lazy_static! {
      static ref TOKEN_SET: RegexSet = RegexSet::new([
        r"^rgb\((?P<r>\d*), *(?P<g>\d*), *(?P<b>\d*)\)$", // rgb
        r"^rgba\((?P<r>\d*), *(?P<g>\d*), *(?P<b>\d*), *(?P<a>\d*)\)$", // rgba
        r"^0x(?P<a>[[:xdigit:]]{2})(?P<r>[[:xdigit:]]{2})(?P<g>[[:xdigit:]]{2})(?P<b>[[:xdigit:]]{2})$", // legacy argb
        r"^(true|false|yes|no|0|1)$", // bool
        r"^(\d*.\d*)$", // float
        r"^(?P<v1>\d*.\d*) +(?P<v2>\d*.\d*)$", // vec2
        r"^\-?\d*$", // Int
        r"^([sS][uU][pP][eE][rR]|CTRL|Ctrl|ctrl|ALT|alt|Alt)$", // MOD
        r"^(?P<pre>.*?) *(?P<deg>\d*) *deg$", // gradient
        r"^\$(?P<name>.*)$", // Variable
        r".*?" // Str
      ]).expect("Error creating regex set");
      static ref TOKEN_REGEXES: Vec<Regex> = TOKEN_SET
        .patterns()
        .iter()
        .map(|pat| Regex::new(pat).expect("error creating regex"))
        .collect();
    }

    let (pre, tokens) = {
        let stuff: Vec<_> = line.split('=').map(|l| l.trim()).collect();
        (stuff[0], stuff[1])
    };
    let mut all_tokens: Vec<Token> = vec![];
    for token in split_args(tokens.to_string()).iter() {
        let token = token.as_str();
        let matches: Vec<_> = TOKEN_SET.matches(token).iter().collect();

        let captures = if !TOKEN_REGEXES.is_empty() && !matches.is_empty() {
            match TOKEN_REGEXES[matches[0]].captures(token) {
                Some(captures) => captures,
                None => panic!("Regex has no captures"),
            }
        } else {
            panic!("Nothing was matched")
        };

        let parsed = if matches.len() >= 2 {
            match matches[0] {
                0 => Token::Color(parse_color(&captures, 0)), // Rgb
                1 => Token::Color(parse_color(&captures, 1)), // Rgba
                2 => Token::Color(parse_color(&captures, 2)), // Argb
                3 => Token::Bool(match token {
                    "true" | "yes" | "1" => true,
                    "false" | "no" | "0" => false,
                    _ => unreachable!(),
                }),
                4 => Token::Float(token.parse::<f64>().unwrap()),
                5 => Token::Vec2(
                    captures["v1"].parse::<f64>().unwrap(),
                    captures["v2"].parse::<f64>().unwrap(),
                ),
                6 => Token::Int(token.parse::<i64>().unwrap()),
                7 => {
                    use Mod::*;
                    Token::Mod(match token.to_lowercase().as_str() {
                        "super" => SUPER,
                        "alt" => ALT,
                        "ctrl" => CTRL,
                        _ => unreachable!(),
                    })
                }
                8 => {
                    let colors = {
                        let mut colors: Vec<Color> = vec![];
                        let mut rgbs: Vec<_> = captures["pre"]
                            .split(") ")
                            .map(|v| v.trim())
                            .map(|v| v.to_string() + ")")
                            .collect();
                        rgbs.pop();
                        for i in rgbs {
                            if i.starts_with("rgba(") {
                                colors
                                    .push(parse_color(&TOKEN_REGEXES[1].captures(&i).unwrap(), 1));
                            } else if i.starts_with("0x") {
                                colors
                                    .push(parse_color(&TOKEN_REGEXES[2].captures(&i).unwrap(), 2));
                            } else {
                                colors.push(parse_color(
                                    &TOKEN_REGEXES[0].captures(&i).expect("invalid color"),
                                    0,
                                ));
                            }
                        }
                        colors
                    };
                    Token::Gradient(colors, captures["deg"].parse::<u16>().unwrap())
                }
                9 => Token::Variable(captures["name"].to_string()),
                10 => Token::Str(token.trim().to_string()),
                _ => todo!(),
            }
        } else if matches.len() == 1 && matches[0] == 10 {
            Token::Str(token.trim().to_string())
        } else {
            panic!("no matches {matches:#?}")
        };
        all_tokens.push(parsed);
    }
    (pre.to_string(), all_tokens)
}

pub fn whole_parser(cfg: &str) -> AstRoot {
    let lines = cfg
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty());
    let mut ast = AstRoot::default();
    let mut vars: BTreeMap<String, Token> = BTreeMap::new();
    let mut depth = vec![];
    for (ln, line) in lines.enumerate() {
        if line.trim().starts_with("#") {
            continue;
        } else if line.ends_with('{') {
            depth.push(line.trim().split(" {").collect::<Vec<_>>()[0]);
            continue;
        } else if line.trim().ends_with('}') {
            depth.pop();
            continue;
        } else if line.trim().starts_with('$') {
            line.replace('$', "");
            let (name, tokens) = line_parse(line);
            vars.insert(name, tokens[0].clone());
        } else {
            let (pre, _) = {
                let stuff: Vec<_> = line.split('=').map(|l| l.trim()).collect();
                (stuff[0], stuff[1])
            };
            if depth.len() == 0 && !pre.contains(":") {
                let (name, tokens) = line_parse(line);
                ast.keywords.insert(name, tokens);
                continue;
            }
            set_value(&mut ast, depth.clone(), line.to_string());
        };
    }
    ast
    //AstObj(BTreeMap::new())
}

fn set_value(ast: &mut AstRoot, depth: Vec<&str>, line: String) {
    let (pre, _) = {
        let stuff: Vec<_> = line.split('=').map(|l| l.trim()).collect();
        (stuff[0], stuff[1])
    };
    //let mut line_depth: Vec<_> = pre.split(':').collect();
    //line_depth.pop();
    //let mut shared_depth = depth.clone();
    //shared_depth.append(&mut line_depth);
    //let length = shared_depth.len();
    let name = if depth.len() != 0 {
        join(depth, ':') + ":" + pre
    } else {
        pre.to_string()
    };
    ast.config.insert(name, line_parse(&line).1);
    /*for (index, i) in shared_depth.iter().enumerate() {
        if index == length - 1 {

        }
        //let head = &ast.0.unwrap_that();
        match head.get(i) {
            Some(This(_)) => panic!("Value cannot be owned by token!"),
            Some(That(tree)) =>
            None => head.insert
        }
    }*/
}
