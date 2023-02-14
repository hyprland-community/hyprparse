use hex::decode;
use lazy_static::lazy_static;
use regex::{Captures, Regex};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use std::string::ToString;
use strum_macros::Display;
#[derive(Display, Debug, Clone, Serialize, Deserialize)]
pub enum Mod {
    SUPER,
    ALT,
    CTRL,
    SHIFT,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Token {
    Empty,
    Int(i64),
    Bool(bool),
    Float(f64),
    Color(Color),
    Vec2(f64, f64),
    Mod(Vec<Mod>),
    Gradient(Vec<Color>, u16),
    Variable(String),
    Str(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Arg {
    Named(String, Token),
    Unnamed(Token),
}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Arg::Named(n, t) => format!("{n}: {t}"),
                Arg::Unnamed(t) => t.to_string(),
            }
        )
    }
}

impl Arg {
    pub fn into_token(self) -> Token {
        match self {
            Arg::Named(_, t) => t,
            Arg::Unnamed(t) => t,
        }
    }
    pub fn into_token_mut(&mut self) -> &mut Token {
        match self {
            Arg::Named(_, t) => t,
            Arg::Unnamed(t) => t,
        }
    }
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
                Mod(m) => join(m.to_vec(), '_'),
                Gradient(colors, degrees) => format!("{} {degrees}deg", join(colors.to_vec(), ' ')),
                Str(s) => s.clone(),
                Empty => "".to_string(),
                Variable(v) => format!("${v}"),
            }
        )
    }
}

pub type AstObj = Token;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AstRoot {
    config: BTreeMap<String, Vec<Arg>>,
    keywords: BTreeMap<String, Vec<Vec<Arg>>>,
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
                for i in v.iter() {
                    buf += &format!("{k} = {}\n", join(i.to_vec(), ','));
                }
            }
            buf
        };
        let config_ser = {
            let confs = self.config.clone();
            let mut buf = "".to_string();
            for (k, v) in confs.iter() {
                buf += &format!("{k} = {}\n", join(v.to_vec(), ','));
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
        1 => {
            // Rgb
            let color = Color::from_rgb(
                get_val_as::<u8>(&cs, "r"),
                get_val_as::<u8>(&cs, "g"),
                get_val_as::<u8>(&cs, "b"),
            );
            color
        }
        2 => {
            // Rgb hex
            let color = Color::from_rgb(
                decode(&cs["r"]).unwrap()[0],
                decode(&cs["g"]).unwrap()[0],
                decode(&cs["b"]).unwrap()[0],
            );
            color
        }
        3 => {
            // Rgba
            let color = Color::from_rgba(
                get_val_as::<u8>(&cs, "r"),
                get_val_as::<u8>(&cs, "g"),
                get_val_as::<u8>(&cs, "b"),
                get_val_as::<u8>(&cs, "a"),
            );
            color
        }
        4 => {
            // Rgba hex
            let color = Color::from_rgba(
                decode(&cs["r"]).unwrap()[0],
                decode(&cs["g"]).unwrap()[0],
                decode(&cs["b"]).unwrap()[0],
                decode(&cs["a"]).unwrap()[0],
            );
            color
        }

        5 => {
            // Argb
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
    fn previous_rgb(index: usize, chars: Vec<char>) -> bool {
        let string: String = chars[index - 3..index].iter().collect();
        let string = string.as_str();
        if string == "gba" || string == "rgb" {
            true
        } else {
            false
        }
    }

    fn splitter<Str: ToString>(s: Str) -> Vec<String> {
        let mut is_in_rgb = false;
        let mut is_in_str = false;
        let mut last = 0;
        let chars: Vec<_> = s.to_string().chars().collect();
        let mut final_str = vec![];
        for (index, c) in chars.iter().enumerate() {
            if *c == '(' && previous_rgb(index, chars.clone()) && !is_in_str {
                is_in_rgb = true;
            } else if *c == ')' && is_in_rgb && !is_in_str {
                is_in_rgb = false;
            } else if *c == '"' && is_in_str {
                is_in_str = false;
            } else if *c == '"' && !is_in_str {
                is_in_str = true;
            } else if *c == ',' && !is_in_str && !is_in_rgb {
                final_str.push(chars[last..index].iter().collect());
                last = index + 1
            };
            //println!("{c}: rgb:{is_in_rgb} str:{is_in_str}");
        }
        final_str.push(chars[last..].iter().collect());
        final_str
    }
    splitter(s)
}

pub fn line_parse(line: &str) -> (String, Vec<Arg>) {
    use regex::RegexSet;
    lazy_static! {
      static ref TOKEN_SET: RegexSet = RegexSet::new([
        r"^( *)$", // Empty
        r"^rgb\((?P<r>\d*), *(?P<g>\d*), *(?P<b>\d*)\)$", // rgb
        r"^rgb\((?P<r>[[:xdigit:]]{2})(?P<g>[[:xdigit:]]{2})(?P<b>[[:xdigit:]]{2})\)$", // rgb hex
        r"^rgba\((?P<r>\d*), *(?P<g>\d*), *(?P<b>\d*), *(?P<a>\d*)\)$", // rgba
        r"^rgba\((?P<r>[[:xdigit:]]{2})(?P<g>[[:xdigit:]]{2})(?P<b>[[:xdigit:]]{2})(?P<a>[[:xdigit:]]{2})\)$", // rgba hex
        r"^0x(?P<a>[[:xdigit:]]{2})(?P<r>[[:xdigit:]]{2})(?P<g>[[:xdigit:]]{2})(?P<b>[[:xdigit:]]{2})$", // legacy argb
        r"^(true|false|yes|no|0|1)$", // bool
        r"^(\-?\d*\.\d*)$", // float
        r"^(?P<v1>\-?\d+\.?\d*) +(?P<v2>\-?\d+\.?\d*)$", // vec2
        r"^\-?\d*$", // Int
        r"(?:^|_| |)(?P<mod>[sS][uU][pP][eE][rR]|[aA][lL][tT]|[cC][tT][rR][lL]|[sS][hH][iI][fF][tT])(?:_| |$|)", // MOD
        r"^(?P<pre>.*?) *(?P<deg>\d*) *deg$", // gradient
        r"^\$(?P<name>.*)$", // Variable
        r#"^"?(?P<content>.*?)"?$"# // Str
      ]).expect("Error creating regex set");
      static ref TOKEN_LEN: usize = TOKEN_SET.len() - 1;
      static ref TOKEN_REGEXES: Vec<Regex> = TOKEN_SET
        .patterns()
        .iter()
        .map(|pat| Regex::new(pat).expect("error creating regex"))
        .collect();
      static ref NAMED_REGEX: Regex = Regex::new(r"^(?:(?P<name>.*?): *(?P<after>.*?)|(?P<lone>.*?))$").unwrap();
    }

    let (pre, tokens) = {
        let stuff: Vec<_> = line.split('=').map(|l| l.trim()).collect();
        (stuff[0], stuff[1])
    };
    let mut all_tokens: Vec<Arg> = vec![];
    for token in split_args(tokens.to_string()).iter() {
        let (name, token) = {
            let token = token.as_str();
            let captures = NAMED_REGEX.captures(token).unwrap();
            if let Some(name) = captures.name("name") {
                (Some(name.as_str()), captures["after"].to_string())
            } else {
                (None, captures["lone"].to_string())
            }
        };

        let matches: Vec<_> = TOKEN_SET.matches(&token).iter().collect();

        let captures = if !TOKEN_REGEXES.is_empty() && !matches.is_empty() {
            match TOKEN_REGEXES[matches[0]].captures(&token) {
                Some(captures) => captures,
                None => panic!("Regex has no captures"),
            }
        } else {
            panic!("Nothing was matched")
        };
        let is_mod = {
            let s = |st: &str| token.to_lowercase().starts_with(st);
            s("super") || s("shift") || s("alt") || s("ctrl")
        };
        let parsed = if matches[0] == 10 && is_mod {
            let mut buf = vec![];
            let matches_iter = TOKEN_REGEXES[10].captures_iter(&token);
            for i in matches_iter {
                buf.push(match i["mod"].to_lowercase().as_str() {
                    "super" => Mod::SUPER,
                    "shift" => Mod::SHIFT,
                    "ctrl" => Mod::CTRL,
                    "alt" => Mod::ALT,
                    _ => unreachable!(),
                })
            }
            Token::Mod(buf)
        } else if matches.len() >= 2 {
            match matches[0] {
                0 => Token::Empty,
                1 => Token::Color(parse_color(&captures, 1)), // Rgb
                2 => Token::Color(parse_color(&captures, 2)), // Rgb
                3 => Token::Color(parse_color(&captures, 3)), // Rgba
                4 => Token::Color(parse_color(&captures, 4)), // Rgba Hex
                5 => Token::Color(parse_color(&captures, 5)), // Argb
                6 => Token::Bool(match token.as_str() {
                    "true" | "yes" | "1" => true,
                    "false" | "no" | "0" => false,
                    _ => unreachable!(),
                }),
                7 => Token::Float(token.parse::<f64>().unwrap()),
                8 => Token::Vec2(
                    captures["v1"].parse::<f64>().unwrap(),
                    captures["v2"].parse::<f64>().unwrap(),
                ),
                9 => Token::Int(token.parse::<i64>().unwrap()),
                10 => {
                    use Mod::*;
                    Token::Mod(vec![match token.to_lowercase().as_str() {
                        "super" => SUPER,
                        "alt" => ALT,
                        "ctrl" => CTRL,
                        "shift" => SHIFT,
                        _ => unreachable!(),
                    }])
                }
                11 => {
                    let colors = {
                        let mut colors: Vec<Color> = vec![];
                        let rgbs = {
                            lazy_static! {
                                static ref REGEX: Regex =
                                    Regex::new(r"(?P<pre>rgba?\(.*?\)|0x[[:xdigit:]]{8})")
                                        .expect("error creating regex");
                            }
                            let pre = captures["pre"].to_string();
                            let mut buf = vec![];
                            for i in REGEX.captures_iter(&pre) {
                                buf.push(i["pre"].to_string())
                            }
                            buf
                        };
                        for i in rgbs {
                            if i.starts_with("rgba(") && i.contains(",") {
                                colors.push(parse_color(
                                    &TOKEN_REGEXES[3].captures(&i).expect("invalid color"),
                                    3,
                                ));
                            } else if i.starts_with("rgba(") && !i.contains(",") {
                                colors.push(parse_color(
                                    &TOKEN_REGEXES[4].captures(&i).expect("invalid color"),
                                    4,
                                ));
                            } else if i.starts_with("0x") {
                                colors.push(parse_color(
                                    &TOKEN_REGEXES[5].captures(&i).expect("invalid color"),
                                    5,
                                ));
                            } else if i.starts_with("rgba(") && !i.contains(",") {
                                colors.push(parse_color(
                                    &TOKEN_REGEXES[2].captures(&i).expect("invalid color"),
                                    2,
                                ));
                            } else {
                                colors.push(parse_color(
                                    &TOKEN_REGEXES[1].captures(&i).expect("invalid color"),
                                    1,
                                ));
                            }
                        }
                        colors
                    };
                    Token::Gradient(colors, captures["deg"].parse::<u16>().unwrap())
                }
                12 => Token::Variable(captures["name"].to_string()),
                13 => Token::Str(captures["content"].to_string()),
                _ => unreachable!(),
            }
        } else if matches.len() == 1 && matches[0] == *TOKEN_LEN {
            Token::Str(
                TOKEN_REGEXES[*TOKEN_LEN].captures(token.trim()).unwrap()["content"].to_string(),
            )
        } else {
            panic!("no matches {matches:#?}")
        };
        match name {
            Some(name) => all_tokens.push(Arg::Named(name.to_string(), parsed)),
            None => all_tokens.push(Arg::Unnamed(parsed)),
        };
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
    for (_ln, line) in lines.enumerate() {
        if line.trim().starts_with("#") {
            continue;
        } else if line.ends_with('{') {
            depth.push(line.trim().split(" {").collect::<Vec<_>>()[0]);
            continue;
        } else if line.trim().ends_with('}') {
            depth.pop();
            continue;
        } else if line.trim().starts_with('$') {
            let line = line.replace('$', "");
            let (name, tokens) = line_parse(&line);
            vars.insert(name, tokens[0].clone().into_token());
        } else {
            let (pre, _) = {
                let stuff: Vec<_> = line.split('=').map(|l| l.trim()).collect();
                (stuff[0], stuff[1])
            };
            if depth.len() == 0 && !pre.contains(":") {
                let (name, tokens) = line_parse(line);
                ast.keywords.entry(name.clone()).or_insert(vec![]);
                match &mut ast.keywords.get_mut(&name) {
                    Some(v) => v.push(tokens),
                    None => unreachable!(),
                };
                //ast.keywords.insert(name, tokens);
                continue;
            }
            let name = if depth.len() != 0 {
                join(depth.clone(), ':') + ":" + pre
            } else {
                pre.to_string()
            };
            let mut parsed = line_parse(&line).1;
            for item in parsed.iter_mut() {
                match item {
                    Arg::Named(n, Token::Variable(name)) => {
                        *item = Arg::Named(
                            n.to_string(),
                            vars.get(name).expect("Unknown variable passed in").clone(),
                        );
                    }
                    Arg::Unnamed(Token::Variable(name)) => {
                        *item = Arg::Unnamed(
                            vars.get(name).expect("Unknown variable passed in").clone(),
                        );
                    }
                    _ => {}
                }
                //if let Token::Variable(name) = item {
                //    *item = vars.get(name).expect("Unknown variable passed in").clone();
                //}
            }
            ast.config.insert(name, parsed);
        };
    }
    ast
    //AstObj(BTreeMap::new())
}
