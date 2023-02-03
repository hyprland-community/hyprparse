pub enum Mod {
    SUPER,
    SHIFT,
    CTRL,
    ALT,
}

pub enum Color {
    Rgb(u8, u8, u8),
    RgbA(u8, u8, u8, u8),
}

pub struct Gradient {
    colors: Vec<Color>,
    angle: u16,
}

pub enum Value {
    Str(String),
    Int(i64),
    Bool(bool),
    Float(f64),
    Vec2(f64, f64),
    Mod(Mod),
    Color(Color),
    Gradient(Gradient),
    Var(String),
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Value::Str(v.to_owned())
    }
}

type ValueMap = BTreeMap<String, Value>;

impl Value {
    fn read_token<Str: ToString>(v: Str, vars: ValueMap) -> Self {
        let v = v.to_string();
        if let Ok(f) = v.parse::<f64>() {
            Self::Float(f)
        } else if let Ok(int) = v.parse::<i64>() {
            Self::Int(int)
        } else if v.starts_with("rgba") {
            let mut buf = v.split('(').collect::<Vec<_>>()[1];
            buf = &buf[..buf.len() - 1];
            let vals: Vec<_> = buf
                .split(',')
                .map(|v: &str| v.parse::<u8>().expect("Error when parsing rgba"))
                .collect();
            Self::Color(Color::RgbA(vals[0], vals[1], vals[2], vals[3]))
        } else {
            Self::Str(v)
        }
    }
}

use std::collections::BTreeMap;

pub fn parse(conf: String) -> ValueMap {
    let mut buf = BTreeMap::new();
    let mut vars: BTreeMap<String, Value> = BTreeMap::new();
    let mut depth: Vec<String> = vec![];
    for i in conf.lines() {
        let line = i.trim();
        if line.ends_with('{') {
            depth.push(i.replace(" {", ""))
        } else if line == "}" {
            depth.pop();
        } else if line.starts_with('$') {
            vars.insert(
                line.replace('$', ""),
                Value::read_token(line.split('=').collect::<Vec<_>>()[1], vars),
            );
        }
    }
    buf
}
