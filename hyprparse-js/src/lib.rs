use hyprparse::*;
use wasm_bindgen::prelude::*;
extern crate console_error_panic_hook;
use std::panic;

#[wasm_bindgen(start)]
pub fn initialize() {
    panic::set_hook(Box::new(console_error_panic_hook::hook));
}

#[wasm_bindgen]
pub fn parse(conf: &str) -> JsValue {
    serde_wasm_bindgen::to_value(&whole_parser(conf)).unwrap()
}

#[wasm_bindgen]
pub fn stringify(conf: JsValue) -> String {
    serde_wasm_bindgen::from_value::<AstRoot>(conf)
        .unwrap()
        .to_string()
}
