# hyprparse (ALPHA)
Alpha Hyprland/Hyprpaper/Hypr configuration file parser

This is still in alpha so expect bugs, and if you do, please make an issue!

## how to add
```toml
hyprparse = { git = "https://github.com/hyprland-community/hyprparse" }
```

## example usage
```rust ,no_run
use hyprparse::whole_parser;

fn main() {
  static CONFIG: &str = r#"
    $test = -69
    general:test4 = $test,$test,$test
    general:test = rgb(69,69,69)
    general:test5 = rgba(FFFFFFFF),rgb(000000),"hello!",test: "hello"
    # hello
    general {
      test2 = rgba(7,8,9,100)
      test3 = rgba(7,8,9,100) 0xFFFFFFFF rgb(4,4,4) 69deg
    }
    bind=SUPER_SHIFT, Q, exit
    bind=,V,exit
  "#;
  println!("{:#?}", whole_parser(CONFIG));
  println!("{}", whole_parser(CONFIG));
}
```
