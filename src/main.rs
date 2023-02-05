use hyprparse::whole_parser;

fn main() {
    static CONFIG: &str = r#"
    $test = -69
    general:test4 = $test,$test,$test
    general:test = rgb(69,69,69)
    general:test5 = rgba(FFFFFFFF)
    # hello
    general {
      test2 = rgba(7,8,9,100)
      test3 = rgba(7,8,9,100) 0xFFFFFFFF 69deg
    }
    bind=SUPER, Q, exit
  "#;
    println!("{:#?}", whole_parser(CONFIG));
}
