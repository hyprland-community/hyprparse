use hyprparse::whole_parser;

fn main() {
    static CONFIG: &str = r#"
    $test = -69
    general:test4 = $test,$test,$test
    general:test = rgb(69,69,69)
    general:test5 = rgba(FFFFFFFF),rgb(000000),"hello!",test: "hello",test2: rgb(4,4,3)
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
