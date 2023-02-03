use hyprparse::{line_parse, whole_parser};

fn main() {
    static CONFIG: &str = r#"
    general:test = rgb(69,69,69)
    # hello
    general {
      test2 = rgba(7,8,9,100)
      test2 = rgba(7,8,9,100) 0xFFFFFFFF 69deg
    }
    bind=SUPER, Q, exit
  "#;
    println!("{:#?}", whole_parser(CONFIG));
    static LINES: &[&str] = &[
        "test1 = rgb(5,5,5)",
        "test2 = rgba(5,5,5,5)",
        "test3 = 0xFFFFFFFF",
        "test4 = true",
        "test5 = 6.2",
        "test6 = 6.2 6.2",
        "tetst7 = 100,-200",
        "test7 = rgb(9,9,9) rgb(9,9,9) 50deg",
        "test8 = 5,6,8",
    ];
    for i in LINES.iter() {
        let line = line_parse(i);
        println!(
            "{} = {}",
            line.0,
            line.1
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<_>>()
                .join(",")
        );
    }
}
