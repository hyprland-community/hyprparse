/*
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
}*/

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

//#[derive(Debug)]
//pub enum AstObj<T, D> {
//    Token(T),
//    Tree(D),
//}
