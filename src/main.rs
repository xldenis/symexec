use lang::Term;

mod lang;


#[derive(Debug, Clone)]
struct State {
    path: Vec<Term>,
    heap: Vec<()>,
    store: (),
}

fn main() {
    println!("Hello, world!");
}
