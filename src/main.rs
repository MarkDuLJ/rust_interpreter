use std::{env::args, process::exit};

fn main() {
   let args:Vec<String> = args().collect();
   println!("arguments: {:?}", args);

   if args.len() > 1 {
    println!("Usage: lox-ast [script]");
    exit(64);
   }else if args.len() == 1 {
       run_file(&args[0]);
   }else {
       run_prompt();
   }
}

fn run_file(_: &String){}
fn run_prompt(){}
