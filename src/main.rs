mod tokenizer;
mod parser;
mod generation;
mod arena;

use crate::tokenizer::*;
use crate::parser::*;
use crate::generation::*;

use std::process;
use std::str;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::process::Command;
use std::env;

fn main() {
    let build_dir = check_args();
    compile(&build_dir);
}
fn check_args() -> String {
    let args = env::args().collect::<Vec<_>>();
    if args.len() == 2 {
        return args[1].clone();
    } else {
        eprintln!("Incorrect usage.");
        eprintln!("Correct usage: cargo run -- <build_dir>");
        process::exit(-1);
    }
}
fn compile(build_dir: &str) {
    let input_file = fs::read_to_string(format!("main.nk")).unwrap();

    let mut tokenizer = Tokenizer::new(input_file);
    let tokens = tokenizer.tokenize().unwrap();
    
    let mut parser = Parser::new(tokens);  
    let tree = parser.parse_prog().unwrap();
    
    let mut generator = Generator::new(tree);
    generator.gen_prog().unwrap();

    let mut output_file = File::create(format!("{build_dir}/out.asm")).unwrap();
    output_file.write(generator.output.as_bytes()).unwrap();

    assemble_file(build_dir);
}
fn assemble_file(build_dir: &str) {
    let assembly_cmd = Command::new("nasm")
        .arg("-f")
        .arg("elf64")
        .arg("-o")
        .arg(format!("{build_dir}/out.o"))
        .arg(format!("{build_dir}/out.asm"))
        .output()
        .unwrap();
    if !assembly_cmd.status.success() {
        println!("{}", str::from_utf8(&assembly_cmd.stderr).unwrap());
    } 
    let linker_cmd = Command::new("ld")
        .arg("-o")
        .arg(format!("{build_dir}/out"))
        .arg(format!("{build_dir}/out.o"))
        .output()
        .unwrap();
    if !linker_cmd.status.success() {
        println!("{}", str::from_utf8(&linker_cmd.stderr).unwrap());
    }
}


