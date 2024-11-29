use std::{fs, path::PathBuf};

use clap::{Parser, Subcommand};
use miette::{Context, IntoDiagnostic};


#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}
  

fn main() -> miette::Result<()> {
    let args = Args::parse();

    match args.command {
        Commands::Tokenize { filename } => {
            let mut any_cc_err = false;

            let file_contents = fs::read_to_string(&filename)
                  .into_diagnostic()
                  .wrap_err_with(|| format!("Reading '{}' failed.", filename.display()))?;

            for token in rust_interpreter::Lexer::new(&file_contents){
                
            }
        },
        Commands::Parse { filename } => todo!(),
        Commands::Run { filename } => todo!(),
    }

    Ok(())
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize{filename: PathBuf},
    Parse{ filename: PathBuf},
    Run{filename: PathBuf}
}