use snafu::Snafu;

pub type Result<T, E = CompilerError> = std::result::Result<T, E>;

#[derive(Debug, Snafu)]
pub enum CompilerError {
    #[snafu(display("Syntax Error\n{}", message))]
    Syntax { message: String },
}
