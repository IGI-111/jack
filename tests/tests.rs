use jack_lang::compile_and_run;

#[test]
fn fib_test() {
    assert_eq!(compile_and_run(include_str!("fib.ml")).unwrap(), 1);
}

#[test]
fn comments_test() {
    assert_eq!(compile_and_run(include_str!("comments.ml")).unwrap(), 0);
}

#[test]
fn float_test() {
    assert_eq!(compile_and_run(include_str!("float.ml")).unwrap(), 0);
}

#[test]
fn empty_test() {
    assert!(compile_and_run("").is_err());
}
