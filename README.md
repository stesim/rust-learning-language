# Rust Learning Language

[![license](https://img.shields.io/github/license/stesim/rust-learning-language.svg)](LICENSE)
[![standard-readme compliant](https://img.shields.io/badge/readme%20style-standard-brightgreen.svg?style=flat-square)](https://github.com/RichardLitt/standard-readme)

_Rust Learning Language (rll)_ is an interpreted toy language with a REPL.
The language has a C-like syntax, is dynamically typed and has hardly any features.

## Background

After going through [the Rust book](https://doc.rust-lang.org/book/), I needed a project to get familiar with Rust in practice.
This is said project.

## Install

```
git clone https://github.com/stesim/rust-learning-language
```

## Usage

Run without any arguments to launch a REPL environment.

```
$ cargo run
rll> func greet(name) { print("Hello, " + name + "!"); }
rll> let who = "world"
rll> greet(who)
Hello, world!
rll> 2+4*10
42
rll> exit
```

A single source file will be evaluated when provided as command line argument.

```
$ cargo run -- example.rll
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.07s
     Running `target/debug/rust_learning_language example.rll`
0
42
a = 3
b = 4
a² + b² = 25
Hello, world!
```

## License

[MIT © stesim](LICENSE)
