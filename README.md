# Re:Wasm

`rewasm` is a decompiler for WebAssembly binaries written in Rust.

## Building and Installation

Building or installing `rewasm` requires a working [Rust Installation](https://www.rust-lang.org/).

To install `rewasm`:

```
$ git clone https://github.com/benediktwerner/rewasm
$ cargo install --path rewasm
$ rewasm --version
```

To build `rewasm` from source:
```
$ git clone https://github.com/benediktwerner/rewasm
$ cd rewasm
$ cargo build --release
$ ./target/release/rewasm --version
```

