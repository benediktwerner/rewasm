# Re:Wasm

`rewasm` is a decompiler for WebAssembly binaries.
It can decompile all WASM binaries from the MVP version 1,
however it's still under development so some features,
like proper type recovery, are still missing and some
binaries still produce pretty unreadable output.

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

## Example Usage

Convert [`examples/loop.wat`](examples/loop.wat) to a `.wasm` file using [WABT's `wat2wasm`](https://github.com/WebAssembly/wabt) and decompile it:
```
$ wat2wasm examples/loop.wat
$ rewasm loop.wasm
```

Output:
```rust
fn func_0(i32 arg_0, i32 arg_1) -> i32 {
    i32 var_2;
    i32 var_3;
    i32 var_4;

    var_2 = 0;
    while var_4 >s 0 {
        var_3 = arg_1;
        while var_3 >s 0 {
            var_2 += 1;
            var_3 += -1;
        }
        var_4 = arg_0 + -1;
    }
    return var_2;
}
```
