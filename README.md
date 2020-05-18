# Re:Wasm

`rewasm` is a decompiler for WebAssembly binaries.
It can decompile all WASM binaries from the MVP version 1,
however it's still under development so some features,
like proper type recovery, are still missing and some
binaries still produce pretty unreadable output.

## Requirements

Running `rewasm` requires [libz3](https://github.com/Z3Prover/z3) (version
`4.8.6` or `4.8.7` should work).

## Installation

You can find prebuilt binaries for 64-bit Linux [here](https://github.com/benediktwerner/rewasm/releases).

### Building from source

Building or installing `rewasm` from source requires a working [Rust Installation](https://www.rust-lang.org/)
(probably at least version `1.37.0`).

To build and install `rewasm` (this will place the
binary in `~/.cargo/bin` which should be in your `$PATH`):

```
$ git clone https://github.com/benediktwerner/rewasm
$ cargo install --path rewasm
$ rewasm --version
```

To just build `rewasm` from source:
```
$ git clone https://github.com/benediktwerner/rewasm
$ cd rewasm
$ cargo build --release
$ ./target/release/rewasm --version
```

## Usage

Decompile whole file:

```
$ rewasm example.wasm > example.dec
```

Decompile a single function (with index `42`):

```
$ rewasm example.wasm 42
```

## Example

Convert [`examples/loop.wat`](examples/loop.wat) to a `.wasm` file using [WABT's `wat2wasm`](https://github.com/WebAssembly/wabt) and decompile it:
```
$ wat2wasm examples/loop.wat
$ rewasm loop.wasm
```

Output:
```rust
fn func_0(arg_0: i32, arg_1: i32) -> i32 {
    let var_2: i32;
    let var_3: i32;

    var_2 = 0;
    while arg_0 >s 0 {
        var_3 = arg_1;
        while var_3 >s 0 {
            var_2 += 1;
            var_3 += -1;
        }
        arg_0 += -1;
    }
    return var_2;
}
```
