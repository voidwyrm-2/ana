# ANA

ANA ("ANA (is) Not (an) Acronym") is a functional pseudo-array language based on Erlang, Lua, and Uiua.

## Examples

```
require "std/io".

hello ← "Meow!".

io→Outln [hello].
```

```
require "std/io".

Fib ← λ (n) {
    if (n < 1) {
        return 0.
    } else if (n = 1) {
        return 1.
    }

    return Fib [n - 1] + Fib [n - 2].
}.

result ← Fib [10].

io→Outln [result].
```

## Installation

At the moment, the only options are either download it via Cargo, or clone via Git and build it manually.

### Via Cargo

Simply run `cargo install --git https://github.com/voidwyrm-2/ana`

### Via Git

```
git clone https://github.com/voidwyrm-2/ana
cd ana
cargo build --release
./target/release/ana examples/hello.ana
```

## Future Development

For an idea of what's planned, read [todo.md](./todo.md).
