# Warp plugin

A modified version of `Scarb` which adds a new custom compiler that allows the use of custom implicits.

## Features

The plugin allows the use of attributes to add parameters to functions that work the same as Cairo 0 custom implicits. For example:

```rust
mod Module{
    fn init_x() {
        let mut x = 100;
        increase_x(50);
        assert(x == 150, 'Error');
    }

    #[implicit(x: felt252)]
    fn increase_x(value: felt252) {
        x = x + value;
    }
}
```

Generates the following code:

```rust
mod Module {
    fn init_x() {
        let mut x = 100;
        increase_x(ref x, 50);
        assert(x == 150, 'Error');
    }

    fn increase_x(ref x: felt252, value: felt252) {
        x = x + value;
    }
}
```

Two important things to notice here:

- In the `init_x` function, the call to `increase_x` gained `x` as an argument

- The `increase_x` function gained an extra `x` parameter.

> **Note** It is important that variable name remains similar in order to automatically infer the implicits, just like `x` in the previous example.

## Installation

First clone the project in your local machine:

```bash
git clone https://github.com/NethermindEth/warp-plugin
```

Then enter the folder and compile using Cargo:

```bash
cd warp-plugin
cargo build --all --release
```

The building process can take a few minutes.

## Use

`warp` comes with three compilers:

- A Lib compiler, provided by Scarb

- The normal Starknet compiler

- Modified Starknet compiler which executes the plugin

In order to use the one with the plugin you should add the following line to Scarb.toml:

```toml
[[target.warp]]
```

Once the Scarb.toml is modified, from the `warp-plugin` folder execute the compiler using the following command:

```bash
target/release/warp build path/to/scarb/project
```
