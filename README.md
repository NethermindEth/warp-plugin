# Warp plugin

A plugin for the Cairo compiler that adds support for warp output.
The compiler will first run the warp plugin, and then the starknet plugin.

## Features

### Warplib

The plugin automatically loads the warplib to the project, provided that the path specified in `Scarb.toml`.

### Implicits arguments

You can use custom attributes to pass implicit arguments to functions.
For now, only `warp_memory` is supported.

```rust
mod Module{
    #[implicit(warp_memory)]
    fn read_from_memory(index: felt252) {
        warp_memory.read_u128(index);
    }
}
```

Generates the following code:

```rust
mod Module {
    use warplib::memory::WarpMemoryTrait;

    fn read_from_memory(ref warp_memory: Felt252Dict<u128>, index: felt252) {
        warp_memory.read_u128(index);
    }
}
```
