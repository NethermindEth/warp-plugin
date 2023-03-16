# warp-plugin

Cairo language plugin for compiling the Warp output to Starknet contracts.

## Testing

Expected test outputs are defined in `crates/warp-plugin/src/plugin_test_data`.

To run the tests, run:

```
cargo test --package warp-plugin --lib -- plugin::test::expand_contract --nocapture
```

To regenerate, set `CAIRO_FIX_TESTS=1`.
