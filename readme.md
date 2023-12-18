# Bugs

 - Expression `()` causes infinite recursion
 - CallExpr does not parse `thing()`


## Perf Stats

### Build Times

```sh
cargo clean; cargo build --release;
```

| Date       | Debug  | Release |
| ---------- | ------ | ------- |
| 12/18/2023 | 13.47s | 48.63s  |
