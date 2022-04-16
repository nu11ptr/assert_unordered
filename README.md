# assert_unordered

[![Crate](https://img.shields.io/crates/v/assert_unordered)](https://crates.io/crates/assert_unordered)
[![Docs](https://docs.rs/assert_unordered/badge.svg)](https://docs.rs/assert_unordered)

A direct replacement for `assert_eq` for unordered collections

This macro is useful for any situation where the ordering of the collection doesn't matter, even
if they are always in the same order. This is because the stdlib `assert_eq` shows the entire
collection for both left and right and leaves it up to the user to visually scan for differences.
In contrast, this crate only works with collections (types that implement `IntoIterator`) and
therefore can show only the differences (see below for an example of what the output looks like).

## Usage

```toml
[dev-dependencies]
assert_unordered = "0.3"
```

## Example
```rust, should_panic
use assert_unordered::assert_eq_unordered;

#[derive(Debug, PartialEq)]
struct MyType(i32);

fn main() {
    let expected = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
    let actual = vec![MyType(2), MyType(0), MyType(4)];

    assert_eq_unordered!(expected, actual);
}
```

Output:
```text
thread 'tests::test' panicked at 'The left did not contain the same items as the right:
In both: "[MyType(2), MyType(4)]"
In left: "[MyType(1), MyType(5)]"
In right: "[MyType(0)]"'
```

## License

This project is licensed optionally under either:

* Apache License, Version 2.0, (LICENSE-APACHE
  or https://www.apache.org/licenses/LICENSE-2.0)
* MIT license (LICENSE-MIT or https://opensource.org/licenses/MIT)
