//! A direct replacement for `assert_eq` for unordered collections
//!
//! This macro is useful for any situation where the ordering of the collection doesn't matter, even
//! if they are always in the same order. This is because the stdlib `assert_eq` shows the entire
//! collection for both left and right and leaves it up to the user to visually scan for differences.
//! In contrast, this crate only works with collections (types that implement `IntoIterator`) and
//! therefore can show only the differences (see below for an example of what the output looks like).
//!
//! # Example
//! ```should_panic
//! use assert_unordered::assert_eq_unordered;
//!
//! #[derive(Debug, PartialEq)]
//! struct MyType(i32);
//!
//! let expected = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
//! let actual = vec![MyType(2), MyType(0), MyType(4)];
//!
//! assert_eq_unordered!(expected, actual);
//! ```
//!
//! Output:
//!  
//! ![example_error](https://raw.githubusercontent.com/nu11ptr/assert_unordered/master/example_error.png)

#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![warn(missing_docs)]

// Trick to test README samples (from: https://github.com/rust-lang/cargo/issues/383#issuecomment-720873790)
#[cfg(doctest)]
mod test_readme {
    macro_rules! external_doc_test {
        ($x:expr) => {
            #[doc = $x]
            extern "C" {}
        };
    }

    external_doc_test!(include_str!("../README.md"));
}

extern crate alloc;

use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt::{Arguments, Debug};
#[cfg(feature = "color")]
#[cfg(windows)]
use std::sync::Once;

#[cfg(feature = "color")]
#[cfg(windows)]
static INIT_COLOR: Once = Once::new();

#[cfg(feature = "color")]
#[cfg(windows)]
static mut COLOR_ENABLED: bool = false;

/// Assert that `$left` and `$right` are "unordered" equal. That is, they contain the same elements,
/// but not necessarily in the same order. If this assertion is false, a panic is raised, and the
/// elements that are different between `$left` and `$right` are shown (when possible).
///
/// Both `$left` and `$right` must be of the same type and implement [PartialEq] and [Iterator] or
/// [IntoIterator], but otherwise can be any type. The iterator `Item` type can be any type that
/// implements [Debug] and [PartialEq]. Optional `$arg` parameters may be given to customize the
/// error message, if any (these are the same as the parameters passed to [format!]).
///
/// # Efficiency
/// If `$left` and `$right` are equal, this assertion is quite efficient just doing a regular equality
/// check and then returning. If they are not equal, `$left` and `$right` are collected into a [Vec]
/// and the elements compared one by one for both `$left` and `$right` (meaning it is at least least
/// O(n^2) algorithmic complexity in the non-equality path).
///
/// # Example
/// ```should_panic
/// use assert_unordered::assert_eq_unordered;
///
/// #[derive(Debug, PartialEq)]
/// struct MyType(i32);
///
/// let expected = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
/// let actual = vec![MyType(2), MyType(0), MyType(4)];
///
/// assert_eq_unordered!(expected, actual);
///  ```
///
/// Output:
///
/// ![example_error](https://raw.githubusercontent.com/nu11ptr/assert_unordered/master/example_error.png)

#[macro_export]
macro_rules! assert_eq_unordered {
    ($left:expr, $right:expr $(,)?) => {
        $crate::pass_or_panic($crate::compare_unordered($left, $right), core::option::Option::None);
    };
    ($left:expr, $right:expr, $($arg:tt)+) => {
        $crate::pass_or_panic(
            $crate::compare_unordered($left, $right),
            core::option::Option::Some(core::format_args!($($arg)+))
        );
    };
}

#[cfg(feature = "color")]
#[cfg(windows)]
#[inline]
fn init_color() -> bool {
    // SAFETY: This is the example given in stdlib docs for how to init a mutable static var
    unsafe {
        INIT_COLOR.call_once(|| {
            COLOR_ENABLED = ansi_term::enable_ansi_support().is_ok();
        });
        COLOR_ENABLED
    }
}

#[cfg(feature = "color")]
#[cfg(not(windows))]
#[inline]
const fn init_color() -> bool {
    true
}

#[doc(hidden)]
pub enum CompareResult {
    Equal,
    NotEqualDiffElements(String, String, String),
}

#[cfg(feature = "color")]
#[doc(hidden)]
#[inline]
pub fn pass_or_panic(result: CompareResult, msg: Option<Arguments>) {
    if init_color() {
        color_pass_or_panic(result, msg)
    } else {
        plain_pass_or_panic(result, msg);
    }
}

#[cfg(not(feature = "color"))]
#[doc(hidden)]
#[inline]
pub fn pass_or_panic(result: CompareResult, msg: Option<Arguments>) {
    plain_pass_or_panic(result, msg);
}

#[cfg(feature = "color")]
fn color_pass_or_panic(result: CompareResult, msg: Option<Arguments>) {
    match result {
        CompareResult::NotEqualDiffElements(in_both, in_left_not_right, in_right_not_left) => {
            use ansi_term::Color::{Green, Red, Yellow};

            let msg = match msg {
                Some(msg) => msg.to_string(),
                None => {
                    format!(
                        "The {} did not contain the {} as the {}",
                        Red.paint("left"),
                        Yellow.paint("same items"),
                        Green.paint("right"),
                    )
                }
            };

            let both = Yellow.paint(format!("In both: {in_both}"));
            let left = Red.paint(format!("In left: {in_left_not_right}"));
            let right = Green.paint(format!("In right: {in_right_not_left}"));

            panic!("{msg}:\n{both}\n{left}\n{right}\n");
        }
        CompareResult::Equal => {}
    }
}

fn plain_pass_or_panic(result: CompareResult, msg: Option<Arguments>) {
    match result {
        CompareResult::NotEqualDiffElements(in_both, in_left_not_right, in_right_not_left) => {
            let msg = match msg {
                Some(msg) => msg,
                // TODO: 1.60 `format_args` not yet stable on 'const fn'. Maybe soon?
                None => format_args!("The left did not contain the same items as the right"),
            };

            panic!(
                "{msg}:\nIn both: {in_both}\nIn left: {in_left_not_right}\nIn right: {in_right_not_left}"
            );
        }
        CompareResult::Equal => {}
    }
}

#[doc(hidden)]
pub fn compare_unordered<I, T>(left: I, right: I) -> CompareResult
where
    I: IntoIterator<Item = T> + PartialEq,
    T: Debug + PartialEq,
{
    // First, try for the easy (and faster compare)
    if left != right {
        // Fallback on the slow unordered path
        let mut in_right_not_left: Vec<_> = right.into_iter().collect();
        let mut in_left_not_right = Vec::new();
        // Optimistically assume we likely got it close to right
        let mut in_both = Vec::with_capacity(in_right_not_left.len());

        for elem1 in left {
            match in_right_not_left.iter().position(|elem2| &elem1 == elem2) {
                Some(idx) => {
                    in_both.push(elem1);
                    in_right_not_left.remove(idx);
                }
                None => {
                    in_left_not_right.push(elem1);
                }
            }
        }

        if !in_left_not_right.is_empty() || !in_right_not_left.is_empty() {
            CompareResult::NotEqualDiffElements(
                format!("{in_both:#?}"),
                format!("{in_left_not_right:#?}"),
                format!("{in_right_not_left:#?}"),
            )
        } else {
            CompareResult::Equal
        }
    } else {
        CompareResult::Equal
    }
}

#[cfg(test)]
mod tests {
    use crate::{compare_unordered, CompareResult};
    use alloc::vec::Vec;
    use alloc::{format, vec};

    #[derive(Debug, PartialEq)]
    struct MyType(i32);

    fn validate_results(
        result: CompareResult,
        both_expected: Vec<MyType>,
        left_expected: Vec<MyType>,
        right_expected: Vec<MyType>,
    ) {
        match result {
            CompareResult::NotEqualDiffElements(both_actual, left_actual, right_actual) => {
                assert_eq!(format!("{both_expected:#?}"), both_actual);
                assert_eq!(format!("{left_expected:#?}"), left_actual);
                assert_eq!(format!("{right_expected:#?}"), right_actual);
            }
            _ => {
                panic!("Left and right were expected to have have different elements");
            }
        }
    }

    #[test]
    fn compare_unordered_not_equal_diff_elem() {
        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(2), MyType(0), MyType(4)];

        validate_results(
            compare_unordered(left, right),
            vec![MyType(2), MyType(4)],
            vec![MyType(1), MyType(5)],
            vec![MyType(0)],
        );
    }

    #[test]
    fn compare_unordered_not_equal_dup_elem_diff_len() {
        let left = vec![MyType(2), MyType(4), MyType(4)];
        let right = vec![MyType(4), MyType(2)];

        validate_results(
            compare_unordered(left, right),
            vec![MyType(2), MyType(4)],
            vec![MyType(4)],
            vec![],
        );
    }

    #[test]
    fn compare_unordered_not_equal_dup_elem() {
        let left = vec![MyType(2), MyType(2), MyType(2), MyType(4)];
        let right = vec![MyType(2), MyType(4), MyType(4), MyType(4)];

        validate_results(
            compare_unordered(left, right),
            vec![MyType(2), MyType(4)],
            vec![MyType(2), MyType(2)],
            vec![MyType(4), MyType(4)],
        );
    }

    #[test]
    fn compare_unordered_equal_diff_order() {
        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(5), MyType(2), MyType(1), MyType(4)];

        assert!(matches!(
            compare_unordered(left, right),
            CompareResult::Equal
        ));
    }

    #[test]
    fn compare_unordered_equal_same_order() {
        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(1), MyType(2), MyType(4), MyType(5)];

        assert!(matches!(
            compare_unordered(left, right),
            CompareResult::Equal
        ));
    }
}
