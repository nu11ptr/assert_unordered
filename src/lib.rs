#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![warn(missing_docs)]

//! A direct replacement for `assert_eq` for unordered collections
//!
//! These macros are useful for any situation where the ordering of the collection doesn't matter, even
//! if they are always in the same order. This is because the stdlib `assert_eq` shows the entire
//! collection for both left and right and leaves it up to the user to visually scan for differences.
//! In contrast, this crate only works with collections (types that implement `IntoIterator`) and
//! therefore shows only the differences (see below for an example of what the output looks like).
//!
//! Both [assert_eq_unordered] and [assert_eq_unordered_set] perform the same function, but with
//! different levels of efficiency on inequality. For large collections, or when the more stringent
//! trait requirements can be met, [assert_eq_unordered_set] should probably be preferred. However,
//! [assert_eq_unordered] only requires [PartialEq] and [Debug] on its elements, which is handy for
//! types with very few trait implementations.
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
//! ```text
//! thread 'tests::test' panicked at 'The left did not contain the same items as the right:
//! In left, not in right: "[MyType(1), MyType(5)]"
//! In right, not in left: "[MyType(0)]"'
//! ```

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
#[cfg(feature = "std")]
use core::hash::Hash;
#[cfg(feature = "std")]
use std::collections::HashSet;

/// Assert that `$left` and `$right` are "unordered" equal. That is, they contain the same elements,
/// but not necessarily in the same order. If this assertion is false, a panic is raised, and the
/// elements that are different between `$left` and `$right` are shown.
///
/// Both `$left` and `$right` must be of the same type and implement [PartialEq] and [Iterator] or
/// [IntoIterator], but otherwise can be any type. The iterator `Item` type can be any type that
/// implements [Debug], [Eq], and [Hash]. Optional `$arg` parameters may be given to customize the
/// error message, if any (these are the same as the parameters passed to [format!]).
///
/// # Efficiency
/// If `$left` and `$right` are equal, this assertion is quite efficient just doing a regular equality
/// check and then returning. If they are not equal, `$left` and `$right` are collected into [HashSet]s
/// and the difference between them is returned.
///
/// While benchmarks have not been done, in general, it would be assumed this macro should be preferred
/// over [assert_eq_unordered!] typically when the iterator `Item` trait implementations can be met, as
/// the other has an algorithm complexity of at least O(n^2 * 2) in the non-equality path.
///
/// # Example
/// ```should_panic
/// use assert_unordered::assert_eq_unordered_set;
///
/// #[derive(Debug, Eq, Hash, PartialEq)]
/// struct MyType(i32);
///
/// let expected = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
/// let actual = vec![MyType(2), MyType(0), MyType(4)];
///
/// assert_eq_unordered_set!(expected, actual);
///  
/// // Output:
/// //
/// // thread 'tests::test' panicked at 'The left did not contain the same items as the right:
/// // In left, not in right: "{MyType(1), MyType(5)}"
/// // In right, not in left: "{MyType(0)}"'
/// ```
#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
#[macro_export]
macro_rules! assert_eq_unordered_set {
    ($left:expr, $right:expr $(,)?) => {
        $crate::pass_or_panic($crate::compare_unordered_set($left, $right), core::option::Option::None);
    };
    ($left:expr, $right:expr, $($arg:tt)+) => {
        $crate::pass_or_panic(
            $crate::compare_unordered_set($left, $right),
            core::option::Option::Some(core::format_args!($($arg)+))
        );
    };
}

/// Assert that `$left` and `$right` are "unordered" equal. That is, they contain the same elements,
/// but not necessarily in the same order. If this assertion is false, a panic is raised, and the
/// elements that are different between `$left` and `$right` are shown.
///
/// Both `$left` and `$right` must be of the same type and implement [PartialEq] and [Iterator] or
/// [IntoIterator], but otherwise can be any type. The iterator `Item` type can be any type that
/// implements [Debug] and [PartialEq]. Optional `$arg` parameters may be given to customize the
/// error message, if any (these are the same as the parameters passed to [format!]).
///
/// # Efficiency
/// If `$left` and `$right` are equal, this assertion is quite efficient just doing a regular equality
/// check and then returning. If they are not equal, `$left` and `$right` are collected into a [Vec]
/// and the elements compared one by one for both `$left` and `$right`.
///
/// While benchmarks have not been done, in general, it would be assumed that [assert_eq_unordered_set!]
/// should be preferred when the iterator item's more stringent traits can be met. This macro has
/// an algorithm complexity of at least O(n^2 * 2) in the non-equality path, so is fine for small
/// datasets in tests, but does not scale well.
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
///  
/// // Output:
/// //
/// // thread 'tests::test' panicked at 'The left did not contain the same items as the right:
/// // In left, not in right: "[MyType(1), MyType(5)]"
/// // In right, not in left: "[MyType(0)]"'
/// ```
///
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

#[doc(hidden)]
pub enum CompareResult {
    Equal,
    NotEqualDiffElements(String, String),
    NotEqualDupElemDiffLen(String, String),
}

#[doc(hidden)]
pub fn pass_or_panic(result: CompareResult, msg: Option<Arguments>) {
    match result {
        CompareResult::NotEqualDiffElements(in_left_not_right, in_right_not_left) => {
            let msg = match msg {
                Some(msg) => msg,
                // TODO: 1.60 `format_args` not yet stable on 'const fn'. Maybe soon?
                None => format_args!("The left did not contain the same items as the right"),
            };

            panic!("{msg}:\nIn left, not in right: {in_left_not_right:?}\nIn right, not in left: {in_right_not_left:?}");
        }
        CompareResult::NotEqualDupElemDiffLen(left, right) => {
            let msg = match msg {
                Some(msg) => msg,
                // TODO: 1.60 `format_args` not yet stable on 'const fn'. Maybe soon?
                None => format_args!("The left is not the same length as the right, and there are duplicate elements"),
            };

            panic!("{msg}:\nLeft: {left:?}\nRight: {right:?}");
        }
        CompareResult::Equal => {}
    }
}

#[cfg(feature = "std")]
#[doc(hidden)]
pub fn compare_unordered_set<I, T>(left: I, right: I) -> CompareResult
where
    I: IntoIterator<Item = T> + PartialEq,
    T: Debug + Eq + Hash,
{
    // First, try for the easy (and faster compare)
    if left != right {
        // Fallback on the slow difference path
        let left_set: HashSet<_> = left.into_iter().collect();
        let right_set: HashSet<_> = right.into_iter().collect();

        if left_set != right_set {
            let in_left_not_right: HashSet<_> = left_set.difference(&right_set).collect();
            let in_right_not_left: HashSet<_> = right_set.difference(&left_set).collect();
            CompareResult::NotEqualDiffElements(
                format!("{in_left_not_right:?}"),
                format!("{in_right_not_left:?}"),
            )
        } else {
            CompareResult::Equal
        }
    } else {
        CompareResult::Equal
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
        let left: Vec<_> = left.into_iter().collect();
        let right: Vec<_> = right.into_iter().collect();

        let in_left_not_right: Vec<_> = left.iter().filter(|&t| !right.contains(t)).collect();
        let in_right_not_left: Vec<_> = right.iter().filter(|&t| !left.contains(t)).collect();

        if !in_left_not_right.is_empty() || !in_right_not_left.is_empty() {
            CompareResult::NotEqualDiffElements(
                format!("{in_left_not_right:?}"),
                format!("{in_right_not_left:?}"),
            )
        } else if left.len() != right.len() {
            CompareResult::NotEqualDupElemDiffLen(format!("{left:?}"), format!("{right:?}"))
        } else {
            CompareResult::Equal
        }
    } else {
        CompareResult::Equal
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "std")]
    use crate::compare_unordered_set;
    use crate::{compare_unordered, CompareResult};
    use alloc::vec;

    #[test]
    fn compare_unordered_not_equal_diff_elem() {
        #[derive(Debug, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(2), MyType(0), MyType(4)];

        match compare_unordered(left, right) {
            CompareResult::NotEqualDiffElements(left_actual, right_actual) => {
                let left_expected = "[MyType(1), MyType(5)]";
                let right_expected = "[MyType(0)]";
                assert_eq!(left_expected, left_actual);
                assert_eq!(right_expected, right_actual);
            }
            _ => {
                panic!("Left and right were expected to have have different elements");
            }
        }
    }

    #[test]
    fn compare_unordered_not_equal_dup_elem() {
        #[derive(Debug, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(2), MyType(4), MyType(4)];
        let right = vec![MyType(4), MyType(2)];

        match compare_unordered(left, right) {
            CompareResult::NotEqualDupElemDiffLen(left_actual, right_actual) => {
                let left_expected = "[MyType(2), MyType(4), MyType(4)]";
                let right_expected = "[MyType(4), MyType(2)]";
                assert_eq!(left_expected, left_actual);
                assert_eq!(right_expected, right_actual);
            }
            _ => {
                panic!("Left and right were expected to have have different elements");
            }
        }
    }

    // Side effect of not having Ord and being able to sort
    #[test]
    fn compare_unordered_equal_dup_elem() {
        #[derive(Debug, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(2), MyType(2), MyType(2), MyType(4)];
        let right = vec![MyType(2), MyType(4), MyType(4), MyType(4)];

        assert!(matches!(
            compare_unordered(left, right),
            CompareResult::Equal
        ));
    }

    #[test]
    fn compare_unordered_equal() {
        #[derive(Debug, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(5), MyType(2), MyType(1), MyType(4)];

        assert!(matches!(
            compare_unordered(left, right),
            CompareResult::Equal
        ));
    }

    #[cfg(feature = "std")]
    #[test]
    fn compare_unordered_set_not_equal() {
        #[derive(Debug, Eq, Hash, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4)];
        let right = vec![MyType(2), MyType(0), MyType(4)];

        match compare_unordered_set(left, right) {
            CompareResult::NotEqualDiffElements(left_actual, right_actual) => {
                let left_expected = "{MyType(1)}";
                let right_expected = "{MyType(0)}";
                assert_eq!(left_expected, left_actual);
                assert_eq!(right_expected, right_actual);
            }
            _ => {
                panic!("Left and right were expected to have have different elements");
            }
        }
    }

    #[cfg(feature = "std")]
    #[test]
    fn compare_unordered_set_equal() {
        #[derive(Debug, Eq, Hash, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(5), MyType(2), MyType(1), MyType(4)];

        assert!(matches!(
            compare_unordered_set(left, right),
            CompareResult::Equal
        ));
    }

    // Side effect of using a set
    #[cfg(feature = "std")]
    #[test]
    fn compare_unordered_set_equal_dup_elem() {
        #[derive(Debug, Eq, Hash, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(2), MyType(4), MyType(4)];
        let right = vec![MyType(4), MyType(2)];

        assert!(matches!(
            compare_unordered_set(left, right),
            CompareResult::Equal
        ));
    }
}
