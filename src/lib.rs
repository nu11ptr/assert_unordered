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
//! [assert_eq_unordered], [assert_eq_unordered_sort], and [assert_eq_unordered_set] perform the same
//! function, but with different trait requirements, efficiency, and limitations. For large collections,
//! and when the more stringent trait requirements can be met, [assert_eq_unordered_set] might make sense.
//! However, [assert_eq_unordered] only requires [PartialEq] and [Debug] on its elements, which is handy for
//! types with very few trait implementations. Lastly, [assert_eq_unordered_sort] has no limitations,
//! but has more stringent trait requirements and can still be inefficient. More details on each,
//! their exact requirements and limitations can be found in their specific documentation sections.
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
/// elements that are different between `$left` and `$right` are shown (when possible).
///
/// Both `$left` and `$right` must be of the same type and implement [PartialEq] and [Iterator] or
/// [IntoIterator], but otherwise can be any type. The iterator `Item` type can be any type that
/// implements [Debug], [Eq], [Ord], [PartialEq], and [PartialOrd]. Optional `$arg` parameters may be given
/// to customize the error message, if any (these are the same as the parameters passed to [format!]).
///
/// # Efficiency
/// If `$left` and `$right` are equal, this assertion is quite efficient just doing a regular equality
/// check and then returning. If they are not equal, `$left` and `$right` are sorted and compared again.
/// If still unequal, they are compared on an element by element basis (meaning it is at least
/// O(n^2 * 2) algorithmic complexity in the non-equality path)
///
/// # Which macro?
///
/// Which macro to use largely depends on the use case. This macro is the most robust at finding all
/// known differences between the collections, but is not very efficient. It also requires [Ord]
/// implemented on its elements.
///
/// # Example
/// ```should_panic
/// use assert_unordered::assert_eq_unordered_sort;
///
/// #[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
/// struct MyType(i32);
///
/// let expected = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
/// let actual = vec![MyType(2), MyType(0), MyType(4)];
///
/// assert_eq_unordered_sort!(expected, actual);
///  ```
///
/// Output:
/// ```text
/// thread 'tests::test' panicked at 'The left did not contain the same items as the right:
/// In left, not in right: "{MyType(1), MyType(5)}"
/// In right, not in left: "{MyType(0)}"'
/// ```
#[macro_export]
macro_rules! assert_eq_unordered_sort {
    ($left:expr, $right:expr $(,)?) => {
        $crate::pass_or_panic($crate::compare_unordered_sort($left, $right), core::option::Option::None);
    };
    ($left:expr, $right:expr, $($arg:tt)+) => {
        $crate::pass_or_panic(
            $crate::compare_unordered_sort($left, $right),
            core::option::Option::Some(core::format_args!($($arg)+))
        );
    };
}

/// Assert that `$left` and `$right` are "unordered" equal. That is, they contain the same elements,
/// but not necessarily in the same order. If this assertion is false, a panic is raised, and the
/// elements that are different between `$left` and `$right` are shown (when possible).
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
/// # Which macro?
///
/// Which macro to use largely depends on the use case. This macro is fairly efficient at finding
/// differences, but since it loads elements in to a set, it will NOT panic if there are duplicate
/// elements even if the collections have different lengths. It also requires [Eq] and [Hash]
/// implemented on its elements. This is also the only macro that requires `std`.
///
/// # Examples
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
///  ```
///
/// Output:
/// ```text
/// thread 'tests::test' panicked at 'The left did not contain the same items as the right:
/// In left, not in right: "{MyType(1), MyType(5)}"
/// In right, not in left: "{MyType(0)}"'
/// ```
///
/// # Limitations
/// Be aware that since using a set, this will NOT panic. If this does not work, consider one of the
/// other two macros.
/// ```
/// use assert_unordered::assert_eq_unordered_set;
///
/// #[derive(Debug, Eq, Hash, PartialEq)]
/// struct MyType(i32);
///
/// let expected = vec![MyType(2), MyType(4), MyType(4)];
/// let actual = vec![MyType(4), MyType(2)];
///
/// assert_eq_unordered_set!(expected, actual);
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
/// O(n^2 * 2) algorithmic complexity in the non-equality path).
///
/// # Which macro?
///
/// Which macro to use largely depends on the use case. This macro is second best at finding all
/// known differences between the collections, but is not very efficient. It will count two collections
/// as equal if they have the same elements and lengths, even if some are duplicated. It does, however,
/// have the least demanding trait requirements of its elements: just [Debug] and [PartialEq]
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
/// ```text
/// thread 'tests::test' panicked at 'The left did not contain the same items as the right:
/// In left, not in right: "{MyType(1), MyType(5)}"
/// In right, not in left: "{MyType(0)}"'
/// ```
///
/// # Limitations
/// Be aware that since we cannot sort, this will NOT panic. If this does not work, consider the
/// [assert_eq_unordered_sort] macro which does not have this limitation.
/// ```
/// use assert_unordered::assert_eq_unordered;
///
/// #[derive(Debug, PartialEq)]
/// struct MyType(i32);
///
/// let expected = vec![MyType(2), MyType(2), MyType(2), MyType(4)];
/// let actual = vec![MyType(2), MyType(4), MyType(4), MyType(4)];
///
/// assert_eq_unordered!(expected, actual);
/// ```
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
    NotEqualDupElements(String, String),
}

#[doc(hidden)]
pub fn pass_or_panic(result: CompareResult, msg: Option<Arguments>) {
    let panic_with_msg = |msg_args, left_msg, right_msg, left, right| {
        let msg = match msg {
            Some(msg) => msg,
            // TODO: 1.60 `format_args` not yet stable on 'const fn'. Maybe soon?
            None => msg_args,
        };

        panic!("{msg}:\n{left_msg}: {left:?}\n{right_msg}: {right:?}");
    };

    match result {
        CompareResult::NotEqualDiffElements(in_left_not_right, in_right_not_left) => {
            panic_with_msg(
                format_args!("The left did not contain the same items as the right"),
                "In left, not in right",
                "In right, not in left",
                in_left_not_right,
                in_right_not_left,
            );
        }
        CompareResult::NotEqualDupElemDiffLen(left, right) => {
            panic_with_msg(
                format_args!(
                    "The left is not the same length as the right, and there are duplicate items"
                ),
                "Left",
                "Right",
                left,
                right,
            );
        }
        CompareResult::NotEqualDupElements(left, right) => {
            panic_with_msg(
                format_args!("There are duplicate items, but left and right are the same length"),
                "Left",
                "Right",
                left,
                right,
            );
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
pub fn compare_unordered_sort<I, T>(left: I, right: I) -> CompareResult
where
    I: IntoIterator<Item = T> + PartialEq,
    T: Debug + Ord + PartialEq,
{
    // First, try for the easy (and faster compare)
    if left != right {
        // Fallback on the slower path by sorting
        let mut left: Vec<_> = left.into_iter().collect();
        let mut right: Vec<_> = right.into_iter().collect();

        left.sort_unstable();
        right.sort_unstable();

        if left != right {
            // If still not equal, compare one by one looking for differences
            let in_left_not_right: Vec<_> = left.iter().filter(|&t| !right.contains(t)).collect();
            let in_right_not_left: Vec<_> = right.iter().filter(|&t| !left.contains(t)).collect();

            if !in_left_not_right.is_empty() || !in_right_not_left.is_empty() {
                CompareResult::NotEqualDiffElements(
                    format!("{in_left_not_right:?}"),
                    format!("{in_right_not_left:?}"),
                )
            } else if left.len() != right.len() {
                // NOTE: We do length check AFTER element compare even though much less efficient
                // because we WANT the elem differences, not to just know they are different

                // Not equal len, but no diff elems, must be duplicate elements
                CompareResult::NotEqualDupElemDiffLen(format!("{left:?}"), format!("{right:?}"))
            } else {
                // Same length, but no differing elements, must be some duplicate elements in each
                CompareResult::NotEqualDupElements(format!("{left:?}"), format!("{right:?}"))
            }
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
            // NOTE: We do length check AFTER element compare even though much less efficient
            // because we WANT the elem differences, not to just know they are different

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
    use crate::{compare_unordered, compare_unordered_sort, CompareResult};
    use alloc::vec;

    // *** Regular ***

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
    fn compare_unordered_equal_diff_order() {
        #[derive(Debug, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(5), MyType(2), MyType(1), MyType(4)];

        assert!(matches!(
            compare_unordered(left, right),
            CompareResult::Equal
        ));
    }

    #[test]
    fn compare_unordered_equal_same_order() {
        #[derive(Debug, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(1), MyType(2), MyType(4), MyType(5)];

        assert!(matches!(
            compare_unordered(left, right),
            CompareResult::Equal
        ));
    }

    // *** Set ***

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
    fn compare_unordered_set_equal_same_order() {
        #[derive(Debug, Eq, Hash, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(1), MyType(2), MyType(4), MyType(5)];

        assert!(matches!(
            compare_unordered_set(left, right),
            CompareResult::Equal
        ));
    }

    #[cfg(feature = "std")]
    #[test]
    fn compare_unordered_set_equal_diff_order() {
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

    // *** Sort ***

    #[test]
    fn compare_unordered_sort_not_equal_diff_elem() {
        #[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(2), MyType(0), MyType(4)];

        match compare_unordered_sort(left, right) {
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
    fn compare_unordered_sort_not_equal_dup_elem_diff_len() {
        #[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
        struct MyType(i32);

        let left = vec![MyType(2), MyType(4), MyType(4)];
        let right = vec![MyType(4), MyType(2)];

        match compare_unordered_sort(left, right) {
            CompareResult::NotEqualDupElemDiffLen(left_actual, right_actual) => {
                let left_expected = "[MyType(2), MyType(4), MyType(4)]";
                let right_expected = "[MyType(2), MyType(4)]";
                assert_eq!(left_expected, left_actual);
                assert_eq!(right_expected, right_actual);
            }
            _ => {
                panic!("Left and right were expected to have have different elements");
            }
        }
    }

    #[test]
    fn compare_unordered_sort_not_equal_dup_elem() {
        #[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
        struct MyType(i32);

        let left = vec![MyType(2), MyType(2), MyType(2), MyType(4)];
        let right = vec![MyType(4), MyType(4), MyType(4), MyType(2)];

        match compare_unordered_sort(left, right) {
            CompareResult::NotEqualDupElements(left_actual, right_actual) => {
                let left_expected = "[MyType(2), MyType(2), MyType(2), MyType(4)]";
                let right_expected = "[MyType(2), MyType(4), MyType(4), MyType(4)]";
                assert_eq!(left_expected, left_actual);
                assert_eq!(right_expected, right_actual);
            }
            _ => {
                panic!("Left and right were expected to have have different elements");
            }
        }
    }

    #[test]
    fn compare_unordered_sort_equal_diff_order() {
        #[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(5), MyType(2), MyType(1), MyType(4)];

        assert!(matches!(
            compare_unordered_sort(left, right),
            CompareResult::Equal
        ));
    }

    #[test]
    fn compare_unordered_sort_equal_same_order() {
        #[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(1), MyType(2), MyType(4), MyType(5)];

        assert!(matches!(
            compare_unordered_sort(left, right),
            CompareResult::Equal
        ));
    }
}
