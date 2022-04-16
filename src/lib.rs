#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![warn(missing_docs)]

//! A direct replacement for `assert_eq` for unordered collections

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
pub fn pass_or_panic(left_right: Option<(String, String)>, msg: Option<Arguments>) {
    if let Some((in_left_not_right, in_right_not_left)) = left_right {
        let msg = match msg {
            Some(msg) => msg,
            // TODO: 1.60 `format_args` not yet stable on 'const fn'. Maybe soon?
            None => format_args!("The left did not contain the same items as the right"),
        };

        panic!("{msg}:\nIn left, not in right: {in_left_not_right:?}\nIn right, not in left: {in_right_not_left:?}");
    }
}

#[cfg(feature = "std")]
#[doc(hidden)]
pub fn compare_unordered_set<I, T>(left: I, right: I) -> Option<(String, String)>
where
    I: IntoIterator<Item = T> + PartialEq,
    T: Debug + Eq + Hash,
{
    // First, try for the easy (and faster compare)
    if left != right {
        // Fallback on the slow difference path
        let left: HashSet<_> = left.into_iter().collect();
        let right: HashSet<_> = right.into_iter().collect();

        if left != right {
            let in_left_not_right: HashSet<_> = left.difference(&right).collect();
            let in_right_not_left: HashSet<_> = right.difference(&left).collect();
            Some((
                format!("{in_left_not_right:?}"),
                format!("{in_right_not_left:?}"),
            ))
        } else {
            None
        }
    } else {
        None
    }
}

#[doc(hidden)]
pub fn compare_unordered<I, T>(left: I, right: I) -> Option<(String, String)>
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
            Some((
                format!("{in_left_not_right:?}"),
                format!("{in_right_not_left:?}"),
            ))
        } else {
            None
        }
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::compare_unordered;
    #[cfg(feature = "std")]
    use crate::compare_unordered_set;
    use alloc::vec;

    #[test]
    fn compare_unordered_not_equal() {
        #[derive(Debug, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(2), MyType(0), MyType(4)];

        match compare_unordered(left, right) {
            Some((left_actual, right_actual)) => {
                let left_expected = "[MyType(1), MyType(5)]";
                let right_expected = "[MyType(0)]";
                assert_eq!(left_expected, left_actual);
                assert_eq!(right_expected, right_actual);
            }
            None => {
                panic!("Left and right were equal, but should not have been");
            }
        }
    }

    #[test]
    fn compare_unordered_equal() {
        #[derive(Debug, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4), MyType(5)];
        let right = vec![MyType(5), MyType(2), MyType(1), MyType(4)];

        assert!(compare_unordered(left, right).is_none());
    }

    #[cfg(feature = "std")]
    #[test]
    fn compare_unordered_set_not_equal() {
        #[derive(Debug, Eq, Hash, PartialEq)]
        struct MyType(i32);

        let left = vec![MyType(1), MyType(2), MyType(4)];
        let right = vec![MyType(2), MyType(0), MyType(4)];

        match compare_unordered_set(left, right) {
            Some((left_actual, right_actual)) => {
                let left_expected = "{MyType(1)}";
                let right_expected = "{MyType(0)}";
                assert_eq!(left_expected, left_actual);
                assert_eq!(right_expected, right_actual);
            }
            None => {
                panic!("Left and right were equal, but should not have been");
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

        assert!(compare_unordered_set(left, right).is_none());
    }
}
