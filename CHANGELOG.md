# Changelog

## Unreleased

## v0.1.7 - 31 July 2023
- Fix bug where `read` was incorrectly returning non utf8 content for string on 
    javascript target. Now returns correct `NonUtf8` error

## v0.1.6 - 19 July 2023
- Add `make_directory` and `delete_directory` functions for working with directories.

## v0.1.5 - 12 July 2023
- Update to use new ffi syntax

## v0.1.4 - 21 June 2023
- Add `is_directory` and `list_contents` functions for working with directories.

## v0.1.3 - 21 June 2023
- Add ffi to remove gleam_erlang dependency

## v0.1.2 - 21 June 2023
- Remove unnecessary dependency on `gleam_javascript`. Whoops.

## v0.1.1 - 31 May 2023
- Refactored to match gleam idiom of ffi "do" functions and single signature.
- Added documentation about the utf8 issue with the read function. 
