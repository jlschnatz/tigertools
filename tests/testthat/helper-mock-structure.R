# Creates a mock item-database project (items/, items/www/, CSV, SQLite, an
# .Rproj file) in a fresh temp dir and returns its path. Call it before any
# withr::local_dir(), since the zip path is relative to tests/testthat.
create_mockstruct <- function() {
  root <- tempfile("mockpkg_")
  dir.create(root)
  utils::unzip(testthat::test_path("testdata", "mock.zip"), exdir = root)
  unlink(file.path(root, "__MACOSX"), recursive = TRUE)
  # Not a hidden ".Rproj": dir() (used by check_folderstruct()) skips dotfiles
  file.create(file.path(root, "mock.Rproj"))
  root
}
