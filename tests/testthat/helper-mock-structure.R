# This helper function creates a mock package structure for testing purposes.
create_mockstruct <- function() {
  root <- tempfile("mockpkg_")
  dir.create(root)
  unzip("testdata/mock.zip", exdir = root)
  file.create(file.path(root, ".Rproj"))
  return(root)
}