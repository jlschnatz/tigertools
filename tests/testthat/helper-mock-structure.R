#' @title Mock Structure for Testing
create_mock_structure <- function() {
  root <- tempfile("mockpkg_")
  dir.create(root)
  
  # Create folder structure
  dir.create(file.path(root, "items", "www"), recursive = TRUE)
  
  # Create required files
  file.create(file.path(root, ".Rproj"))
  file.create(file.path(root, "items", "test.md"))
  writeLines(test_md_content, file.path(root, "items", "test.md"))
  
  return(root)
}




