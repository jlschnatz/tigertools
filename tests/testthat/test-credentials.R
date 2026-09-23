local_credentials_project <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(root, .local_envir = env)
  for (d in c("csv", "sqlite", "unmasked")) dir.create(d)
  # write_masked() also writes a copy to here::here(); keep that inside the
  # temp project instead of the package source tree
  testthat::local_mocked_bindings(here = function(...) root, .package = "here", .env = env)
  root
}

read_creds <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbReadTable(con, "credentials_db")
}

test_that("generate_pw() returns n alphanumeric passwords of the given length", {
  pw <- generate_pw(5, 12)
  expect_length(pw, 5)
  expect_true(all(nchar(pw) == 12))
  expect_true(all(grepl("^[A-Za-z0-9]+$", pw)))
  expect_error(generate_pw("5", 12))
})

test_that(".generate_users() hashes exactly the unmasked passwords", {
  x <- suppressMessages(.generate_users(3, 10))
  expect_equal(nrow(x$masked), 3)
  expect_equal(x$masked$user_name, x$unmasked$user_name)
  expect_false(any(x$masked$password_hashed == x$unmasked$password))
  for (i in 1:3) {
    expect_true(sodium::password_verify(x$masked$password_hashed[i], x$unmasked$password[i]))
  }
  # pseudonyms are lowercased with spaces replaced by "-"
  expect_false(any(grepl("\\s", x$masked$user_name)))
  expect_equal(x$masked$user_name, tolower(x$masked$user_name))
})

test_that("generate_users() aborts outside the credentials project folder", {
  withr::local_dir(withr::local_tempdir())
  expect_error(generate_users(2, 8), "not set correctly")
})

test_that("generate_users() writes masked CSV + SQLite, the server copy, and unmasked CSV", {
  root <- local_credentials_project()
  suppressMessages(generate_users(3, 10))

  stamp <- format(Sys.Date(), "%d%m%y")
  masked_csv <- utils::read.csv(file.path("csv", sprintf("data_user_credentials_%s.csv", stamp)))
  unmasked_csv <- utils::read.csv(file.path("unmasked", sprintf("data_user_credentials_unmasked_%s.csv", stamp)))
  expect_equal(nrow(masked_csv), 3)
  expect_named(unmasked_csv, c("user_name", "password"))
  expect_equal(masked_csv$user_name, unmasked_csv$user_name)

  db <- read_creds(file.path("sqlite", sprintf("db_credentials_%s.sqlite", stamp)))
  server <- read_creds(file.path(root, "db_credentials.sqlite"))
  expect_equal(db, server)
  expect_true(sodium::password_verify(db$password_hashed[1], unmasked_csv$password[1]))
})

test_that("generate_users() appends to existing users", {
  local_credentials_project()
  suppressMessages(generate_users(2, 8))
  suppressMessages(generate_users(2, 8))
  server <- read_creds("db_credentials.sqlite")
  expect_equal(nrow(server), 4)
  expect_false(anyDuplicated(server$user_name) > 0)
})

test_that("write_masked() refuses duplicated usernames", {
  local_credentials_project()
  x <- suppressMessages(.generate_users(2, 8))
  suppressMessages(write_masked(x))
  expect_error(suppressMessages(write_masked(x)), "duplicated usernames")
})
