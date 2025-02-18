test_that("extract_string_file_names() works", {
  expect_equal(extract_string_file_names("read_excel(\"file.xlsx\")"), "file.xlsx")
})

test_that("extract_plain_file_names() works", {
  expect_equal(extract_plain_file_names("_quarto.yml"), "_quarto.yml")
  expect_equal(extract_plain_file_names("{.file _quarto.yml}"), "_quarto.yml")
})

