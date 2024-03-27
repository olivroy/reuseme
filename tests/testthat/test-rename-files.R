describe("rename_files2()", {
  og_file <- fs::path_real(test_path("ref", "my-analysis.R"))
  # temp dir + change working directory
  tmp_dir <- withr::local_tempdir()
  withr::local_dir(new = tmp_dir)
  fs::dir_create(c("data", "R"))
  fs::file_create("data/my-streets.csv")
  fs::file_create("data/my-highways.csv")

  fs::file_copy(og_file, "R/my-analysis.R")
  it("prevents file renaming if conflicts", {
    rlang::local_interactive(TRUE)
    expect_snapshot({
      rename_files2("data/my-streets.csv", "data/my-roads.csv")
    })
    expect_false(fs::file_exists("data/my-roads.csv"))
    expect_true(fs::file_exists("data/my-streets.csv"))

  })
  it("is easier to test messages with no action", {
    rlang::local_interactive(TRUE)
    expect_snapshot({
      rename_files2("data/my-streets.csv", "data/my-roads.csv", force = TRUE, action = "test")
    })
    expect_false(fs::file_exists("data/my-roads.csv"))
    expect_true(fs::file_exists("data/my-streets.csv"))
  })

  it("renames files if forced to do so", {
    rlang::local_interactive(TRUE)
    expect_snapshot({
      rename_files2("data/my-streets.csv", "data/my-roads.csv", force = TRUE)
    })
    expect_true(fs::file_exists("data/my-roads.csv"))
    expect_false(fs::file_exists("data/my-streets.csv"))

  })

})
test_that("A fake test", {
  expect_true(TRUE)
})
