describe("rename_files2()", {
  og_file <- fs::path_real(test_path("ref", "my-analysis.R"))
  # temp dir + change working directory
  tmp_dir <- withr::local_tempdir()
  withr::local_dir(new = tmp_dir)
  fs::dir_create(
    c("data", "R", "data-raw"))
  fs::file_create(c(
    "data/my-streets.csv","data/my-highways.csv",
    "data/my-king.png", "R/a.R"
    ))
  fs::file_copy(og_file, "R/my-analysis.R")

  it("prevents file renaming if dangerous", {
    expect_error(
      rename_files2("data/my-streets.csv", "data/my-roads"),
      "extension"
    )
    expect_error(
      rename_files2("data/my-straeets.csv", "data/my-roads.csv"),
      "exist"
    )
  })
  it("prevents file renaming if conflicts", {
    expect_snapshot({
      rename_files2("data/my-streets.csv", "data/my-roads.csv")
    })
    # No change
    expect_false(fs::file_exists("data/my-roads.csv"))
    expect_true(fs::file_exists("data/my-streets.csv"))
  })
  it("is easier to test messages with no action", {
    expect_snapshot({
      rename_files2("data/my-streets.csv", "data/my-roads.csv", force = TRUE, action = "test")
    })
    # No change
    expect_false(fs::file_exists("data/my-roads.csv"))
    expect_true(fs::file_exists("data/my-streets.csv"))
  })

  it("renames files if forced to do so", {
    expect_snapshot({
      rename_files2("data/my-streets.csv", "data/my-roads.csv", force = TRUE)
    })
    # changed
    expect_true(fs::file_exists("data/my-roads.csv"))
    expect_false(fs::file_exists("data/my-streets.csv"))
  })

  it("doesn't check for references if file name is short", {
    expect_snapshot(
      rename_files2("R/a.R", "R/b.R")
    )
    expect_true(fs::file_exists("R/b.R"))
  })
  it("priorizes references if name is generic or widely used in files", {
    fs::file_move("data/my-roads.csv", "data/my-streets.csv")
    expect_true(fs::file_exists("data/my-streets.csv"))
    expect_snapshot(error = FALSE, {
      rename_files2("data/my-streets.csv", "data-raw/my-streets.csv")
    })
    expect_true(fs::file_exists("data/my-streets.csv"))
  })
  it("relaxes its conditions for figures", {
    rename_files2("data/my-king.png", "data/my-king2.png")
    expect_true(fs::file_exists("data/my-king2.png"))
  })
})
test_that("A fake test", {
  expect_true(TRUE)
})
