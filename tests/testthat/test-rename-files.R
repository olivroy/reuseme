describe("rename_files2()", {
  og_file <- fs::path_real(test_path("_ref", "my-analysis.R"))
  # temp dir + change working directory
  tmp_dir <- withr::local_tempdir()
  fs::dir_create(
    tmp_dir,
    c("data", "R", "data-raw")
  )
  fs::file_create(
    tmp_dir,
    c(
    "data/my-streets.csv", "data/my-highways.csv",
    "data/my-king.png", "R/a.R"
  ))
  # TODO when ready add test for the presence of my-streets-raw.csv
  # fs::file_create("my-streets-raw.csv")
  fs::file_copy(og_file, fs::path(tmp_dir, "R/my-analysis.R"))

  it("prevents file renaming if dangerous", {
    withr::local_dir(new = tmp_dir)
    expect_error(
      rename_files2("data/my-streets.csv", "data/my-roads"),
      "extension"
    )
    expect_error(
      rename_files2("data/my-straeets.csv", "data/my-roads.csv"),
      "exist"
    )
    expect_error(
      rename_files2("data/my-streets.csv", "data/my-highways.csv"),
      "exist"
    )
  })
  it("prevents file renaming if conflicts", {
    withr::local_dir(new = tmp_dir)
    expect_snapshot({
      rename_files2("data/my-streets.csv", "data/my-roads.csv")
    })
    # No change
    expect_false(fs::file_exists("data/my-roads.csv"))
    expect_true(fs::file_exists("data/my-streets.csv"))
  })
  it("is easier to test messages with no action", {
    withr::local_dir(new = tmp_dir)
    expect_snapshot({
      rename_files2("data/my-streets.csv", "data/my-roads.csv", overwrite = TRUE, action = "test")
    })
    # No change
    expect_false(fs::file_exists("data/my-roads.csv"))
    expect_true(fs::file_exists("data/my-streets.csv"))
  })

  it("renames files if forced to do so", {
    withr::local_dir(new = tmp_dir)
    expect_snapshot({
      rename_files2("data/my-streets.csv", "data/my-roads.csv", warn_conflicts = "none", overwrite = TRUE)
    })
    # changed
    expect_true(fs::file_exists("data/my-roads.csv"))
    expect_false(fs::file_exists("data/my-streets.csv"))
  })

  it("doesn't check for references if file name is short", {
    withr::local_dir(new = tmp_dir)
    expect_snapshot(
      rename_files2("R/a.R", "R/b.R")
    )
    expect_true(fs::file_exists("R/b.R"))
  })
  it("priorizes references if name is generic or widely used in files", {
    withr::local_dir(new = tmp_dir)
    fs::file_move("data/my-roads.csv", "data/my-streets.csv")
    expect_true(fs::file_exists("data/my-streets.csv"))
    expect_snapshot(error = FALSE, {
      rename_files2("data/my-streets.csv", "data-raw/my-streets.csv")
    })
    expect_true(fs::file_exists("data/my-streets.csv"))
  })
  it("can accept overridden preferences", {
    withr::local_dir(new = tmp_dir)
    expect_snapshot(error = FALSE, {
      rename_files2("data/my-streets.csv", "data-raw/my-streets.csv", warn_conflicts = "all")
    })
    expect_true(fs::file_exists("data/my-streets.csv"))
  })
  it("relaxes its conditions for figures", {
    withr::local_dir(new = tmp_dir)
    rename_files2("data/my-king.png", "data/my-king2.png")
    expect_true(fs::file_exists("data/my-king2.png"))
  })

  it("calls check_referenced_files()", {
    withr::local_options(cli.hyperlinks = FALSE, cli.width = Inf)
    expect_snapshot(
      check_referenced_files(path = tmp_dir),
      transform = ~ gsub(" in.+$", " in some locations.", .x)
    )
  })
})
test_that("Helper files returns the expected input", {
  expect_equal(scope_rename("streets.csv", "streets2.csv"), "file_names")
  expect_equal(compute_conflicts_regex("streets.csv", "file_names"), "streets.csv")

  expect_equal(scope_rename("R/a.R", "R/b.R"), "file_names")
  expect_equal(compute_conflicts_regex("R/a.R", "file_names"), "R/a.R")

  expect_snapshot(error = TRUE, compute_conflicts_regex("x", "unknown_strategy"))
  # there could be a my-streets-raw.csv that exists.
  expect_equal(compute_conflicts_regex("dat/my-streets.csv", "object_names"), "dat/my-streets.csv|my_streets")
  expect_equal(compute_conflicts_regex("my-streets.csv", "base_names"), "my-streets|my_streets")
})

test_that("file testing are working as expected", {
  expect_true(is_adding_a_suffix("streets.csv", "streets2.csv"))
  expect_true(is_adding_a_suffix("aasa", "aasa2"))
  expect_true(is_short_file_name("R/12345.R", nchars = 5))
  expect_true(is_moving("R/my.R", "data/my.R"))
  expect_true(is_image("x.PNG"))
  expect_true(is_generic_file_name("data1.xlsx"))
  expect_false(is_generic_file_name("my-data.csv"))
})

test_that("force and action are deprecated", {
  file <- withr::local_tempfile(fileext = ".R", lines = c("# x1"))
  file2 <- withr::local_tempfile(fileext = ".R")
  unlink(file2)
  lifecycle::expect_deprecated(
    rename_files2(file, file2, force = TRUE)
  ) |>
    expect_message("Renamed .+ by force")
})
