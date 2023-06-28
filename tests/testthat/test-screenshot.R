test_that("`screenshot()` does nothing in non-interactive sessions", {
  withr::local_options(rlang_interactive = FALSE)
  expect_snapshot(screenshot())
})

test_that("Writing ggplot to clipboard works", {
  skip()
  gg_to_clipboard <- function(plot = ggplot2::last_plot(), width = 1000, height = 600, pointsize = 40) {
    grDevices::win.graph(width = width, height = height, pointsize = pointsize)
    print(plot)
    savePlot("clipboard", type = "wmf")
    dev.off()
  }
})
