# tests/testthat/test-save_plot.R

library(testthat)
library(ggplot2)

# Create a helper function to set up a test plot
create_test_plot <- function() {
  ggplot(mtcars, aes(x = mpg, y = wt)) +
    geom_point(alpha = 0.7) +
    labs(title = "Test Plot", x = "Miles Per Gallon", y = "Weight (1000 lbs)")
}

# Create a temporary directory for test outputs
setup_test_dir <- function() {
  test_dir <- file.path(tempdir(), paste0("test_save_plot_", format(Sys.time(), "%H%M%S")))
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  return(test_dir)
}

test_that("save_plot can save in PDF format", {
  skip_if_not_installed("ggplot2")

  test_dir <- setup_test_dir()
  on.exit(unlink(test_dir, recursive = TRUE))

  test_plot <- create_test_plot()
  pdf_file <- file.path(test_dir, "test_plot.pdf")

  save_plot(test_plot, pdf_file, width = 6, height = 4)

  expect_true(file.exists(pdf_file))
  expect_gt(file.size(pdf_file), 0)
})

test_that("save_plot can save in PNG format", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("pdftools")

  test_dir <- setup_test_dir()
  on.exit(unlink(test_dir, recursive = TRUE))

  test_plot <- create_test_plot()
  png_file <- file.path(test_dir, "test_plot.png")
  pdf_file <- file.path(test_dir, "test_plot.pdf")

  # Expect a warning about unused arguments in pdf_convert
  expect_warning(
    save_plot(test_plot, png_file, width = 6, height = 4, dpi = 150),
    "arguments not used by format"
  )

  expect_true(file.exists(png_file))
  expect_true(file.exists(pdf_file))
  expect_gt(file.size(png_file), 0)
  expect_gt(file.size(pdf_file), 0)
})

test_that("save_plot can save in SVG format", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("svglite")

  test_dir <- setup_test_dir()
  on.exit(unlink(test_dir, recursive = TRUE))

  test_plot <- create_test_plot()
  svg_file <- file.path(test_dir, "test_plot.svg")

  save_plot(test_plot, svg_file, width = 6, height = 4)

  expect_true(file.exists(svg_file))
  expect_gt(file.size(svg_file), 0)
  expect_false(file.exists(file.path(test_dir, "test_plot.pdf")))
})

test_that("save_plot accepts additional ggsave parameters", {
  skip_if_not_installed("ggplot2")

  test_dir <- setup_test_dir()
  on.exit(unlink(test_dir, recursive = TRUE))

  test_plot <- create_test_plot()
  pdf_file <- file.path(test_dir, "test_plot.pdf")

  save_plot(
    test_plot,
    pdf_file,
    width = 6,
    height = 4,
    units = "cm",
    bg = "white",
    scale = 1.2
  )

  expect_true(file.exists(pdf_file))
  expect_gt(file.size(pdf_file), 0)
})

test_that("save_plot handles filenames without extension", {
  skip_if_not_installed("ggplot2")

  test_dir <- setup_test_dir()
  on.exit(unlink(test_dir, recursive = TRUE))

  test_plot <- create_test_plot()
  no_ext_file <- file.path(test_dir, "test_plot")
  pdf_file <- paste0(no_ext_file, ".pdf")

  save_plot(test_plot, no_ext_file, width = 6, height = 4)

  expect_true(file.exists(pdf_file))
  expect_gt(file.size(pdf_file), 0)
})

test_that("save_plot handles unsupported formats", {
  skip_if_not_installed("ggplot2")

  test_dir <- setup_test_dir()
  on.exit(unlink(test_dir, recursive = TRUE))

  test_plot <- create_test_plot()
  unsupported_file <- file.path(test_dir, "test_plot.xyz")
  pdf_file <- file.path(test_dir, "test_plot.pdf")

  expect_warning({
    save_plot(test_plot, unsupported_file, width = 6, height = 4)
  }, "is not supported")

  expect_true(file.exists(pdf_file))
  expect_gt(file.size(pdf_file), 0)
  expect_false(file.exists(unsupported_file))
})
