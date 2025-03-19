#' Save a ggplot with Flexible Format Handling
#'
#' @description
#' This function saves a ggplot object in various formats. It intelligently handles
#' file extensions to determine the output format. For SVG output, it saves directly.
#' For all other formats (PDF, PNG, JPG, etc.), it first saves as PDF and then
#' converts to the target format if needed. For unsupported formats, it saves as PDF.
#'
#' @param plot A ggplot2 object to be saved.
#' @param filename Character string specifying the output filename (with extension).
#' @param width Numeric value for the width of the output in inches. Default is 8.
#' @param height Numeric value for the height of the output in inches. Default is 6.
#' @param dpi Numeric value for the resolution of the output in dots per inch. Default is 300.
#' @param ... Additional arguments passed to ggplot2::ggsave().
#'
#' @return No return value, called for side effects (saving files).
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point(alpha = 0.5)
#' # Save as PDF
#' save_plot(p, "my_plot.pdf")
#' # Save as SVG directly
#' save_plot(p, "my_plot.svg")
#' # Save as PNG (with PDF intermediate)
#' save_plot(p, "my_plot.png")
#' # With additional ggsave parameters
#' save_plot(p, "my_plot.png", bg = "white", units = "cm")
#' }
#'
#' @importFrom ggplot2 ggsave
#' @importFrom pdftools pdf_convert
#' @importFrom tools file_path_sans_ext file_ext
#'
#' @export
save_plot <- function(plot, filename, width = 8, height = 6, dpi = 300, ...) {
  # Input validation
  if (!inherits(plot, "ggplot")) {
    stop("The 'plot' argument must be a ggplot object.")
  }

  if (!is.character(filename) || length(filename) != 1) {
    stop("The 'filename' argument must be a single character string.")
  }

  if (!is.numeric(width) || width <= 0) {
    stop("The 'width' argument must be a positive number.")
  }

  if (!is.numeric(height) || height <= 0) {
    stop("The 'height' argument must be a positive number.")
  }

  if (!is.numeric(dpi) || dpi <= 0) {
    stop("The 'dpi' argument must be a positive number.")
  }

  # Extract file extension and base name
  file_extension <- tolower(tools::file_ext(filename))
  base_filename <- tools::file_path_sans_ext(filename)

  # If no extension provided, default to PDF
  if (file_extension == "") {
    file_extension <- "pdf"
    filename <- paste0(filename, ".pdf")
  }

  # For unsupported formats, switch to PDF and warn
  supported_formats <- c("pdf", "svg", "png", "jpg", "jpeg", "tiff")
  if (!file_extension %in% supported_formats) {
    original_filename <- filename
    filename <- paste0(base_filename, ".pdf")
    file_extension <- "pdf"
    warning(paste("Format", toupper(tools::file_ext(original_filename)),
                  "is not supported. Saving as PDF instead at:", filename))
  }

  # Special case: SVG format (direct save)
  if (file_extension == "svg") {
    if (!requireNamespace("svglite", quietly = TRUE)) {
      stop("Package 'svglite' is required for saving to SVG format. Please install it with install.packages('svglite').")
    }

    tryCatch({
      ggplot2::ggsave(
        filename = filename,
        plot = plot,
        width = width,
        height = height,
        ...
      )
    }, error = function(e) {
      stop(paste("Failed to save SVG:", e$message))
    })

    return(invisible(NULL))
  }

  # For all other formats: First save as PDF
  pdf_filename <- paste0(base_filename, ".pdf")

  tryCatch({
    ggplot2::ggsave(
      filename = pdf_filename,
      plot = plot,
      width = width,
      height = height,
      ...
    )
  }, error = function(e) {
    stop(paste("Failed to save PDF:", e$message))
  })

  # If target format is not PDF, convert from PDF
  if (file_extension != "pdf") {
    tryCatch({
      pdftools::pdf_convert(
        pdf = pdf_filename,
        format = file_extension,
        dpi = dpi,
        filenames = filename
      )
    }, error = function(e) {
      warning(paste("Failed to convert PDF to", toupper(file_extension), ":", e$message,
                    "\nThe plot has been saved as PDF instead at:", pdf_filename))
    })
  }

  return(invisible(NULL))
}
