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
#' @param path Character string specifying the directory where the plot should be saved.
#'        If NULL (default), the current working directory is used.
#' @param width Numeric value for the width of the output in inches. Default is 8.
#' @param height Numeric value for the height of the output in inches. Default is 6.
#' @param dpi Numeric value for the resolution of the output in dots per inch. Default is 300.
#' @param ... Additional arguments passed to ggplot2::ggsave().
#'
#' @return No return value, called for side effects (saving files).
#'
#' @export
save_plot <- function(plot, filename, path = NULL, width = 8, height = 6, dpi = 300, ...) {
  # Input validation
  if (!inherits(plot, "ggplot")) {
    stop("The 'plot' argument must be a ggplot object.")
  }

  if (!is.character(filename) || length(filename) != 1) {
    stop("The 'filename' argument must be a single character string.")
  }

  if (!is.null(path) && (!is.character(path) || length(path) != 1)) {
    stop("The 'path' argument must be a single character string or NULL.")
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

  # Create directory if it doesn't exist
  if (!is.null(path)) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    # Remove trailing slash if present to use file.path consistently
    path <- gsub("[\\/]+$", "", path)
  } else {
    path <- "."
  }

  # Extract file extension and base name (from filename only, not full path)
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
                  "is not supported. Saving as PDF instead at:", file.path(path, filename)))
  }

  # Create full file paths using file.path (which handles slashes correctly)
  full_filename <- file.path(path, filename)
  pdf_filename <- file.path(path, paste0(base_filename, ".pdf"))

  # Special case: SVG format (direct save)
  if (file_extension == "svg") {
    if (!requireNamespace("svglite", quietly = TRUE)) {
      stop("Package 'svglite' is required for saving to SVG format. Please install it with install.packages('svglite').")
    }

    tryCatch({
      ggplot2::ggsave(
        filename = full_filename,
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
      # Create a pattern for the output filename that includes %d for page numbers
      # This is required by pdf_convert even if we're only converting one page
      output_pattern <- gsub(paste0("\\.", file_extension, "$"),
                             paste0("_%d.", file_extension),
                             full_filename)

      # Convert the PDF to the desired format
      converted_files <- pdftools::pdf_convert(
        pdf = pdf_filename,
        format = file_extension,
        dpi = dpi,
        verbose = FALSE
      )

      # Rename the first converted file to the desired filename
      if (length(converted_files) > 0) {
        file.rename(converted_files[1], full_filename)

        # Remove any extra files (if multiple pages were converted)
        if (length(converted_files) > 1) {
          file.remove(converted_files[-1])
        }

        # Delete the intermediate PDF file if conversion was successful
        file.remove(pdf_filename)
      }

    }, error = function(e) {
      warning(paste("Failed to convert PDF to", toupper(file_extension), ":", e$message,
                    "\nThe plot has been saved as PDF instead at:", pdf_filename))
    })
  }

  return(invisible(NULL))
}
