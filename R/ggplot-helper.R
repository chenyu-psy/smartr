#' Custom facet scales
#'
#' @description
#' Sets different axis limits for each facet in a ggplot2 facet_wrap or facet_grid plot.
#'
#' @details
#' This is an advanced function that modifies the internal behavior of ggplot2.
#' It should be used with caution as it may break with future ggplot2 updates.
#' The function works by intercepting the initialization of scales in the facet
#' and replacing the limits with custom values provided by the user.
#'
#' This function is based on the solution provided at:
#' https://stackoverflow.com/questions/51735481/ggplot2-change-axis-limits-for-each-individual-facet-panel
#'
#' @author Canadian Marine, Chenyu Li
#'
#' @param xlims A list of numeric vectors specifying the x-axis limits for each facet.
#'        Each element should be a vector of length 2 (c(min, max)) or NULL to use default limits.
#' @param ylims A list of numeric vectors specifying the y-axis limits for each facet.
#'        Each element should be a vector of length 2 (c(min, max)) or NULL to use default limits.
#'
#' @return A ggplot2 layer that can be added to a ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Create a sample plot with facets
#' data <- data.frame(
#'   x = 1:100,
#'   y = rnorm(100),
#'   group = rep(letters[1:4], each = 25)
#' )
#'
#' # Set custom y-axis limits for each facet
#' ylims <- list(c(-1, 1), c(-2, 2), c(-3, 3), c(-4, 4))
#'
#' # Apply custom limits using the + operator
#' p <- ggplot(data, aes(x, y)) +
#'   geom_point() +
#'   facet_wrap(~group) +
#'   facet_scales(ylims = ylims)
#'
#' # Set both x and y limits
#' xlims <- list(c(1, 20), c(5, 25), c(10, 30), c(15, 35))
#' p2 <- ggplot(data, aes(x, y)) +
#'   geom_point() +
#'   facet_wrap(~group) +
#'   facet_scales(xlims = xlims, ylims = ylims)
#' }
facet_scales <- function(xlims = NULL, ylims = NULL) {
  # Create a custom ggproto object
  structure(
    list(
      xlims = xlims,
      ylims = ylims
    ),
    class = c("facet_scales", "gg")
  )
}

#' @export
ggplot_add.facet_scales <- function(object, plot, object_name) {
  # Input validation
  if (is.null(object$xlims) && is.null(object$ylims)) {
    warning("No axis limits provided. Returning original plot.")
    return(plot)
  }

  # Check if the plot has facets
  if (is.null(plot$facet) || inherits(plot$facet, "FacetNull")) {
    warning("The plot does not have facets. Individual axis scaling will have no effect.")
    return(plot)
  }

  # Store the original init_scales function
  init_scales_orig <- plot$facet$init_scales

  # Create a new function that will intercept and modify the scales
  init_scales_new <- function(...) {
    # Call the original function to get the scales
    scales <- init_scales_orig(...)

    # Modify x-axis limits if provided
    if (!is.null(object$xlims) && !is.null(scales$x)) {
      x_scales <- scales$x

      # Validate xlims length
      if (length(object$xlims) != length(x_scales)) {
        warning("Length of xlims (", length(object$xlims),
                ") does not match number of facets (", length(x_scales),
                "). Some facets may not be modified as expected.")
      }

      # Apply custom limits to each facet
      for (i in seq_along(x_scales)) {
        if (i <= length(object$xlims) && !is.null(object$xlims[[i]])) {
          # Validate the limit format
          if (length(object$xlims[[i]]) != 2 || !is.numeric(object$xlims[[i]])) {
            warning("xlims[[", i, "]] is not a numeric vector of length 2. Skipping.")
            next
          }
          x_scales[[i]]$limits <- object$xlims[[i]]
        }
      }

      # Update the scales with modified x-axis limits
      scales$x <- x_scales
    }

    # Modify y-axis limits if provided
    if (!is.null(object$ylims) && !is.null(scales$y)) {
      y_scales <- scales$y

      # Validate ylims length
      if (length(object$ylims) != length(y_scales)) {
        warning("Length of ylims (", length(object$ylims),
                ") does not match number of facets (", length(y_scales),
                "). Some facets may not be modified as expected.")
      }

      # Apply custom limits to each facet
      for (i in seq_along(y_scales)) {
        if (i <= length(object$ylims) && !is.null(object$ylims[[i]])) {
          # Validate the limit format
          if (length(object$ylims[[i]]) != 2 || !is.numeric(object$ylims[[i]])) {
            warning("ylims[[", i, "]] is not a numeric vector of length 2. Skipping.")
            next
          }
          y_scales[[i]]$limits <- object$ylims[[i]]
        }
      }

      # Update the scales with modified y-axis limits
      scales$y <- y_scales
    }

    return(scales)
  }

  # Replace the original init_scales function with our modified version
  plot$facet$init_scales <- init_scales_new

  return(plot)
}
