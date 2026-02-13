#' Plot Latent Model Comparison Results
#'
#' Creates visualization of model comparison results
#'
#' @param x An object of class 'modelscompete4' or 'latent_comparison'
#' @param type Type of plot: 'fit' for fit indices, 'diff' for differences
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot object (if ggplot2 and tidyr are available), otherwise NULL
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' model1 <- 'F1 =~ x1 + x2 + x3'
#' fit1 <- cfa(model1, data = HolzingerSwineford1939)
#' result <- compare_latent_models(fit1)
#' plot_latent_comparison(result)
#' }
plot_latent_comparison <- function(x, type = 'fit', ...) {
  # Check if required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 package is required for plotting. Please install it with install.packages('ggplot2')")
    return(invisible(NULL))
  }

  if (!requireNamespace("tidyr", quietly = TRUE)) {
    warning("tidyr package is required for plotting. Please install it with install.packages('tidyr')")
    return(invisible(NULL))
  }

  if (type == 'fit') {
    # Extract fit comparison data
    if (inherits(x, "latent_comparison")) {
      plot_data <- x$fit_comparison
    } else if (inherits(x, "modelscompete4")) {
      plot_data <- x$fit_comparison
    } else {
      stop("Object must be of class 'latent_comparison' or 'modelscompete4'")
    }

    # Check if we have data to plot
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      warning("No data available for plotting")
      return(invisible(NULL))
    }

    # Add model names as a column
    plot_data$Model <- rownames(plot_data)

    # Reshape data for plotting
    # Use explicit column names to avoid .data issue
    plot_long <- tidyr::pivot_longer(
      data = plot_data,
      cols = -c("Model"),
      names_to = "Criterion",
      values_to = "Value"
    )

    # Create plot - FIXED: Use string names instead of .data
    p <- ggplot2::ggplot(
      data = plot_long,
      ggplot2::aes_string(x = "Model", y = "Value", fill = "Criterion")
    ) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::labs(
        title = "Model Fit Indices Comparison",
        x = "Model",
        y = "Value",
        fill = "Fit Index"
      ) +
      ggplot2::theme_minimal()

    return(p)
  } else if (type == 'diff') {
    # Plot for difference tests
    if (is.null(x$diff_tests)) {
      message("No difference tests available. Returning fit plot instead.")
      return(plot_latent_comparison(x, type = 'fit', ...))
    }

    # Create difference test plot
    message("Difference plot not yet implemented. Returning fit plot.")
    return(plot_latent_comparison(x, type = 'fit', ...))
  } else {
    stop("type must be either 'fit' or 'diff'")
  }
}
