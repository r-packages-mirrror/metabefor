#' @rdname freqTab_heatMaps
#' @export
#' @examples ### Load an example Rxs Project
#' example_rxsProject_1 <- metabefor::example_rxsProject_1;
#' 
#' ### Create a heatmap
#' metabefor::heatMap_from_rxsProject(
#'   example_rxsProject_1,
#'   "sourceAuthors",
#'   "publicationYear",
#'   rowLab = "Author",
#'   colLab = "Year"
#' );
heatMap_from_rxsProject <- function(x,
                                    rowRegex,
                                    colRegex,
                                    freqTabArgs = NULL,
                                    rowOrder = NULL,
                                    colOrder = NULL,
                                    rowLab = "Row",
                                    colLab = "Column",
                                    freqLab = "Frequency",
                                    plotTitle = paste0("Frequency Table for ",
                                                       rowLab, " and ", colLab),
                                    xLabelRotationAngle = 45,
                                    legend.position = "right",
                                    flattenValues = TRUE,
                                    fillScale = ggplot2::scale_fill_viridis_c(),
                                    theme = ggplot2::theme_minimal()) {
  
  freqTabArgs <-
    c(
      list(x = x,
           rowRegex = rowRegex,
           colRegex = colRegex,
           flattenValues = flattenValues),
      freqTabArgs);
  
  freqTab <-
    do.call(
      rxsProject_to_freqTab,
      freqTabArgs
    );
  
  if (is.null(rowOrder)) {
    rowOrder <- rownames(freqTab);
  }
  if (is.null(colOrder)) {
    colOrder <- colnames(freqTab);
  }
  
  return(
    heatMap_from_freqTab(
      freqTab,
      rowOrder = rowOrder,
      colOrder = colOrder,
      rowLab = rowLab,
      colLab = colLab,
      freqLab = freqLab,
      plotTitle = plotTitle,
      xLabelRotationAngle = xLabelRotationAngle,
      legend.position = legend.position,
      fillScale = fillScale,
      theme = theme
    )
  );

}
