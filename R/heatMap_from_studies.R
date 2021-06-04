#' @rdname freqTab_heatMaps
#' @export
heatMap_from_studies <- function(x,
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
                                 fillScale = ggplot2::scale_fill_viridis_c(),
                                 theme = ggplot2::theme_minimal()) {
  
  freqTabArgs <-
    c(
      list(x = x,
           rowRegex = rowRegex,
           colRegex = colRegex),
      freqTabArgs);
  
  freqTab <-
    do.call(
      studies_to_freqTab,
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
