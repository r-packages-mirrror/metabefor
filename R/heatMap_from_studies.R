#' @rdname freqTab_heatMaps
#' @export
heatMap_from_studies <- function(x,
                                 rowRegex,
                                 colRegex,
                                 freqTabArgs = NULL,
                                 rowOrder = rownames(freqTab),
                                 colOrder = colnames(freqTab),
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
  
  return(
    heatMap_from_freqTab(
      freqTab,
      rowOrder = rownames(freqTab),
      colOrder = colnames(freqTab),
      rowLab = "Row",
      colLab = "Column",
      freqLab = "Frequency",
      plotTitle = paste0("Frequency Table for ",
                         rowLab, " and ", colLab),
      xLabelRotationAngle = 45,
      legend.position = "right",
      fillScale = ggplot2::scale_fill_viridis_c(),
      theme = ggplot2::theme_minimal()
    )
  );

}
