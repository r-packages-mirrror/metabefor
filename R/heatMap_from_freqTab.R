#' @param freqTab The frequency table (as produced
#' by `studyTreeList_to_freqTab`).
#' @param rowLab,colLab,freqLab,plotTitle The labels to use for the rows (y
#' axis), columns (x axis), frequencies (fill), and the plot title to use.
#' @param fillScale The fill scale to use.
#' @param theme The ggplot2 theme to use.
#'
#' @rdname freqTab_heatMaps
#' 
#' @export
#'
#' @examples
heatMap_from_freqTab <- function(freqTab,
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
  
  df <-
    data.frame(row = rep(rownames(freqTab), times = ncol(freqTab)),
               col = rep(colnames(freqTab), each = nrow(freqTab)),
               freq = as.vector(freqTab));
  
  df$row <- factor(df$row,
                   levels = rowOrder,
                   labels = rowOrder,
                   ordered = TRUE);
  
  df$col <- factor(df$col,
                   levels = colOrder,
                   labels = colOrder,
                   ordered = TRUE);
  
  heatMap <-
    ggplot2::ggplot(data = df,
                    mapping = ggplot2::aes_string(
                      x = "col",
                      y = "row",
                      fill = "freq")
    ) +
    ggplot2::geom_tile() +
    theme +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(x = colLab,
                  y = rowLab,
                  fill = freqLab,
                  title = plotTitle) +
    ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = xLabelRotationAngle,
                                                              hjust = 1,
                                                              vjust = 1),
                   plot.title.position = "plot",
                   legend.position = legend.position);
  
  return(heatMap);
  
}