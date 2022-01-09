getTxt <- function(textId,
                   textDf = NULL) {
  if (is.null(textDf)) {
    return(metabefor::opts$get(textId));
  } else {
    textColNames <-
      metabefor::opts$get(textsColNames);
  }
}
