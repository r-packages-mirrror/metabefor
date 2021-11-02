#' Flatten all studies to a wide data frame (somewhat brutally)
#'
#' @param studies The studies object (i.e. parsed rxs files)
#'
#' @return A data frame
#' @export
#'
#' @examples
flattenStudies_to_wideDf <- function(studies,
                                     silent = metabefor::opts$get("silent")) {

  ### Using iconv because during purling/evaluation, conversion got lost
  rxsList <- lapply(
    studies$rxsTrees,
    function(rxsObject) {
      if (inherits(rxsObject, "Node")) {
        return(rxsObject$Get('value'));
      } else {
        return(NA);
      }
    });
  names(rxsList) <- names(studies$rxsTrees);
  
  flattenedValues <-
    lapply(
      rxsList,
      function(x) {
        return(
          lapply(
            x,
            metabefor::flattenNodeValue
          )
        );
      }
    );
  
  flattenedValuesAsDfs <-
    lapply(
      flattenedValues,
      as.data.frame
    );
  
  flattenedValuesAsDfs <-
    lapply(
      names(flattenedValuesAsDfs),
      function(fileName) {
        flattenedValuesAsDfs[[fileName]]$filename <-
          fileName;
        return(flattenedValuesAsDfs[[fileName]]);
      }
    );
  
  rxsDatRaw <-
    metabefor::rbind_df_list(
      flattenedValuesAsDfs
    );
  
  row.names(rxsDatRaw) <-
    rxsDatRaw$filename;
  
  ### Process lists to create vectors
  rxsDat <- data.frame(rxs__source = row.names(rxsDatRaw));
  multiResponseRegexes <- c();
  for (colName in names(rxsDatRaw)) {
    if (any(lapply(rxsDatRaw[, colName], length) > 1)) {
      options <-
        sort(unique(unlist(rxsDatRaw[, colName])));
      rxsDat[, paste0(colName, "__raw")] <-
        iconv(vecTxtQ(options), from="UTF-8");
      for (optionName in options) {
        rxsDat[, paste0(colName, "__", optionName)] <-
          rxsDatRaw[, colName] == optionName;
      }
      multiResponseRegexes <-
        c(multiResponseRegexes,
          paste0(paste0(colName, "__", options),
                 collapse="|")
        );
    } else {
      rxsDat[, colName] <-
        iconv(unlist(rxsDatRaw[, colName]),
              from="UTF-8");
    }
  }
  
  return(rxsDat);
  
}