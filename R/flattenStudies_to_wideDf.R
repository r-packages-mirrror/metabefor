#' Flatten all sources' Rxs trees to a wide data frame (somewhat brutally)
#'
#' @param x The full Rxs Project object (i.e. the parsed Rxs files)
#' @param silent Whether to be silent or chatty.
#'
#' @return A data frame
#' @export
#'
#' @examples
flattenRxsProject_to_wideDf <- function(x,
                                        silent = metabefor::opts$get("silent")) {
  
  if (!inherits(x, "rxs_parsedExtractionScripts")) {
    stop(wrap_error(
      "As `x`, you have to pass a full Rxs project (i.e. as ",
      "obtained when parsing a set of Rxs files ",
      "with `metabefor::rxs_parseExtractionScripts()`), but instead, ",
      "you passed an object with class(es) ", vecTxtQ(class(x)), "."
    ));
  }
  
  ### Using iconv because during purling/evaluation, conversion got lost
  rxsList <- lapply(
    x$rxsTrees,
    function(rxsObject) {
      if (inherits(rxsObject, "Node")) {
        return(rxsObject$Get('value'));
      } else {
        return(NA);
      }
    });
  names(rxsList) <- names(x$rxsTrees);
  
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