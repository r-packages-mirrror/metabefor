uniqueDf_ncols <- function(listOfDfs,
                           silent=FALSE) {

  if (is.null(names(listOfDfs))) {
    dfNames <- paste0("df_", seq_along(listOfDfs));
  } else {
    dfNames <- names(listOfDfs);
  }
  
  colNames <- lapply(listOfDfs, names);
  
  colNameLengths <- unlist(lapply(colNames, length));
  
  uniqueColLengthTable <- table(colNameLengths);
  uniqueColLengths <- names(uniqueColLengthTable);
  uniqueColLengthNs <- as.vector(uniqueColLengthTable);
  uniqueColLengthOrder <- order(uniqueColLengths);
  uniqueColLengths <- uniqueColLengths[uniqueColLengthOrder];
  uniqueColLengthNs <- uniqueColLengthNs[uniqueColLengthOrder];
  
  uniqueColLengths_asText <- paste0(
    uniqueColLengths,
    " (",
    uniqueColLengthNs,
    " times)"
  );
  
  dfNames_perLength <-
    lapply(
      as.vector(uniqueColLengths),
      function(colLength) {
        return(dfNames[colNameLengths == colLength]);
      }
    );
  names(dfNames_perLength) <- uniqueColLengths;
  
  colNames_perLength_asText <-
    vecTxt(
      lapply(
        uniqueColLengths,
        function(colLength) {
          return(
            paste0(
              colLength, " (",
              vecTxtQ(dfNames_perLength[[colLength]]),
              ")"
            )
          );
        }
      )
    );
  
  if (!(length(uniqueColLengths) == 1)) {

    if (!silent) {
      cat0("\n--- WARNING: Found varying numbers of columns (",
           vecTxt(uniqueColLengths_asText), ")! Specifically:\n",
           "--- ", colNames_perLength_asText, "\n\n");
    }
  }

  return(uniqueColLengths);
  
}