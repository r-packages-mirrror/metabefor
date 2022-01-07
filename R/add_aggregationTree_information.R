#' @rdname add_aggregationTree_information
#' @export
add_aggregationTree_information <- function(x,
                                            aggregationTree,
                                            fieldName,
                                            prefixes = NULL,
                                            suffixes = NULL) {
  
  if (!inherits(x, "rxs_parsedExtractionScripts")) {
    stop(wrap_error(
      "As `x`, you have to pass a full Rxs project (i.e. as ",
      "obtained when parsing a set of Rxs files ",
      "with `metabefor::rxs_parseExtractionScripts()`), but instead, ",
      "you passed an object with class(es) ", vecTxtQ(class(x)), "."
    ));
  }
  
  for (i in seq_along(x$rxsTrees)) {
    if (inherits(x$rxsTrees[[i]], "Node")) {
      add_aggregationTree_information_toRxsTree(
        rxsTree = x$rxsTrees[[i]],
        aggregationTree = aggregationTree,
        fieldName = fieldName,
        prefixes = prefixes,
        suffixes = suffixes
      );
    }
  }
  
  return(invisible(x));
  
}
