#' Look for all entity identifiers that a given pattern
#'
#' @param x,rxsTree The full Rxs project (i.e. parsed extraction
#' scripts) or one single Rxs tree.
#' @param regex The regular expression to use to find entity identifiers
#' @param includeValueListsOfMatch Whether to also include the value lists
#' inside matching entities (useful for quickly selecting e.g. all
#' results)
#' @param excludeParentWhenValueListReturned Whether, if an entity matches,
#' has a value list as value, and those value lists are returns (i.e.
#' `includeValueListsOfMatch` is `TRUE`), the parent entity (that matched
#' the regular expression) should be excluded.
#'
#' @return A character vector of identifiers
#' @rdname matchingUniqueEntityIdentifiers
#' @export
#'
#' @examples
rxsProject_matchingUniqueEntityIdentifiers <- function(x,
                                                       regex,
                                                       includeValueListsOfMatch = TRUE,
                                                       excludeParentWhenValueListReturned = TRUE) {
  
  if (!inherits(x, "rxs_parsedExtractionScripts")) {
    stop(wrap_error(
      "As `x`, you have to pass a full Rxs project (i.e. as ",
      "obtained when parsing a set of Rxs files ",
      "with `metabefor::rxs_parseExtractionScripts()`), but instead, ",
      "you passed an object with class(es) ", vecTxtQ(class(x)), "."
    ));
  }
  
  res <-
    lapply(
      x$rxsTrees,
      rxsTree_matchingUniqueEntityIdentifiers,
      regex = regex,
      includeValueListsOfMatch = includeValueListsOfMatch,
      excludeParentWhenValueListReturned = excludeParentWhenValueListReturned
    );
  
  ### Eliminate non-data.frame elements
  res <- res[
    unlist(
      lapply(
        res,
        is.data.frame
      )
    )
  ];
  
  res <-
    do.call(
      rbind,
      res
    );
  
  row.names(res) <- NULL;
  
  res <- unique(res);
  
  return(res);
  
}
