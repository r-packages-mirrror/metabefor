#' Look for all entity identifiers that a given pattern
#'
#' @param studies,studyTree The `studies` object (i.e. parsed extraction
#' scripts) or one single `studyTree`.
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
studies_matchingUniqueEntityIdentifiers <- function(studies,
                                                    regex,
                                                    includeValueListsOfMatch = TRUE,
                                                    excludeParentWhenValueListReturned = TRUE) {
  res <-
    lapply(
      studies$rxsTrees,
      studyTree_matchingUniqueEntityIdentifiers,
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
