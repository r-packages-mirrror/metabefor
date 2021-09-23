#' Check whether a study tree has an entity
#' 
#' Check whether a study tree has an entity with an identifier that
#' matches the regular expression.
#'
#' @param x The study tree.
#' @param entityId_regex The regular expression 
#'
#' @return `TRUE` or `FALSE`.
#' @export
studyTree_has_entity <- function(x,
                                 entityId_regex) {
  res <- data.tree::Traverse(
    x,
    filterFun = function(node) {
      return(grepl(entityId_regex, node$name));
    }
  );
  return(!(is.null(res) || (length(res) == 0)));
}