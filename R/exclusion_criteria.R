#' #' Specifying exclusion criteria
#' #'
#' #' @param ... The exclusion criteria as a series of named
#' #' character values.
#' #'
#' #' @return A `exclusion_criteria` object, basically a dataframe with
#' #' two columns: the codes and their descriptions.
#' #' @rdname exclusion_criteria
#' #'
#' #' @examples
#' #' exclusion_criteria(notEmpirical = "Not an empirical study (e.g. a review or debate paper)",
#' #'                    notFluffy = "Fluffiness is not assessed");
#' #' @export
#' exclusion_criteria <- function(...) {
#'   listed <- list(...);
#'   if (length(names(listed))==0) {
#'     criteriaNames <-
#'       paste0("excl_criterion_", seq_along(listed));
#'   } else {
#'     criteriaNames <-
#'       names(listed);
#'   }
#'   descriptions <-
#'     as.character(listed);
#'   res <- descriptions;
#'   res <- data.frame(code = criteriaNames,
#'                     description = unlist(listed),
#'                     row.names = NULL,
#'                     stringsAsFactors=FALSE);
#'   return(res);
#' }
#' 
#' #' @export
#' #' @rdname exclusion_criteria
#' #' @method print exclusion_criteria
#' print.exclusion_criteria <- function(x,
#'                                      ...) {
#'   ### To add: check whether we're knitting
#'   print(x, ...);
#'   invisible(x);
#' }
#' 
