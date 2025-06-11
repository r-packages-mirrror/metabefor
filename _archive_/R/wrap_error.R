#' Wrap an error message to try to get users to read it
#' 
#' Pretty much does what it says on the tin.
#'
#' @param ... Character values that are concatenated without separator.
#'
#' @return The wrapped message.
#' @export
#'
#' @examples cat(
#'   metabefor::wrap_error(
#'     "You missed a closing quote somewhere!"
#'   )
#' );
wrap_error <- function(...) {
  closingLines <-
    c(
      "-------- ! -------------------------------------- ! --------",
      "-------- ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ! --------",
      "-------- ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ! --------",
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
      "------------------------------------------------------------",
      "---------------- ! Thank you for reading ! -----------------"
    );
  closingLine <- sample(closingLines, 1);
  return(
    paste0(
      "\n",
      "-------- ! metabefor error, please read carefully ! --------",
      "\n\n",
       wrapVector(paste0(...), 60),
       "\n\n",
      closingLine,
      "\n\n"
    )
  );
}