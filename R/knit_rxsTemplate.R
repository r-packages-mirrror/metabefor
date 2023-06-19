#' Knit the Rxs Template
#'
#' @param x The object with the Rxs Specification as produced by a call to
#' [metabefor::rxs_fromSpecifications()].
#'
#' @return
#' @export
#'
#' @examples
knit_rxsTemplate <- function(x) {
  
  if (inherits(x, "rxsStructure")) {
    
    return(
      knitr::asis_output(
        paste0(
          "\n\n<pre><textarea rows='40' cols='124' style='font-family:monospace;font-size:11px;white-space:pre;'>",
          paste0(
            unlist(x$rxsTemplate),
            collapse = "\n"
          ),
          "</textarea></pre>\n\n",
          sep = "\n"
        )
      )
    );
    
  } else if (inherits(x, "rxsStructures")) {
    
    return(
      knitr::asis_output(
        unlist(
          lapply(
            names(x$rxsTemplates),
            function(currentX_name) {
              return(
                paste0(
                  metabefor::heading(
                    currentX_name
                  ),
                  "\n\n<pre><textarea rows='40' cols='124' style='font-family:monospace;font-size:11px;white-space:pre;'>",
                  paste0(
                    unlist(x$rxsTemplates[[currentX_name]]),
                    collapse = "\n"
                  ),
                  "</textarea></pre>\n\n",
                  sep = "\n"
                )
              );
            }
          )
        )
      )
    );
    
  } else {
    stop("As `x`, pass an object of class `rxsStructure`, as produced by ",
         "a call to metabefor::rxs_fromSpecifications().");
  }
  
  
}