#' Knit the Rxs Template
#' 
#' This function knits an Rxs template stored in a parsed Rxs specification,
#' as produced by a call to [metabefor::rxs_fromSpecifications()], into an
#' R Markdown (or Quarto) file. 
#'
#' @param x The object with the Rxs structure as produced by a call to
#' [metabefor::rxs_fromSpecifications()].
#'
#' @return A character vector with class "knit_asis".
#' @export
#'
#' @examples ### Load example Rxs structure
#' data('rxs_minimal_example_2', package="metabefor");
#' 
#' ### Produce the Markdown
#' knittableMarkdown <-
#'   knit_rxsTemplate(rxs_minimal_example_2);
#'
#' ### Show the beginning
#' cat(substr(knittableMarkdown, 1, 2463));
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
        paste0(
          unlist(
            lapply(
              names(x$rxsTemplates),
              function(currentX_name) {
                return(
                  paste0(
                    metabefor::heading(
                      "Module: ",
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
          ),
          collapse = "\n\n"
        )
      )
    );
    
  } else {
    stop("As `x`, pass an object of class `rxsStructure`, as produced by ",
         "a call to metabefor::rxs_fromSpecifications().");
  }
  
  
}