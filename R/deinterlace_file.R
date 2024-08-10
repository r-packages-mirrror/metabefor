#' Deinterlace a file
#' 
#' This function removes either *all* odd lines from a file, or *all* even
#' lines. This can be useful when moving a file between operating systems
#' (e.g., from Apple to Windows) causes in double line endings.
#'
#' @param x A charactor vector or a file.
#' @param output Optionally, a filename to write the results to.
#'
#' @return The corrected character vector; if `output` is not `NULL`, that
#' vector is returned invisibly.
#' 
#' @export
#'
#' @examples metabefor::deinterlace_file(
#'   c(
#'     "first line",
#'     "",
#'     "second (real) line",
#'     "",
#'     "third line"
#'   )
#' );
deinterlace_file <- function(x,
                             output = NULL) {
  
  if (file.exists(x)) {
    ### Read file
    contents <- readLines(x);
  } else if (length(x) > 1) {
    contents <- x;
  } else {
    stop("As `x`, pass either a path to an (existing) file, or a character ",
         "vector (i.e., with length of at least two elements). You passed ",
         "neither (you passed '", x, "').");
  }

  ### Get vectors with only odd and only even lines
  oddLines <- contents[is.odd(seq_along(contents))];
  evenLines <- contents[is.even(seq_along(contents))];
  
  ### Remove spaces from lines holding only spaces
  oddLines_spaceless <- gsub("^\\s+$", "", oddLines);
  evenLines_spaceless <- gsub("^\\s+$", "", evenLines);
  
  ### Get unique values only
  oddLines_unique <- unique(oddLines_spaceless);
  evenLines_unique <- unique(evenLines_spaceless);
  
  if (all(oddLines_unique == "")) {
    res <- evenLines;
  } else if (all(evenLines_unique == "")) {
    res <- oddLines;
  } else {
    if (length(oddLines_unique) < length(evenLines_unique)) {
      shortest <- "odd";
      longest <- "even";
      shortestLength <- length(oddLines_unique);
      longestLength <- length(evenLines_unique);
      firstLines <- oddLines_unique[1:(min(shortestLength, 5))];
    } else {
      shortest <- "even";
      longest <- "odd";
      shortestLength <- length(evenLines_unique);
      longestLength <- length(oddLines_unique);
      firstLines <- evenLines_unique[1:(min(shortestLength, 5))];
    }
    warning("Neither all odd lines, nor all even lines are empty. ",
            "Returning contents of file ",
            x, " as is. For potential troubleshooting: the vector with ",
            shortest, " lines is the shortest (it has ", shortestLength,
            " unique elements, versus ", longestLength,
            " unique elements for the ", longest,
            " vector). The first elements are:\n\n",
            paste0("  - ", firstLines, "\n"));
    res <- contents;
  }
  
  if (is.null(output)) {
    return(res);
  } else {
    writeLines(
      res,
      output
    );
    return(invisible(res));
  }

}