#' Convert a bibentry (or more) to a data frame
#' 
#' This function takes a bibentry and converts it to a data frame.
#'
#' @param bibentry The bibentry (or bibentries)
#' @param authorCollapseFunc The function to use to collapse the vector of
#' author names into one value.
#' @param authorCollapseArgs Argyments to pass to `authorCollapseFunc()`.
#'
#' @return A data frame
#' @export
#'
#' @examples ### Use the base R bibentry
#' metabefor::bibentry_to_df(citation());
bibentry_to_df <- function(bibentry,
                           authorCollapseFunc = metabefor::vecTxt,
                           authorCollapseArgs = list(delimiter = " and ",
                                                     lastDelimiter = NULL)) {
  
  if (!inherits(bibentry, "bibentry")) {
    stop("As `x`, you can only pass an object of class `bibentry`. ",
         "You passed an object of class(s) ",
         metabefor::vecTxtQ(class(bibentry)));
  }
  
  if (length(bibentry) > 1) {
    res <- lapply(bibentry,
                  bibentry_to_df,
                  authorCollapseFunc = authorCollapseFunc,
                  authorCollapseArgs = authorCollapseArgs);
    return(
      metabefor::rbind_df_list(res)
    );
  }
  
  authorsAsDf <- as.data.frame(bibentry[[1]]$author);
  authorsAsVector <-
    apply(
      authorsAsDf,
      1,
      function(x) {
        return(
          paste0(
            x['family'],
            ", ",
            x['given']
          )
        )
      }
    );
  
  asList <-
    unclass(bibentry)[[1]];
  
  asList$author <-
    do.call(
      authorCollapseFunc,
      c(list(vector = authorsAsVector),
        authorCollapseArgs)
    );

  res <- data.frame(asList);
  
  attr(res, "bibtype") <- attr(asList, "bibtype");
  attr(res, "key") <- attr(asList, "key");
  
  if ("bibtype" %in% names(res)) {
    res$bibtype_fromAttr <- attr(asList, "bibtype");
  } else {
    res$bibtype <- attr(asList, "bibtype");
  }
  
  if ("key" %in% names(res)) {
    res$key_fromAttr <- attr(asList, "key");
  } else {
    res$key <- attr(asList, "key");
  }
  
  attr(res, "authorDf") <-
    as.data.frame(bibentry$author);
  
  return(res);
  
}

#' Convert [utils::person()] object to a data frame
#' 
#' This function takes a [utils::person()] object and converts it to a data frame.
#' 
#' @param x The [utils::person()] object
#' @param ... Any additional argument to pass to [base::data.frame()].
#'
#' @export
#' @return A data frame with one row for each person.
mbf_as.data.frame.person <- function(x, ...) {
  
  if (!inherits(x, "person")) {
    stop("As `x`, you can only pass an object of class `person`. ",
         "You passed an object of class(s) ", vecTxtQ(class(x)));
  }
  
  if (length(x) > 1) {
    return(
      metabefor::rbind_df_list(
        lapply(
          x,
          as.data.frame
        )
      )
    );
  }

  res <- data.frame(
    given = ifelse(is.null(x$given), "", x$given),
    family = ifelse(is.null(x$family), "", x$family),
    email = ifelse(is.null(x$email), "", x$email),
    role = ifelse(is.null(x$role), "", x$role),
    comment = ifelse(is.null(x$comment), "", x$comment),
    ...
  );
  
  return(res);
  
}
