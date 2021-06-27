#' Get a value list as data frame from a study tree, list of trees, or studies object
#'
#' @param x The study tree, list of trees, or studies object
#' @param requiredFields Fields that have to exist in 
#' @param silent Whether to be quiet or chatty.
#'
#' @return A dataframe
#'
#' @examples
#' 
#' @export
#' @rdname get_valueList_asDf
get_valueList_asDf_fromStudyTree <- function(x,
                                             requiredFields = NULL,
                                             silent = metabefor::opts$get("silent")) {
  
  if (is.null(x)) {
    if (!silent) {
      cat0("The object you passed to get the value list from is NULL!");
    }
    return(invisible(NULL));
  }
  
  if (inherits(x, "rxs") && inherits(x, "Node")) {
    
    ###---------------------------------------------------------------------------
    ### Start looking for the target nodes
    ###---------------------------------------------------------------------------
    
    targetNodes <-
      data.tree::Traverse(
        x,
        filterFun = function(node) {
          if (is.null(node$value) ||
              is.na(node$value) ||
              (length(node$value) == 0)) {
            return(FALSE);
          } else if (!is.list(node$value)) {
            return(FALSE);
          } else if (is.null(requiredFields)) {
            return(TRUE);
          } else if (is.null(names(node$value))) {
            return(FALSE);
          } else if (all(requiredFields %in% names(node$value))) {
            return(TRUE);
          } else {
            return(FALSE);
          }
        }
      );

    targetNodeNames <-
      unlist(
        lapply(
          targetNodes,
          function(node) {
            return(node$name);
          }
        )
      );
    
    if (!silent) {
      cat0("\nFound the following entity nodes that contained the required ",
           "fields (if any were specified): ", vecTxtQ(targetNodeNames),
           ".\n");
    }
    
    res <- lapply(
      targetNodeNames,
      get_singleValue_fromTree,
      x = x,
      returnDf = TRUE,
      silent = silent
    );
    
    return(
      rbind_df_list(
        res
      )
    );
    
  } else {
    stop("The object you passed is not a study tree! It has class(es) ",
         vecTxtQ(class(x)), ".");
  }

}

#' @export
#' @rdname get_valueList_asDf
get_valueList_asDf <- function(x,
                               requiredFields = NULL,
                               silent = metabefor::opts$get("silent")) {
  
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    
    x <- x$rxsTrees;
    
    usableElements <-
      unlist(
        lapply(
          x,
          function(singleX) {
            return(
              inherits(singleX, "rxs") && inherits(singleX, "Node")
            );
          }
        )
      );
    
    if (any(!usableElements)) {
      if (!silent) {
        cat0("Some rxsTrees are invalid, specifically: ",
             metabefor::vecTxtQ(names(usableElements)[which(!usableElements)]),
             ".");
      } else {
        warning("Some rxsTrees are invalid, specifically: ",
                metabefor::vecTxtQ(names(usableElements)[which(!usableElements)]),
                ".");
      }
    }
    
    x <- x[usableElements];
    
    res <-
      lapply(
        names(x),
        function(i) {
          if (!silent) {
            cat0("\nStarting to process study ", i, "... ");
          }
          res <- 
            get_valueList_asDf_fromStudyTree(
              x = x[[i]],
              requiredFields = requiredFields,
              silent = silent
            );
          ### Set study identifier
          res$studyId <- i;
          ### Return result
          return(res);
        }
      );
    
    return(
      rbind_df_list(
        res)
    );
    
  } else {
    stop("The object you passed is not an object with parsed Rxs files! It has class(es) ",
         vecTxtQ(class(x)), ".");
  }
  
}
