#' Get a value list as data frame from a study tree, list of trees, or studies object
#'
#' @param x The study tree, list of trees, or studies object
#' @param requiredFields Fields that have to exist in 
#' @param flattenVectorsInDf When returning a data frame, whether to flatten
#' vectors into a single character string value, or whether to explode into
#' multiple rows.
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
                                             flattenVectorsInDf = TRUE,
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
      flattenVectorsInDf = flattenVectorsInDf,
      returnLongDf = FALSE,
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
                               flattenVectorsInDf = TRUE,
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
              flattenVectorsInDf = flattenVectorsInDf,
              silent = silent
            );
          if (is.data.frame(res)) {
            if (!silent) {
              cat0("Data frame returned.\n");
            }
            ### Set study identifier
            res$studyId <- i;
            ### Return result
            return(res);
          } else {
            if (!silent) {
              cat0("No data frame returned.\n");
            }
            return(NULL);
          }
        }
      );
    
    resThatAreValid <-
      unlist(
        lapply(
          res,
          function(x) {
            return(!(is.null(x) || is.na(x)));
          }
        )
      );

    if (!silent) {
      cat0("\nStarting to combine ",
           length(res[resThatAreValid]),
           " data frames obtained from ",
           length(res), " sources...\n");
    }
    
    res <-
      rbind_df_list(
        res[resThatAreValid]
      );
    
    cat0("Returning a data frame with ", nrow(res),
         " rows and ", ncol(res), " columns.\n");
    
    return(res);
    
  } else {
    stop("The object you passed is not an object with parsed Rxs files! It has class(es) ",
         vecTxtQ(class(x)), ".");
  }
  
}
