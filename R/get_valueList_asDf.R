#' Get a value list as data frame from an Rxs tree, list of trees, or full Rxs project
#' 
#' A value list is another name for the values of the entities within a clustering
#' entity. This function retrieves all those values and returns them in a data
#' frame.
#'
#' @param x The Rxs tree, list of trees, or full Rxs project
#' @param requiredFields Fields that have to exist in the target entities
#' (otherwise, the entity is excluded)
#' @param pathString_regex_select Regex that the target entities' path strings have to
#' match (otherwise, the entity is excluded)
#' @param flattenVectorsInDf The default action to apply for values not matching
#' one of `pathString_regex_flatten` and `pathString_regex_explode`: to flatten
#' by default, pass `TRUE`; to explode by default, pass `FALSE`.
#' @param pathString_regex_flatten,pathString_regex_explode Regular expressions
#' matched against each entity node's path string (i.e. its path from the root,
#' delimited by slashes). Vectors in entity nodes matching
#' `pathString_regex_flatten` will be flattened into a single character
#' string value; vectors in entity nodes matching `pathString_regex_explode`
#' will be exploded into multiple rows.
#' @param fieldname_regex_alwaysFlatten A regular expression to force
#' flattening of fields regardless of matching to other regular expressions.
#' @param silent Whether to be quiet or chatty.
#'
#' @return A dataframe
#'
#' @export
#' @rdname get_valueList_asDf
get_valueList_asDf_fromRxsTree <- function(x,
                                           requiredFields = NULL,
                                           flattenVectorsInDf = TRUE,
                                           pathString_regex_select = NULL,
                                           pathString_regex_flatten = NULL,
                                           pathString_regex_explode = NULL,
                                           fieldname_regex_alwaysFlatten = NULL,
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
            if (is.null(pathString_regex_select)) {
              return(TRUE);
            } else {
              return(grepl(pathString_regex_select, node$pathString));
            }
          } else if (is.null(names(node$value))) {
            return(FALSE);
          } else if (all(requiredFields %in% names(node$value))) {
            if (is.null(pathString_regex_select)) {
              return(TRUE);
            } else {
              return(grepl(pathString_regex_select, node$pathString));
            }
          } else {
            return(FALSE);
          }
        }
      );
    
    if (length(targetNodes) == 0) {
      return(NULL);
      msg("\nFound no entity nodes that contained the required ",
          "fields (if any were specified), so returning NULL.\n",
          silent=silent);
    }
  
    targetNodeNames <-
      unlist(
        lapply(
          targetNodes,
          function(node) {
            return(node$name);
          }
        )
      );
    
    msg("\nFound the following entity nodes that contained the required ",
        "fields (if any were specified): ", vecTxtQ(targetNodeNames),
        ".\n", silent=silent);

    res <- lapply(
      targetNodeNames,
      get_singleValue_fromTree,
      x = x,
      returnDf = TRUE,
      flattenVectorsInDf = flattenVectorsInDf,
      pathString_regex_select = pathString_regex_select,
      pathString_regex_flatten = pathString_regex_flatten,
      pathString_regex_explode = pathString_regex_explode,
      fieldname_regex_alwaysFlatten = fieldname_regex_alwaysFlatten,
      returnLongDf = FALSE,
      silent = silent
    );
    names(res) <- targetNodeNames;
    
    uniqueColLengths <-
      uniqueDf_ncols(res, silent=silent);
    
    if (!silent) {
      cat0("\nStarting to combine ",
           length(res),
           " data frames with values into one data frame for this source...\n");
    }
    
    res <- rbind_df_list(res);
    
    if (!silent) {
      cat0("Done!\n");
    }
    
    return(res);
    
  } else {
    stop("The object you passed is not a Rxs tree! It has class(es) ",
         vecTxtQ(class(x)), ".");
  }

}

#' @export
#' @rdname get_valueList_asDf
get_valueList_asDf <- function(x,
                               requiredFields = NULL,
                               pathString_regex_select = NULL,
                               flattenVectorsInDf = TRUE,
                               pathString_regex_flatten = NULL,
                               pathString_regex_explode = NULL,
                               fieldname_regex_alwaysFlatten = NULL,
                               silent = metabefor::opts$get("silent")) {
  
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    
    x <- x$rxsTrees;
    treeNames <- names(x);
    
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
    xNames <- names(x);
    
    res <-
      lapply(
        names(x),
        function(i) {
          if (!silent) {
            cat0("\nStarting to process source ", i, "... ");
          }
          res <- 
            get_valueList_asDf_fromRxsTree(
              x = x[[i]],
              requiredFields = requiredFields,
              pathString_regex_select = pathString_regex_select,
              flattenVectorsInDf = flattenVectorsInDf,
              pathString_regex_flatten = pathString_regex_flatten,
              pathString_regex_explode = pathString_regex_explode,
              fieldname_regex_alwaysFlatten = fieldname_regex_alwaysFlatten,
              silent = silent
            );
          if (is.data.frame(res)) {
            if (!silent) {
              cat0("Data frame returned.\n");
            }
            ### Set source identifier
            res$sourceId <- i;
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
    names(res) <- xNames;
    
    resThatAreValid <-
      unlist(
        lapply(
          res,
          function(x) {
            return(!(is.null(x) || is.na(x)));
          }
        )
      );
    
    uniqueColLengths <-
      uniqueDf_ncols(res, silent=silent);
    
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
    
    if (!silent) {
      cat0("Returning a data frame with ", nrow(res),
           " rows and ", ncol(res), " columns.\n");
    }
    
    return(res);
    
  } else {
    stop("The object you passed is not an object with parsed Rxs files! It has class(es) ",
         vecTxtQ(class(x)), ".");
  }
  
}
