#' Get a single value from a study tree, list of trees, or studies object
#'
#' @param x 
#' @param entityId The entity identifier of the value to get
#' @param lookInValueLists Whether to also look inside value lists
#' @param returnDf Whether to return a data frame or not
#'
#' @return A list or a dataframe (if `returnDf` is `TRUE`)
#'
#' @examples
#' 
#' @export
#' @rdname get_singleValue
get_singleValue_fromTree <- function(x,
                                     entityId,
                                     lookInValueLists = TRUE,
                                     returnDf = FALSE,
                                     silent = metabefor::opts$get("silent")) {
  
  if (inherits(x, "rxs") && inherits(x, "Node")) {
    
    foundNode <- data.tree::FindNode(
        x,
        entityId
      );
    
    if (!is.null(foundNode)) {
      
      if (returnDf) {
        return(
          as.data.frame(
            padVectors(
              foundNode$value
            )
          )
        );
      } else {
        return(foundNode$value);
      }
      
    } else if (lookInValueLists) {
      
      valuesFromValueLists <-
        x$Get(
          function(node) {
            return(node$value[[entityId]]);
          },
          filterFun = function(node) {
            if (is.null(names(node$value))) {
              return(FALSE);
            } else {
              return(entityId %in% names(node$value));
            }
          }
        );
      
      if (returnDf) {
        
        return(
          as.data.frame(
            padVectors(
              valuesFromValueLists
            )
          )
        );

      } else {
        return(valuesFromValueLists);
      }
    } else {
      if (!silent) {
        cat("\nDid not find an entity with identier ('name')", entityId);
      }
      return(invisible(NULL));
    }
  } else {
    stop("The object you passed is not a study tree! It has class(es) ",
         vecTxtQ(class(x)), ".");
  }
}

#' @export
#' @rdname get_singleValue
get_singleValue_fromTreeList <- function(x,
                                         entityId,
                                         returnDf = TRUE,
                                         nullValue = 0,
                                         naValue = NULL,
                                         warningValues = list(NULL, NA),
                                         warningFunctions = NULL,
                                         silent = metabefor::opts$get("silent")) {
  
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
      cat("Some rxsTrees are invalid, specifically: ",
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
          cat0("\nStarting to extract entity with identifier '", entityId,
               "' from study ", i, "...");
        }
        return(
          get_singleValue_fromTree(
            x = x[[i]],
            entityId = entityId
          )
        );
      }
    );

  if (!is.null(names(x))) {
    ids <- names(x);
  } else {
    ids <- seq_along(res);
  }
  
  names(res) <- ids;
  
  if (!is.null(warningFunctions)) {
    for (currentFunction in warningFunctions) {
      warningFunctionResult <-
        currentFunction(res);
      msg <- paste0(
        warningFunctionResult,
        collapse = "\n"
      );
      if (!silent) {
        cat(msg);
      } else {
        warning(msg);
      }
    }
  }
  
  res_is_lists <-
    any(
      unlist(
        lapply(
          res,
          is.list
        )
      )
    );
  
  if ((!res_is_lists) && !is.null(warningValues)) {
    for (i in seq_along(warningValues)) {
      warningValueResult <-
        unlist(lapply(res, `==`, warningValues[[i]]));
      if ((!all(is.na(warningValueResult))) && any(warningValueResult)) {
        msg <- paste0(
          "\nWhen checking for matches with warning value '",
          warningValues[[i]], "', I found matches for studies ",
          vecTxtQ(ids[which(warningValueResult)]), "."
        );
        if (!silent) {
          cat(msg);
        } else {
          warning(msg);
        }
      }
    }
  }

  res_is_null <-
    which(
      unlist(
        lapply(
          res,
          is.null
        )
      )
    );
  
  res_is_na <-
    which(
      unlist(
        lapply(
          res,
          is.na
        )
      )
    );
  
  if (!is.null(nullValue)) {
    res[res_is_null] <- nullValue;
  }
  
  if (!is.null(naValue)) {
    res[res_is_na] <- naValue;
  }
  
  if (returnDf) {
    resDfList <-
      mapply(
        function(resVector, id) {

          ### In case we have a value list
          resVector <- unlist(resVector,
                              recursive = FALSE);
          
          ### In case a list was stored in the value list
          if (is.list(resVector)) {
            stop("Encountered an extracted `value` that is a list with ",
                 "at least 2 levels - no way to automatically process this.");
          }
          
          if (!is.null(names(resVector))) {
            resVectorNames <- names(resVector);
          } else {
            resVectorNames <- rep(NA, length(resVector));
          }

          resDf <-
            data.frame(
              rep(id, length(resVector)),
              resVectorNames,
              resVector
            );
          
          names(resDf) <-
            c("Id", "Field", entityId);
          
          return(resDf);
          
        },
        res,
        ids,
        SIMPLIFY = FALSE
      );
    
    resDf <-
      metabefor::rbind_df_list(resDfList);
    
    return(resDf);
  } else {
    return(res);
  }
  
}

#' @export
#' @rdname get_singleValue
get_singleValue <- function(x,
                            entityId,
                            returnDf = TRUE,
                            silent = metabefor::opts$get("silent")) {
  
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    return(
      get_singleValue_fromTreeList(
        x = x$rxsTrees,
        entityId = entityId,
        returnDf = returnDf,
        silent = silent
      )
    );
  } else {
    stop("The object you passed is not an object with parsed Rxs files! It has class(es) ",
         vecTxtQ(class(x)), ".");
  }
  
}
