#' Get a single value from a study tree, list of trees, or studies object
#'
#' @param x 
#' @param entityId The entity identifier of the value to get
#' @param lookInValueLists Whether to also look inside value lists
#' @param returnDf Whether to return a data frame or not
#' @param pathString_regex_select Regex that the target entities' path strings
#' have to match (otherwise, the entity is excluded)
#' @param flattenVectorsInDf The default action to apply for values not matching
#' one of `pathString_regex_flatten` and `pathString_regex_explode`: to flatten
#' by default, pass `TRUE`; to explode by default, pass `FALSE`.
#' @param pathString_regex_flatten,pathString_regex_explode Regular expressions
#' matched against each entity node's path string (i.e. its path from the root,
#' delimited by slashes). Vectors in entity nodes matching
#' `pathString_regex_flatten` will be flattened into a single character
#' string value; vectors in entity nodes matching `pathString_regex_explode`
#' will be exploded into multiple rows.
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
                                     flattenVectorsInDf = TRUE,
                                     pathString_regex_flatten = NULL,
                                     pathString_regex_explode = NULL,
                                     fieldname_regex_neverExplode = NULL,
                                     returnLongDf = TRUE,
                                     pathString_regex_select = ".*",
                                     silent = metabefor::opts$get("silent")) {

  if (inherits(x, "rxs") && inherits(x, "Node")) {
    
    foundNode <- data.tree::FindNode(
        x,
        entityId
      );
    
    if (!is.null(foundNode)) {
      
      ### Get path string so we don't have to get it from the node repeatedly
      nodePathString <- foundNode$pathString;

      if (!grepl(pathString_regex_select, nodePathString)) {
        if (!silent) {
          cat("\nFound a node with identier ('name') '", entityId,
              "', but its path string ('",
              foundNode$pathString, "') did not match the ",
              "specified pathString_regex_select ('", pathString_regex_select,
              "').");
        }
        return(invisible(NULL));
      } else {
        if (!silent) {
          cat0("\nFound a node with identier ('name') '", entityId,
              "': ");
        }
      }
      
      ### Decide whether in the end we should flatten vectors
      ### Decide whether in the end we should flatten vectors
      flattenTheVector <- flattenVectorsInDf;
      if (!silent) {
        cat0("flattenVectorsInDf is ", as.character(flattenVectorsInDf),
             ", and the node's path string (`", nodePathString, "`) ");
      }
      if ((!is.null(pathString_regex_flatten)) &&
          (grepl(pathString_regex_flatten, nodePathString))) {
        flattenTheVector <- TRUE;
        if (!silent) {
          cat0("matched the 'flatten' path string regex (`",
               pathString_regex_flatten, "`), ");
        }
      } else {
        if (!silent) {
          cat0("did not match the 'flatten' path string regex (`",
               pathString_regex_flatten, "`), ");
        }
      }
      if ((!is.null(pathString_regex_explode)) &&
          (grepl(pathString_regex_explode, nodePathString))) {
        flattenTheVector <- FALSE;
        if (!silent) {
          cat0("matched the 'explode' path string regex (`",
               pathString_regex_explode, "`), ");
        }
      } else {
        if (!silent) {
          cat0("did not match the 'explode' path string regex (`",
               pathString_regex_explode, "`), ");
        }
      }
      if (!silent) {
        cat0("so vectors will be ",
             ifelse(flattenTheVector, "flattened", "exploded"),
             ".\n");
      }
      
      ### We found a node with this name, return its value
      
      if (flattenTheVector) {
        
        ### Flatten vectors to strings with VecTxtQ
        res <- flattenNodeValues(foundNode$value);
        
      } else {
        
        if (returnLongDf) {
          ### Split vectors to different rows
          res <- splitVectors(foundNode$value,
                              fieldname_regex_neverExplode = fieldname_regex_neverExplode);
        } else {
          ### Pad all elements with length > 1 to the same
          ### length to allow conversion to data frame
          res <- padVectors(foundNode$value,
                            fieldname_regex_neverExplode = fieldname_regex_neverExplode);
        }

      }
      
      ### Start returning result

      if (returnDf && returnLongDf) {
        
        ### Long ('tidy') data frame, with all values in one column
        if (is.list(res)) {
          
        }

        return(data.frame(value = res));
        
      } else if (returnDf) {

        ### Wide dataframe, with one column for each entity
        return(
          do.call(
            data.frame,
            as.list(res)
          )
        );
        
      } else {
        
        ### Don't return a data frame; just return 'raw'
        
        return(res);
        
      }
      
    } else if (lookInValueLists) {
      
      ### We didn't find a node with this name, but will look in
      ### value lists ('clustered entities')
      
      valuesFromValueLists <-
        x$Get(
          function(node) {
            
            ### Get path string so we don't have to get it from the node repeatedly
            nodePathString <- node$pathString;
            
            ### Decide whether in the end we should flatten vectors
            flattenTheVector <- flattenVectorsInDf;
            if (!is.null(pathString_regex_flatten)) {
              if (grepl(pathString_regex_flatten, nodePathString)) {
                flattenTheVector <- TRUE;
              }
            }
            if (!is.null(pathString_regex_explode)) {
              if (grepl(pathString_regex_explode, nodePathString)) {
                flattenTheVector <- FALSE;
              }
            }

            ### Flatten vectors to strings with VecTxtQ
            if (flattenTheVector) {
              res <- flattenNodeValues(node$value[[entityId]]);
            } else {
              res <- node$value[[entityId]];
            }

            return(res);
          },
          filterFun = function(node) {
            if (is.null(names(node$value))) {
              return(FALSE);
            } else {
              return(entityId %in% names(node$value));
            }
          }
        );

      if (length(valuesFromValueLists) > 0) {
        
        ### This should be redundant now that we flatten in the lambda
        ### function we use when creating valuesFromValueLists a few lines
        ### higher
        
        # if (flattenVectorsInDf) {
        #   
        #   res <- flattenNodeValues(valuesFromValueLists);
        #   
        # } else {

          if (returnLongDf) {
            ### Pad all elements with length > 1 to the same
            ### length to allow conversion to data frame
            res <- padVectors(foundNode$value,
                              fieldname_regex_neverExplode = fieldname_regex_neverExplode);
          } else {
            res <- splitVectors(foundNode$value,
                                fieldname_regex_neverExplode = fieldname_regex_neverExplode);
          }
          
        # }
        
        ### Start returning result
        
        if (returnDf && returnLongDf) {
          
          ### Long ('tidy') data frame, with all values in one column
          return(data.frame(res));
          
        } else if (returnDf) {
          
          ### Wide dataframe, with one column for each entity
          return(
            do.call(
              data.frame,
              as.list(res)
            )
          );
          
        } else {
          
          ### Don't return a data frame; just return 'raw'
          
          return(res);
          
        }        

      } else {
        return(valuesFromValueLists);
      }
      
    } else {
      if (!silent) {
        cat("\nDid not find an entity with identier ('name') '", entityId,
            "'.");
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
                                         flattenVectorsInDf = TRUE,
                                         warningValues = list(NULL, NA),
                                         warningFunctions = NULL,
                                         pathString_regex_select = ".*",
                                         pathString_regex_flatten = NULL,
                                         pathString_regex_explode = NULL,
                                         fieldname_regex_neverExplode = NULL,
                                         returnLongDf = TRUE,
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
            entityId = entityId,
            flattenVectorsInDf = flattenVectorsInDf,
            pathString_regex_select = pathString_regex_select,
            pathString_regex_flatten = pathString_regex_flatten,
            pathString_regex_explode = pathString_regex_explode,
            fieldname_regex_neverExplode = fieldname_regex_neverExplode,
            silent = silent,
            returnDf = returnDf
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

  if (returnDf && (!returnLongDf)) {

    res <-
      lapply(
        names(res),
        function(x) {
          res[[x]]$sourceId <- x;
          return(res[[x]]);
        }
      );

    return(
      metabefor::rbind_df_list(
        res
      )
    );
    
  } else {
    
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
}

#' @export
#' @rdname get_singleValue
get_singleValue <- function(x,
                            entityId,
                            returnDf = TRUE,
                            flattenVectorsInDf = TRUE,
                            returnLongDf = TRUE,
                            pathString_regex_select = ".*",
                            pathString_regex_flatten = NULL,
                            pathString_regex_explode = NULL,
                            fieldname_regex_neverExplode = NULL,
                            silent = metabefor::opts$get("silent")) {
  
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    return(
      get_singleValue_fromTreeList(
        x = x$rxsTrees,
        entityId = entityId,
        returnDf = returnDf,
        flattenVectorsInDf = flattenVectorsInDf,
        pathString_regex_select = pathString_regex_select,
        pathString_regex_flatten = pathString_regex_flatten,
        pathString_regex_explode = pathString_regex_explode,
        fieldname_regex_neverExplode = fieldname_regex_neverExplode,
        returnLongDf = returnLongDf,
        silent = silent
      )
    );
  } else {
    stop("The object you passed is not an object with parsed Rxs files! It has class(es) ",
         vecTxtQ(class(x)), ".");
  }
  
}