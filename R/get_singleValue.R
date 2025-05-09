#' Get a single value from a Rxs tree, list of trees, or full Rxs project object
#'
#' @param x The tree, tree list, or full Rxs project object.
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
#' @param fieldname_regex_alwaysFlatten A regular expression to force
#' flattening of fields regardless of matching to other regular expressions.
#' @param returnLongDf Whether to return a long ('tidy') data frame, with all
#' values on a separate row, or a wide ('multivariate') data frame, where
#' columns represent different variables. Setting this to `FALSE` is not yet
#' supported.
#' @param nullValue,naValue Values to return when encountering `NULL` or `NA`.
#' @param warningValues Values to return warnings for.
#' @param warningFunctions A list of functions to run on the values that can
#' return a character value - if they do, that is shown as a warning or printed
#' (depending on the value of `silent`).
#' @param silent Whether to be silent or chatty.
#'
#' @return A list or a dataframe (if `returnDf` is `TRUE`).
#'
#' @export
#' 
#' @rdname get_singleValue
get_singleValue_fromTree <- function(x,
                                     entityId,
                                     lookInValueLists = TRUE,
                                     returnDf = FALSE,
                                     flattenVectorsInDf = TRUE,
                                     pathString_regex_flatten = NULL,
                                     pathString_regex_explode = NULL,
                                     fieldname_regex_alwaysFlatten = NULL,
                                     returnLongDf = TRUE,
                                     pathString_regex_select = ".*",
                                     silent = metabefor::opts$get("silent")) {

  if (inherits(x, "rxs") && inherits(x, "Node")) {
    
    ### Get the source identifier of this tree, if available
    if ((!is.null(x$root$rxsMetadata) &&
         (!is.null(x$root$rxsMetadata)))) {
      sourceId <- x$root$rxsMetadata$id;
    } else {
      sourceId <- NA;
    }

    foundNode <- data.tree::FindNode(
        x,
        entityId
      );
    
    if (!is.null(foundNode)) {
      
      ### Get path string so we don't have to get it from the node repeatedly
      nodePathString <- foundNode$pathString;

      if ((!is.null(pathString_regex_select)) &&
            (!grepl(pathString_regex_select, nodePathString))) {
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

      if ("value" %in% (foundNode$attributes)) {
        
        foundValue <- foundNode$value;
        
      } else {

        msg("\nFound a node with identier ('name') '", entityId,
            "', and its path string ('",
            foundNode$pathString, "') matched the ",
            "specified pathString_regex_select ('", pathString_regex_select,
            "'), but it did not contain a `value`; most likely, ",
            "it is a container entity. Returning NULL.",
            silent = silent);

        return(NULL);

      }

      if (flattenTheVector) {
        
        ### Flatten vectors to strings with VecTxtQ
        res <- flattenNodeValues(foundValue);
        
      } else {
        
        if (returnLongDf) {
          ### Split vectors to different rows
          res <- splitVectors(foundValue,
                              fieldname_regex_alwaysFlatten = fieldname_regex_alwaysFlatten);
        } else {
          ### Pad all elements with length > 1 to the same
          ### length to allow conversion to data frame
          res <- padVectors(foundValue,
                            fieldname_regex_alwaysFlatten = fieldname_regex_alwaysFlatten);
        }

      }

      ### Start returning result

      if (returnDf && returnLongDf) {
        ### Long ('tidy') data frame, with all values in one column
        
        if (is.list(foundValue)) {
          ### (we look at foundNode$value instead of res because res may
          ###  have become a vector, even if foundNode$value is a list)
          
          ### List of entities in a clustering entity
          resDf <-
            rbind_df_list(
              lapply(
                res,
                anything_to_tidyDf,
                sourceId = sourceId,
                entityId = entityId
              ),
              suppressNonDfError = TRUE
            );
          
          ### Set entity names, take potential vectors into account
          resDf$entityId =
            unname(
              unlist(
                mapply(
                  rep,
                  x = names(res),
                  times = unlist(lapply(res, length)),
                  SIMPLIFY = FALSE
                )
              )
            );
          
          return(resDf);
            
        } else {
          
          ### Vector from a single entity
          return(
            anything_to_tidyDf(
              res,
              sourceId = sourceId,
              entityId = entityId
            )
          );
          
        }

      } else if (returnDf) {
        
        if (is.list(foundNode$value)) {
          ### (we look at foundNode$value instead of res because res may
          ###  have become a vector, even if foundNode$value is a list)
          
          if (is.list(res)) {
            res <- as.data.frame(res);
          } else {
            ### If it's a character vector, we have to convert it to a list
            ### first
            res <- as.data.frame(as.list(res));
          }

          res <- cbind(data.frame(sourceId = rep(sourceId, nrow(res))),
                       res);
          
          return(res);
          
        } else {
          
          ### Wide dataframe, with one column for each entity
          
          if (length(res) > 0) {
            resDf <-
              data.frame(rep(sourceId, length(res)),
                         res);
          } else {
            resDf <-
              data.frame(sourceId,
                         "_NULL_");
          }

          names(resDf) <- c('sourceId', entityId);

          return(resDf);
          
        }

      } else {
        
        ### Don't return a data frame; just return 'raw'
        
        return(res);
        
      }
      
    } else if (lookInValueLists) {
      
      msg("Did not find a node with entity identifier '",
          entityId, "', but `lookInValueLists` is TRUE, so ",
          "will now proceed to look in value lists (i.e. clustering ",
          "entities), so that I can also find entities with the identifier ",
          "you supplied if they are clustered entities.\n",
          silent=silent);
      
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
        
          msg("Found at least one entity with identier ('name') '", entityId,
              "' in clustering entities (value lists).\n");
        
          if (returnLongDf) {
            ### Pad all elements with length > 1 to the same
            ### length to allow conversion to data frame
            res <- padVectors(
              valuesFromValueLists,
              fieldname_regex_alwaysFlatten = fieldname_regex_alwaysFlatten);
          } else {
            res <- splitVectors(
              valuesFromValueLists,
              fieldname_regex_alwaysFlatten = fieldname_regex_alwaysFlatten);
          }
          
        # }
        
        ### Start returning result
        
        if (returnDf && returnLongDf) {
          
          msg("Returning result in a long ('tidy') dataframe (with ",
              "all values in one column).\n\n",
              silent=silent);

          ### Long ('tidy') data frame, with all values in one column
          return(
            anything_to_tidyDf(
              res,
              sourceId = sourceId,
              entityId = entityId
            )
          );
          
        } else if (returnDf) {
          
          msg("Returning result in a wide dataframe (with the values ",
              "in separate columns).\n\n",
              silent=silent);
          
          ### I don't know yet whether this is necessary here; it's
          ### included a few lines higher, 201-208, but not sure we
          ### need it here, too.
          
          # if (is.list(res)) {
          #   res <- as.data.frame(res);
          # } else {
          #   ### If it's a character vector, we have to convert it to a list
          #   ### first
          #   res <- as.data.frame(as.list(res));
          # }
          
          ### Wide dataframe, with one column for each entity
          res <-
            data.frame(rep(sourceId, length(res)),
                       res);
          names(res) <- c('sourceId', entityId);
          return(res);
          
        } else {

          msg("Returning result without placing it in a data frame.\n\n",
              silent=silent);
          
          ### Don't return a data frame; just return 'raw'
          
          return(res);
          
        }        

      } else {
        
        msg("Did not find entity with identier ('name') '", entityId,
            "' in clustering entities (value lists).\n",
            silent=silent);
        
        return(valuesFromValueLists);
      }
      
    } else {
      if (!silent) {
        cat("\nDid not find an entity with identier ('name') '", entityId,
            "', and did not look in value lists (clustering entities).\n");
      }
      return(invisible(NULL));
    }
  } else {
    stop("The object you passed is not a Rxs tree! It has class(es) ",
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
                                         lookInValueLists = TRUE,
                                         flattenVectorsInDf = TRUE,
                                         warningValues = list(NULL, NA),
                                         warningFunctions = NULL,
                                         pathString_regex_select = ".*",
                                         pathString_regex_flatten = NULL,
                                         pathString_regex_explode = NULL,
                                         fieldname_regex_alwaysFlatten = NULL,
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
               "' from source ", i, "...");
        }
        return(
          get_singleValue_fromTree(
            x = x[[i]],
            entityId = entityId,
            flattenVectorsInDf = flattenVectorsInDf,
            pathString_regex_select = pathString_regex_select,
            pathString_regex_flatten = pathString_regex_flatten,
            pathString_regex_explode = pathString_regex_explode,
            fieldname_regex_alwaysFlatten = fieldname_regex_alwaysFlatten,
            silent = silent,
            returnDf = returnDf,
            returnLongDf = returnLongDf
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
          if (nrow(res[[x]]) > 0) {
            res[[x]]$sourceId <- x;
          }
          return(res[[x]]);
        }
      );

    return(
      metabefor::rbind_df_list(
        res,
        suppressNonDfError = TRUE
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
            warningValues[[i]], "', I found matches for sources ",
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
    
    res_no_rows <-
      which(
        unlist(
          lapply(
            res,
            nrow
          )
        ) == 0
      );
    
    if (!is.null(nullValue)) {
      res[res_is_null] <- nullValue;
    }
    
    if (!is.null(naValue)) {
      res[res_is_na] <- naValue;
      res[res_no_rows] <-
        data.frame(value = naValue);
    }

    if (returnDf) {
      resDfList <-
        mapply(
          anything_to_tidyDf,
          res,
          ids,
          MoreArgs = list(entityId = entityId),
          SIMPLIFY = FALSE
        );
  
      resDf <-
        metabefor::rbind_df_list(
          resDfList,
          suppressNonDfError = TRUE
        );
  
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
                            lookInValueLists = TRUE,
                            pathString_regex_select = ".*",
                            pathString_regex_flatten = NULL,
                            pathString_regex_explode = NULL,
                            fieldname_regex_alwaysFlatten = NULL,
                            silent = metabefor::opts$get("silent")) {
  
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    return(
      get_singleValue_fromTreeList(
        x = x$rxsTrees,
        entityId = entityId,
        returnDf = returnDf,
        flattenVectorsInDf = flattenVectorsInDf,
        lookInValueLists = TRUE,
        pathString_regex_select = pathString_regex_select,
        pathString_regex_flatten = pathString_regex_flatten,
        pathString_regex_explode = pathString_regex_explode,
        fieldname_regex_alwaysFlatten = fieldname_regex_alwaysFlatten,
        returnLongDf = returnLongDf,
        silent = silent
      )
    );
  } else if (inherits(x, "rxs") && inherits(x, "rxsObject")) {
    return(
      get_singleValue_fromTree(
        x = x,
        entityId = entityId,
        lookInValueLists = lookInValueLists,
        returnDf = returnDf,
        flattenVectorsInDf = flattenVectorsInDf,
        pathString_regex_flatten = pathString_regex_flatten,
        pathString_regex_explode = pathString_regex_explode,
        fieldname_regex_alwaysFlatten = fieldname_regex_alwaysFlatten,
        returnLongDf = returnLongDf,
        pathString_regex_select = pathString_regex_select,
        silent = silent
      )
    );
  } else {
    stop("The object you passed is not an object with parsed Rxs files! It has class(es) ",
         vecTxtQ(class(x)), ".");
  }
  
}
