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
#' @rdname getSingleValue
getSingleValue_fromTree <- function(x,
                                    entityId,
                                    lookInValueLists = TRUE,
                                    silent = metabefor::opts$get("silent")) {
  
  if (inherits(x, "rxs") && inherits(x, "Node")) {
    foundNode <- data.tree::FindNode(
        x,
        entityId
      );
    if (!is.null(foundNode)) {
      return(foundNode$value);
    } else if (lookInValueLists) {
      valuesFromValueLists <-
        x$Get(
          function(node) {
            return(node$value[[entityId]]);
          },
          filterFun = function(node) {
            return(entityId %in% names(node$value));
          }
        )
      return(valuesFromValueLists);
    } else {
      if (!silent) {
        cat("\nDid not find an entity with identier ('name')", entityId);
      }
      return(invisible(NULL));
    }
  } else {
    stop("Wrong class!");
  }
}

#' @export
#' @rdname getSingleValue
getSingleValue_fromTreeList <- function(x,
                                        entityId,
                                        returnDf = TRUE) {
  
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
    warning("Some rxsTrees are invalid, specifically: ",
            metabefor::vecTxtQ(names(usableElements)[which(!usableElements)]),
            ".");
  }
  
  x <- x[usableElements];
  
  res <-
    lapply(
      x,
      getSingleValue_fromTree,
      entityId = entityId
    );
  
  if (!is.null(names(x))) {
    ids <- names(x);
  } else {
    ids <- seq_along(res);
  }
  
  if (returnDf) {
    resDfList <-
      mapply(
        function(resVector, id) {
          
          ### Just in case
          resVector <- unlist(resVector);
          
          resDf <-
            data.frame(
              rep(id, length(resVector)),
              resVector
            );
          
          names(resDf) <-
            c("Id", entityId);
          
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
#' @rdname getSingleValue
getSingleValue <- function(x,
                           entityId,
                           returnDf = TRUE) {
  
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    return(
      getSingleValue_fromTreeList(
        x = x$rxsTrees,
        entityId = entityId,
        returnDf = returnDf
      )
    );
  } else {
    stop("Wrong class!");
  }
  
}
