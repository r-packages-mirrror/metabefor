anything_to_tidyDf <- function(anything,
                               sourceId,
                               entityId) {
  
  if (is.vector(anything)) {

    if (length(anything) == 1) {
      resDf <-
        data.frame(
          sourceId = rep(unname(sourceId), length(anything)),
          entityId = unname(entityId),
          value = unname(anything)
        );
    } else {
      
      resDf <-
        data.frame(
          sourceId = rep(unname(sourceId), length(anything)),
          entityId = rep(unname(entityId), length(anything)),
          value = unname(anything)
        );
    }

  } else if (is.data.frame(anything) &&
             (ncol(anything) == 3) &&
             (names(anything) == c("sourceId", "entityId", "value"))) {
    
    resDf <- anything;
        
  } else if (is.data.frame(anything) &&
      (ncol(anything) == 1) &&
      (is.character(row.names(anything)))) {

    if (nrow(anything) == 1) {
      resDf <-
        data.frame(
          sourceId = rep(sourceId, nrow(anything)),
          entityId = entityId,
          value = anything[[1]]
        );
    } else {
      resDf <-
        data.frame(
          sourceId = rep(sourceId, nrow(anything)),
          entityId = row.names(anything),
          value = anything[[1]]
        );
    }

  } else {

    ### In case we have a value list
    anything <- unlist(anything,
                       recursive = FALSE);

    ### In case a list was stored in the value list
    if (is.list(anything)) {
      stop("Encountered an extracted `value` that is a list with ",
           "at least 2 levels - no way to automatically process this.");
    }

    if (!is.null(names(anything))) {
      resVectorNames <- names(anything);
    } else {
      resVectorNames <- rep(NA, length(anything));
    }

    resDf <-
      data.frame(
        sourceId = rep(sourceId, length(anything)),
        entityId = resVectorNames,
        value = anything
      );

  }

  if (nrow(resDf) == 0) {
    resDf <- NULL;
  } else {
    names(resDf) <-
      #c("Id", "Field", entityId);
      c("sourceId", "entityId", "value");
  }
  
  row.names(resDf) <- NULL;

  return(resDf);

}
