#' @export
#' @param recursive Whether to read directories recursively
#' @rdname rock_import_and_export
rxs_import_from_rock <- function(x,
                                 input = NULL,
                                 filenameRegex = NULL,
                                 recursive = TRUE,
                                 rxsSourceId = metabefor::opts$get("rockInterfacing_rxsSourceId"),
                                 rxsEntityId = metabefor::opts$get("rockInterfacing_rxsEntityId")) {

  ### Configure the rock package
  rock::opts$set(
    idRegexes = c(
      rock::opts$defaults$idRegexes,
      setNames(
        c(paste0("\\[\\[", rxsSourceId, "[=:]([a-zA-Z0-9_]+)\\]\\]"),
          paste0("\\[\\[", rxsEntityId, "[=:]([a-zA-Z0-9_]+)\\]\\]")),
        nm = c(rxsSourceId, rxsEntityId)
      )
    ),
    persistentIds = c(
      rock::opts$defaults$persistentIds,
      c(rxsSourceId, rxsEntityId)
    )
  );
  
  ###-----------------------------------------------------------------------------
  ### Import source(s) and prepare coding for import into Rxs tree
  ###-----------------------------------------------------------------------------
  
  if (dir.exists(input)) {
    parsedSource <- rock::parse_sources(
      path = input,
      regex = filenameRegex,
      recursive = recursive
    );
  } else if (file.exists(input)) {
    parsedSource <- rock::parse_source(
      file = input
    );
  } else {
    stop("The `input` you specified ('", input, "') is neither",
         "an existing directory nor an existing file.");
  }

  ### Get all codeIds, all sourceIds and all entity Ids
  allCodeIds <- parsedSource$convenience$codings;
  allSourceIds <- unique(parsedSource$mergedSourceDf[, 'rxsSourceId']);
  allEntityIds <- unique(parsedSource$mergedSourceDf[, 'rxsEntityId']);
  
  ### Name the sourceId and entityId vector
  ### so that lapply also assigns these names
  names(allSourceIds) <- allSourceIds;
  names(allEntityIds) <- allEntityIds;
  
  ### Create conveniently organised object
  organisedCoding <-
    lapply(
      allSourceIds,
      function(currentSourceId) {
        return(
          lapply(
            allEntityIds,
            function(currentEntityId) {
              return(
                as.list(
                  parsedSource$mergedSourceDf[
                    parsedSource$mergedSourceDf[, 'rxsSourceId'] == currentSourceId &
                      parsedSource$mergedSourceDf[, 'rxsEntityId'] == currentEntityId,
                    allCodeIds
                  ]
                )
              );
            }
          )
        );
      }
    );
  
  ###-----------------------------------------------------------------------------
  ### Add coding to Rxs tree
  ###-----------------------------------------------------------------------------
  
  for (currentSourceId in allSourceIds) {
    
    for (currentEntityId in allEntityIds) {
      
      targetEntityNode <-
        data.tree::FindNode(
          x$rxsTrees[[currentSourceId]],
          currentEntityId
        );
      
      for (currentCodeId in allCodeIds) {
        
        ### Compose new name --- NOTE: NEEDS TO BE DYNAMIC IN THE FUNCTION!!!
        newName <- paste0(currentEntityId, "_", currentCodeId);
        
        ### Now we will add the codings as siblings
        targetEntityNode$AddSibling(
          newName,
          value =
            organisedCoding[[currentSourceId]][[currentEntityId]][[currentCodeId]]
        );
        
      }
      
    }
    
  }
  
  return(invisible(x));
  
}
                                 