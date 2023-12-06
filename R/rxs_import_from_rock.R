#' @export
#' @param recursive Whether to read directories recursively
#' @param silent Whether to be chatty or silent.
#' @param newEntityIds,newEntityIdCodeFirst To determine how the new entity
#' identifiers are composed, `newEntityIds` s used. This has to be a
#' [base::sprintf] `fmt` string containing exactly two occurrences of "`%s`".
#' If `newEntityIdCodeFirst` is `FALSE`, the first `%s` is replaced by the
#' the original entity identifier (stored using the `rxsEntityId` class
#' instance identifier in the ROCK format), and the second by the code. If it is
#' `TRUE`, this order is reversed.
#' @rdname rock_import_and_export
rxs_import_from_rock <- function(x,
                                 input = NULL,
                                 newEntityIds = "%s_%s",
                                 newEntityIdCodeFirst = FALSE,
                                 filenameRegex = NULL,
                                 recursive = TRUE,
                                 silent = metabefor::opts$get("silent"),
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
      recursive = recursive,
      silent = silent
    );
  } else if (file.exists(input)) {
    parsedSource <- rock::parse_source(
      file = input,
      silent = silent
    );
  } else {
    stop("The `input` you specified ('", input, "') is neither",
         "an existing directory nor an existing file.");
  }

  ### Get all codeIds, all sourceIds and all entity Ids
  allCodeIds <- parsedSource$convenience$codingLeaves;
  allSourceIds <- unique(parsedSource$mergedSourceDf[, 'rxsSourceId']);
  allEntityIds <- unique(parsedSource$mergedSourceDf[, 'rxsEntityId']);
  
  ### Name the sourceId and entityId vector
  ### so that lapply also assigns these names
  names(allSourceIds) <- allSourceIds;
  names(allEntityIds) <- allEntityIds;
  
  allSourceIds <- allSourceIds[allSourceIds != "no_id"];
  allEntityIds <- allEntityIds[allEntityIds != "no_id"];

  msg(
    "\nStarting to add codings for ", length(allSourceIds),
    " sources and ", allEntityIds, " entities.",
    silent = silent
  );

  ### Create conveniently organised object
  organisedCoding <-
    lapply(
      allSourceIds,
      function(currentSourceId) {
        return(
          lapply(
            allEntityIds,
            function(currentEntityId) {
              res <-
                as.list(
                  parsedSource$mergedSourceDf[
                    parsedSource$mergedSourceDf[, 'rxsSourceId'] == currentSourceId &
                      parsedSource$mergedSourceDf[, 'rxsEntityId'] == currentEntityId,
                    allCodeIds
                  ]
                );
              ### Count all code occurrences
              res <- lapply(res, sum);
              return(res);
            }
          )
        );
      }
    );
  
  ###-----------------------------------------------------------------------------
  ### Add coding to Rxs tree
  ###-----------------------------------------------------------------------------
  
  msg(
    "\nStarting to add codings (i.e., code identifiers and 0/1 indicating ",
    "whether those were applied) to the Rxs tree.",
    silent = silent
  );
  
  for (currentSourceId in allSourceIds) {
    
    msg(
      "\n  - Starting to process source with identifier '",
      currentSourceId, "':",
      silent = silent
    );
    
    for (currentEntityId in allEntityIds) {

      msg(
        "\n    - Starting to process entity with identifier '",
        currentEntityId, "':",
        silent = silent
      );
      
      targetEntityNode <-
        data.tree::FindNode(
          x$rxsTrees[[currentSourceId]],
          currentEntityId
        );
      
      for (currentCodeId in allCodeIds) {
        
        ### Compose new name --- NOTE: NEEDS TO BE DYNAMIC IN THE FUNCTION!!!
        #newName <- paste0(currentEntityId, "_", currentCodeId);
        if (newEntityIdCodeFirst) {
          newName <- sprintf(newEntityIds, currentCodeId, currentEntityId);
        } else {
          newName <- sprintf(newEntityIds, currentEntityId, currentCodeId);
        }
        
        ### Now we will add the codings as siblings
        targetEntityNode$AddSibling(
          newName,
          value =
            organisedCoding[[currentSourceId]][[currentEntityId]][[currentCodeId]]
        );
        
        msg(
          "\n      - Added code with identifier '",
          currentCodeId, "' as entity with identifier '", newName, "'.",
          silent = silent
        );
        
      }

    }
    
  }
  
  if ("rockProducts" %in% names(x)) {
    x$rockProducts <-
      c(x$rockProducts,
        structure(
          list(
            parsedSource
          ),
          names = input
        ));
  } else {
    x$rockProducts <-
      structure(
        list(
          parsedSource
        ),
        names = input
      );
  }
  
  msg(
    "\nFinished adding codings for ", length(allSourceIds),
    " sources and ", allEntityIds, " entities.",
    silent = silent
  );
  
  return(invisible(x));
  
}
                                 