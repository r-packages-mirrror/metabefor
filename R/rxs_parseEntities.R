rxs_parseEntities <- function(entities,
                              moduleName = NULL,
                              rxsRootName = metabefor::opts$get('rxsRootName'),
                              silent = metabefor::opts$get("silent")) {

  eC <- metabefor::opts$get("entityColNames");
  
  reservedNames <-
    c(metabefor::opts$get("rxsReservedNames"),
      data.tree::NODE_RESERVED_NAMES_CONST);
  
  usedReservedNames <-
    reservedNames %IN% entities[[eC$identifierCol]];
  
  if (any(usedReservedNames)) {
    if (!is.null(moduleName)) {
      moduleBit <- paste0("While processing module '", moduleName, "', in ");
    } else {
      moduleBit <- "In ";
    }
    stop(moduleBit, "the column with entity identifiers ('", eC$identifierCol,
         "'), you specified one of the reserved names as an identifier, ",
         "specifically ", vecTxtQ(reservedNames[usedReservedNames]), ". ",
         "Please change it to something else!");
  }
  
  ### Prepare dataframe with entities for conversion to a tree
  dataFrameNetwork <-
    as.data.frame(entities[!is.na(entities[[eC$identifierCol]]),
                           unique(c(eC$identifierCol,
                                    eC$parentCol, names(entities)))]);

  if (!is.null(moduleName)) {
    moduleBit <- paste0("for module '", moduleName, "' ");
  } else {
    moduleBit <- "";
  }
  
  msg("\nrxs_parseEntities read an entity spreadsheet ",
      moduleBit, "with the following columns: ",
      vecTxtQ(names(dataFrameNetwork)), ".\n",
      silent = silent);

  ### Add a root entity for the entities without one
  dataFrameNetwork[[eC$parentCol]][is.na(dataFrameNetwork[[eC$parentCol]])] <-
    rxsRootName;

  ### Check for nonexistent parents
  nonExistentParents <-
    !(dataFrameNetwork[[eC$parentCol]] %in% c(rxsRootName, dataFrameNetwork[[eC$identifierCol]]));
  if (any(nonExistentParents)) {
    
    if (!is.null(moduleName)) {
      moduleBit <- paste0("In module '", moduleName, "', the ");
    } else {
      moduleBit <- "The ";
    }
    stop(moduleBit, "items with the following identifiers have a parent ",
         "(i.e., specify a container entity that should contain them) with an ",
         "identifier that cannot be found in the list of entity identifiers:\n\n",
         paste0(paste0("  - '",
                       dataFrameNetwork[[eC$identifierCol]][nonExistentParents],
                       "' with parent '",
                       dataFrameNetwork$parent[nonExistentParents],
                       "' on line ",
                       which(entities[[eC$identifierCol]] %in%
                               entities[[eC$identifierCol]][nonExistentParents])),
                collapse=";\n"),
         ")!");
  }

  ### Convert to tree
  extractionScriptTree <- data.tree::FromDataFrameNetwork(dataFrameNetwork);

  ### Check for unique names
  if (!data.tree::AreNamesUnique(extractionScriptTree)) {
    warning(paste0("Not all identifiers in the extraction script ",
                   "tree are unique! Duplicated elements: ",
                   vecTxtQ(dataFrameNetwork[[eC$identifierCol]][duplicated(dataFrameNetwork[[eC$identifierCol]])]),
                   ". This may cause problems - it is advisable ",
                   "to make sure identifiers are unique."));
  }


  ### Retrieve all recursing node definitions and place them
  ### in a separate list. We do this first for the recursing
  ### nodes, then for the recurring nodes, because all recursing
  ### nodes are recurring nodes.
  recursingNodes <-
    data.tree::Traverse(extractionScriptTree,
                        traversal="level",
                        filterFun=function(node) {
                          return(isRecursingEntityDefinition(node,
                                                             recursingColName=eC$recursingCol));
                        });
  ### Remove all recursing node definitions from the extraction
  ### script tree
  numberOfRecursingEntities <-
    data.tree::Prune(extractionScriptTree,
          pruneFun=function(node) {
            return(!isRecursingEntityDefinition(node,
                                                recursingColName=eC$recursingCol));
          });
  ### Name the recursing entities
  names(recursingNodes) <-
    sapply(recursingNodes, function(x) return(x$name));

  ### Retrieve all recurring node definitions and place them
  ### in a separate list
  recurringNodes <-
    data.tree::Traverse(extractionScriptTree,
                        traversal="level",
                        filterFun=function(node) {
                          return(isRecurringEntityDefinition(node,
                                                             recurringColName=eC$recurringCol));
                        });
  ### Remove all recursing node definitions from the extraction
  ### script tree
  numberOfRecurringEntities <-
    data.tree::Prune(extractionScriptTree,
          pruneFun=function(node) {
            return(!isRecurringEntityDefinition(node,
                                                recurringColName=eC$recurringCol));
          });
  ### Name the recurring entities
  names(recurringNodes) <-
    sapply(recurringNodes, function(x) return(x$name));

  ### Add all recurring node definitions in the tree
  ### where they are included
  extractionScriptTree$Do(function(node,
                                   recurringColName=eC$recurringCol,
                                   recurrNodes=recurringNodes) {

                            ### Check which recursive node to add
                            nodeToInclude <- node[[recurringColName]];
                            ### Add each child
                            for (currentChild in recurrNodes[[nodeToInclude]]$children) {
                              node$AddChildNode(data.tree::Clone(currentChild));
                            }

                          },
                          filterFun = function(node) {
                            return(isRecurringEntityInclusion(node,
                                                              recurringColName=eC$recurringCol));
                          });
  ### Add all recursing node definitions in the tree
  ### where they are included
  extractionScriptTree$Do(function(node,
                                   recursingColName=eC$recursingCol,
                                   recursNodes=recursingNodes) {

    ### Check which recursive node to add
    nodeToInclude <- node[[recursingColName]];
    ### Add each child
    for (currentChild in recursNodes[[nodeToInclude]]$children) {
      node$AddChildNode(data.tree::Clone(currentChild));
    }

  },
  filterFun = function(node) {
    return(isRecursingEntityInclusion(node,
                                      recursingColName=eC$recursingCol));
  });

  res <- list(extractionScriptTree=extractionScriptTree,
              recurringNodes=recurringNodes,
              recursingNodes=recursingNodes);

  attr(res, "numberOfRecurringEntities") <- numberOfRecurringEntities;
  attr(res, "numberOfRecursingEntities") <- numberOfRecursingEntities;

  class(res) <- "parsedEntities";

  return(res);

}



