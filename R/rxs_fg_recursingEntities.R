rxs_fg_recursingEntities <- function(listOfNodes,
                                     valueTemplates,
                                     silent=metabefor::opts$get("silent"),
                                     overrideLevel = NULL) {
  
  rxsVersion <- metabefor::opts$get("rxsVersion");
  rxsCurrentNodeName <- metabefor::opts$get("rxsCurrentNodeName");
  eC <- metabefor::opts$get("entityColNames");
  indent <- metabefor::opts$get("indentDefault");
  indentSpaces <- metabefor::opts$get("indentSpaces");
  fullWidth <- metabefor::opts$get("fullWidth");
  commentCharacter <- metabefor::opts$get("commentCharacter");
  fillerCharacter <- metabefor::opts$get("fillerCharacter");
  repeatingSuffix <- metabefor::opts$get("repeatingSuffix");
  
  ### A container for all recursing entities

  if (!("parsedValueTemplates" %in% class(valueTemplates))) {
    stop("Argument 'valueTemplates' does not have class 'parsedValueTemplates' ",
         "(but instead ", vecTxtQ(class(valueTemplates)), ").");
  }

  if (is.numeric(overrideLevel)) {
    level <- overrideLevel;
  } else {
    level <- 1;
    if (!is.null(overrideLevel) && !silent) {
      cat0("Argument 'overrideLevel' is not NULL but also not numeric; ",
           "ignoring it and setting the level to 1.");
    }
  }

  lV <- rxs_fg_layoutVars(level = level);

  titleDescription <-
    rxs_fg_TitleDescription(title="RECURSING ENTITIES",
                            description=paste0("This section contains the recursing entities. When done ",
                                               "extracting from this paper, they can be removed from ",
                                               "the extraction script."));

  openingTxt <- paste0(" START: RECURSING ENTITIES ");
  closingTxt <- paste0(" END: RECURSING ENTITIES ");
  openingTxt <-
    paste0(lV$indentSpaces,
           commentCharacter,
           repStr(lV$fullWidth - nchar(openingTxt) - 4, fillerCharacter),
           openingTxt,
           repStr(fillerCharacter, 3));
  closingTxt <-
    paste0(lV$indentSpaces,
           commentCharacter,
           repStr(lV$fullWidth - nchar(closingTxt) - 4, fillerCharacter),
           closingTxt,
           repStr(fillerCharacter, 3));

  ### Now get the character vectors for the children
  childFragments <- unlist(sapply(listOfNodes,
                                  function(node) {
                                    res <- rxs_fg_dispatcher(node,
                                                             valueTemplates = valueTemplates,
                                                             silent=silent);
                                    ### Remove assignment of addition of node for this entity
                                    if (rxsVersion < "0.3") {
                                      pattern <- paste0("study$AddChild('", node$name, "');");
                                    } else {
                                      pattern <- paste0("currentNode$AddChild('", node$name, "');");
                                    }
                                    res <- sapply(res,
                                                  gsub,
                                                  pattern=pattern,
                                                  replacement="",
                                                  fixed=TRUE);
                                    ### In each recursive entity (and its entire tree), replace
                                    ### the assignment path with the placeholder name
                                    res <- sapply(res,
                                                  gsub,
                                                  pattern=returnPathToRoot(node),
                                                  replacement="recursiveElementPlaceholder_level1",
                                                  fixed=TRUE);
                                    return(c("",
                                             res,
                                             ""));
                                  }));


  ### Return the result in a list in case we're called for multiple nodes

  return(list(c(lV$lineFiller,
                openingTxt,
                lV$lineFiller,
                #childAddition,
                #assignmentToChild,
                titleDescription,
                #lV$valuePrefix,
                #valueAssignment,
                #lV$valuePrefix,
                #lV$lineFiller,
                #lV$valuePrefix,
                childFragments,
                lV$valuePrefix,
                lV$lineFiller,
                closingTxt,
                lV$lineFiller)));

}
