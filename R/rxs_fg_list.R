rxs_fg_list <- function(node,
                        valueTemplates,
                        silent=metabefor::opts$get("silent"),
                        overrideLevel = NULL,
                        codingHelp = "<entityTitle>: <entityDescription> [Examples: <examples>] [Value description: <valueDescription>]",
                        codingHelpSep = "; ") {
  
  eC <- metabefor::opts$get("entityColNames");
  rxsVersion <- metabefor::opts$get("rxsVersion");
  rxsCurrentNodeName <- metabefor::opts$get("rxsCurrentNodeName");
  indent <- metabefor::opts$get("indentDefault");
  indentSpaces <- metabefor::opts$get("indentSpaces");
  fullWidth <- metabefor::opts$get("fullWidth");
  commentCharacter <- metabefor::opts$get("commentCharacter");
  fillerCharacter <- metabefor::opts$get("fillerCharacter");
  repeatingSuffix <- metabefor::opts$get("repeatingSuffix");
  
  ### A list of relatively simple values.

  if (!("parsedValueTemplates" %in% class(valueTemplates))) {
    stop("Argment 'valueTemplates' does not have class 'parsedValueTemplates' ",
         "(but instead ", vecTxtQ(class(valueTemplates)), ").");
  }

  if (is.numeric(overrideLevel)) {
    level <- overrideLevel;
  } else {
    level <- node$level;
    if (!is.null(overrideLevel) && !silent) {
      cat0("\nArgument 'overrideLevel' is not NULL but also not numeric; ",
           "ignoring it and setting the level to this node's level.\n");
    }
  }

  lV <- rxs_fg_layoutVars(level = level);

  ### For repeating nodes, we only set a temporary name, which
  ### we later change to the value of the first field.
  if (is_TRUE(node[[eC$repeatingCol]])) {
    currentEntityName <- paste0(node$name, repeatingSuffix);
    currentStartEndName <- paste0(node$name, " (REPEATING)");
  } else {
    currentEntityName <- node$name;
    currentStartEndName <- node$name;
  }
  
  if (rxsVersion < "0.3.0") {
    ### Old version, before making new node available as 'current node name'
    childAddition <- paste0(lV$indentSpaces,
                            returnPathToRoot(node$parent),
                            "$AddChild('",
                            currentEntityName,
                            "');");
  } else {
    ### Now we always make new node accessible as 'current node name'
    childAddition <- paste0(lV$indentSpaces,
                            rxsCurrentNodeName, " <- ",
                            rxsCurrentNodeName,
                            "$AddChild('",
                            currentEntityName,
                            "');");
  }
  
  if (rxsVersion < "0.3.0") {
    ### Old version, before making new node available as 'current node name'
    assignmentToChild <- paste0(lV$indentSpaces,
                                returnPathToRoot(node$parent),
                                "$", currentEntityName,
                                "[['value']] <-");
  } else {
    ### Now we always make new node accessible as 'current node name'
    assignmentToChild <- paste0(lV$indentSpaces,
                                rxsCurrentNodeName,
                                "[['value']] <-");
  }

  titleDescription <-
    rxs_fg_TitleDescription(title=node[[eC$titleCol]],
                            description=node[[eC$descriptionCol]],
                            level=level);

  listEntities <- node$Get(function(node) {
    valueAssignment <-
      rxs_fg_defaultValueAssignment(node=node,
                                    valueTemplates = valueTemplates,
                                    level = level);
    return(paste0(node$name, " = ", trimws(valueAssignment)));
  }, filterFun = data.tree::isLeaf);

  entityValidations <- node$Get(function(node) {
    return(rxs_fg_valueTemplateValidation(node=node,
                                          valueTemplates = valueTemplates,
                                          level = level));
  }, filterFun = data.tree::isLeaf);

  ### If this list has a child entity that is marked as an identifying
  ### entity, rename it to the value of this entity
  identifyingEntityName <-
    node$Get('name',
             filterFun = function(nd) {
               return(nd$isLeaf && is_TRUE(nd[[eC$identifyingCol]]));
             });

  if (!is.null(identifyingEntityName)) {
    if (rxsVersion < "0.3.0") {
      nodeRenaming <- c(paste0(lV$indentSpaces,
                               returnPathToRoot(node$parent),
                               "$", currentEntityName,
                               "$name <- metabefor::nodeName(",
                               returnPathToRoot(node$parent),
                               "$", currentEntityName, "$value[['",
                               identifyingEntityName[1], "']], \"",
                               currentEntityName, "\");"));
    } else {
      nodeRenaming <- c(paste0(lV$indentSpaces,
                               rxsCurrentNodeName,
                               "$name <- metabefor::nodeName(",
                               rxsCurrentNodeName,
                               "$value[['",
                               identifyingEntityName[1], "']], \"",
                               currentEntityName, "\");"));
    }
    if (length(identifyingEntityName) > 1) {
      warning("More than one entity in the list '", node$name,
              "' is marked as identifying entity. The full ",
              "list is ", vecTxtQ(identifyingEntityName),
              ". Only using the first one ('",
              identifyingEntityName[1], "').");
    }
  } else if (is.null(identifyingEntityName) && is_TRUE(node[[eC$repeatingCol]])) {

    ###
    ###   Check rxs version and behave depending on version!!!
    ###

    if (rxsVersion < "0.3.0") {
      nodeRenaming <- c(paste0(lV$indentSpaces,
                               returnPathToRoot(node$parent),
                               "$", currentEntityName,
                               "$name <- metabefor::nodeName(",
                               returnPathToRoot(node$parent),
                               "$", currentEntityName, "$value[[1]], \"",
                               currentEntityName, "\");"));
    } else {
      nodeRenaming <- c(paste0(lV$indentSpaces,
                               rxsCurrentNodeName,
                               "$name <- metabefor::nodeName(",
                               rxsCurrentNodeName,
                               "$value[[1]], \"",
                               currentEntityName, "\");"));
    }
  } else {
    nodeRenaming <- NULL;
  }

  listElementNames <- node$Get('name', filterFun = data.tree::isLeaf);

  entityReferences <- node$Get('entityRef');
  entityReferences <- entityReferences[!is.na(entityReferences)];
  if (length(entityReferences) > 0) {
    entityReferences <- paste0("c(",
                               paste0(names(entityReferences), '="', entityReferences, '"',
                                      collapse=", "),
                               ");");
    if (rxsVersion < "0.3.0") {
      entityReferences <- paste0(lV$indentSpaces,
                                 returnPathToRoot(node$parent),
                                 "$", currentEntityName,
                                 "[['entityRefs']] <- ",
                                 entityReferences);
    } else {
      entityReferences <- paste0(lV$indentSpaces,
                                 rxsCurrentNodeName,
                                 "[['entityRefs']] <- ",
                                 entityReferences);
    }
  } else {
    entityReferences <- NULL;
  }

  fieldReferences <- node$Get('fieldRef');
  fieldReferences <- fieldReferences[!is.na(fieldReferences)];
  if (length(fieldReferences) > 0) {
    fieldReferences <- paste0("c(",
                              paste0(names(fieldReferences), '="', fieldReferences, '"',
                                     collapse=", "),
                              ");");
    if (rxsVersion < "0.3.0") {
      fieldReferences <- paste0(lV$indentSpaces,
                                returnPathToRoot(node$parent),
                                "$", currentEntityName,
                                "[['fieldRefs']] <- ",
                                fieldReferences);
    } else {
      fieldReferences <- paste0(lV$indentSpaces,
                                rxsCurrentNodeName,
                                "[['fieldRefs']] <- ",
                                fieldReferences);
    }
  } else {
    fieldReferences <- NULL;
  }

  if (rxsVersion < "0.3.0") {
    validationAssignmentStart <- paste0(lV$indentSpaces,
                                        returnPathToRoot(node$parent),
                                        "$", currentEntityName,
                                        "[['validation']] <- ");
    backToParent <- NULL;
  } else {
    validationAssignmentStart <- paste0(lV$indentSpaces,
                                        rxsCurrentNodeName,
                                        "[['validation']] <- ");
    backToParent <- paste0(lV$indentSpaces,
                           rxsCurrentNodeName,
                           " <- ", rxsCurrentNodeName,
                           "$parent;");
  }
  validationFollowingSpaces <- repStr(nchar(validationAssignmentStart));

  if (length(listEntities) == 1) {
    valueAssignment <- c(paste0(lV$valuePrefix, "list(", listEntities[1], ");"));
    validationAssignment <- c(paste0(lV$indentSpaces, validationAssignmentStart,
                                     "list(`", listElementNames[1], "` = expression(", entityValidations[1], "));"));
  } else if (length(listEntities) == 2) {
    valueAssignment <- c(paste0(lV$valuePrefix, "list(", listEntities[1], ","),
                         paste0(lV$valuePrefix, repStr(5), listEntities[2], ");"));
    validationAssignment <- c(paste0(validationAssignmentStart, "list(`", listElementNames[1],
                                     "` = expression(", entityValidations[1], "),"),
                              paste0(validationFollowingSpaces, repStr(5), "`", listElementNames[2],
                                     "` = expression(", entityValidations[2], "));"));
  } else {
    valueAssignment <- c(paste0(lV$valuePrefix, "list(", listEntities[1], ","),
                         paste0(lV$valuePrefix, repStr(5), listEntities[-c(1, length(listEntities))], ","),
                         paste0(lV$valuePrefix, repStr(5), listEntities[length(listEntities)], ");"));
    validationAssignment <- c(paste0(validationAssignmentStart, "list(`",
                                     listElementNames[1], "` = expression(", entityValidations[1], "),"),
                              paste0(validationFollowingSpaces, repStr(5),
                                     "`", listElementNames[-c(1, length(entityValidations))], "` = expression(",
                                     entityValidations[-c(1, length(entityValidations))], "),"),
                              paste0(validationFollowingSpaces, repStr(5),
                                     "`", listElementNames[length(entityValidations)], "` = expression(", entityValidations[length(entityValidations)], "));"));
  }

  if (!is.null(codingHelp) && !is.na(codingHelp)) {

    listEntityLengths <- nchar(valueAssignment);
    maxListEntityLength <- max(listEntityLengths);
    spacesToAdd <- (maxListEntityLength + 2) - listEntityLengths;

    codingHelpStrings_examples <-
      node$Get(function(node) {
        return(rxs_fg_valueTemplateExamples(node=node,
                                            valueTemplates = valueTemplates,
                                            level = level,
                                            listVersion = TRUE));
      }, filterFun = data.tree::isLeaf);
    codingHelpStrings_examples <-
      unlist(lapply(codingHelpStrings_examples,
                    paste0,
                    collapse=codingHelpSep));

    codingHelpStrings_entityDescriptions <-
      node$Get(eC$descriptionCol, filterFun = data.tree::isLeaf);
    codingHelpStrings_entityTitles <-
      node$Get(eC$titleCol, filterFun = data.tree::isLeaf);

    codingHelpStrings_valueDescriptions <-
      node$Get(function(node) {
        return(rxs_fg_valueTemplateDescription(node=node,
                                               valueTemplates = valueTemplates,
                                               level = level,
                                               listVersion = TRUE));
      }, filterFun = data.tree::isLeaf);

    valueAssignment <- paste0(valueAssignment,
                              sapply(spacesToAdd, repStr));

    codingHelpStrings <-
      sapply(seq_along(valueAssignment),
             function(i) {
               res <- codingHelp;
               res <- gsub("<examples>", codingHelpStrings_examples[i], res);
               res <- gsub("<entityTitle>", codingHelpStrings_entityTitles[i], res);
               res <- gsub("<entityDescription>", codingHelpStrings_entityDescriptions[i], res);
               res <- gsub("<valueDescription>", codingHelpStrings_valueDescriptions[i], res);
               return(res);
             });

    valueAssignment <- paste0(valueAssignment,
                              commentCharacter,
                              repStr(fillerCharacter, 2), " ",
                              codingHelpStrings);

  }

  openingTxt <- paste0(" START: ", currentStartEndName, " ");
  closingTxt <- paste0(" END: ", currentStartEndName, " ");
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

  ### Return the result in a list in case we're called for multiple nodes
  return(list(c(lV$lineFiller,
                openingTxt,
                lV$lineFiller,
                childAddition,
                assignmentToChild,
                titleDescription,
                lV$valuePrefix,
                valueAssignment,
                lV$valuePrefix,
                lV$lineFiller,
                validationAssignment,
                nodeRenaming,
                entityReferences,
                fieldReferences,
                backToParent,
                lV$lineFiller,
                closingTxt,
                lV$lineFiller)));

}
