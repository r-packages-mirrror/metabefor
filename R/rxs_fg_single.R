rxs_fg_single <- function(node,
                          valueTemplates,
                          silent=metabefor::opts$get("silent"),
                          overrideLevel = NULL) {
  
  eC <- metabefor::opts$get("entityColNames");
  rxsVersion <- metabefor::opts$get("rxsVersion");
  rxsCurrentNodeName <- metabefor::opts$get("rxsCurrentNodeName");
  indent <- metabefor::opts$get("indentDefault");
  indentSpaces <- metabefor::opts$get("indentSpaces");
  fullWidth <- metabefor::opts$get("fullWidth");
  commentCharacter <- metabefor::opts$get("commentCharacter");
  fillerCharacter <- metabefor::opts$get("fillerCharacter");
  repeatingSuffix <- metabefor::opts$get("repeatingSuffix");
  
  ### Simplest situation; we'll just return a full, simple
  ### fragment for this entity alone.

  if (!("parsedValueTemplates" %in% class(valueTemplates))) {
    stop("Argument 'valueTemplates' does not have class 'parsedValueTemplates' ",
         "(but instead ", vecTxtQ(class(valueTemplates)), ").");
  }

  if (is.numeric(overrideLevel)) {
    level <- overrideLevel;
  } else {
    level <- node$level;
    if (!is.null(overrideLevel) && !silent) {
      cat0("Argument 'overrideLevel' is not NULL but also not numeric; ",
           "ignoring it and setting the level to this node's level ");
    }
  }

  lV <- rxs_fg_layoutVars(level = level);

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
                                returnPathToRoot(node),
                                "[['value']] <-");
  } else {
    ### Now we always make new node accessible as 'current node name'
    assignmentToChild <- paste0(lV$indentSpaces,
                                rxsCurrentNodeName,
                                "[['value']] <-");
  }
  
  if (eC$instructionsCol %in% names(node)) {
    titleDescription <-
      rxs_fg_TitleDescription(title=node[[eC$titleCol]],
                              description=node[[eC$descriptionCol]],
                              instructions=node[[eC$instructionsCol]],
                              level=level);
  } else {
    titleDescription <-
      rxs_fg_TitleDescription(title=node[[eC$titleCol]],
                              description=node[[eC$descriptionCol]],
                              level=level);
  }
  
  valueAssignment <-
    rxs_fg_defaultValueAssignment(node=node,
                                  valueTemplates = valueTemplates,
                                  level = level);

  descriptionExamplesHeader <-
    paste0(lV$indentSpaces,
           commentCharacter,
           repStr(lV$fullWidth - nchar(" VALUE DESCRIPTION AND EXAMPLES ") - 4, fillerCharacter),
           " VALUE DESCRIPTION AND EXAMPLES ",
           repStr(fillerCharacter, 3));

  valueTemplateDescription <-
    rxs_fg_valueTemplateDescription(node=node,
                                    valueTemplates = valueTemplates,
                                    level = level);

  valueTemplateExamples <-
    rxs_fg_valueTemplateExamples(node=node,
                                 valueTemplates = valueTemplates,
                                 level = level);

  valueTemplateValidation <-
    rxs_fg_valueTemplateValidation(node=node,
                                   valueTemplates = valueTemplates,
                                   level = level);

  ### Use 'this' - process afterwards using data.tree - or some other
  ### way to leverage data.tree's "Do"


  if (rxsVersion < "0.3.0") {
    validationSpecification <-
      paste0(lV$indentSpaces,
             returnPathToRoot(node),
             "[['validation']] <- expression(",
             valueTemplateValidation,
             ");");
    backToParent <- NULL;
  } else {
    validationSpecification <-
      paste0(lV$indentSpaces,
             rxsCurrentNodeName,
             "[['validation']] <- expression(",
             valueTemplateValidation,
             ");");
    backToParent <- paste0(lV$indentSpaces,
                           rxsCurrentNodeName,
                           " <- ", rxsCurrentNodeName,
                           "$parent;");
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
                descriptionExamplesHeader,
                lV$lineFiller,
                lV$commentPrefix,
                valueTemplateDescription,
                lV$commentPrefix,
                valueTemplateExamples,
                lV$commentPrefix,
                lV$lineFiller,
                validationSpecification,
                backToParent,
                lV$lineFiller,
                closingTxt,
                lV$lineFiller)));

}
