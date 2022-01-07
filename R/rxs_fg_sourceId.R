rxs_fg_sourceId <- function(level = 1,
                            indent = TRUE,
                            indentSpaces = 2,
                            fullWidth = 80,
                            commentCharacter = "#",
                            fillerCharacter = "#",
                            repeatingSuffix = "__1__",
                            silent=metabefor::opts$get("silent"),
                            overrideLevel = NULL) {
  
  eC <- metabefor::opts$get("entityColNames");
  rxsVersion <- metabefor::opts$get("rxsVersion");
  rxsCurrentNodeName <- metabefor::opts$get("rxsCurrentNodeName");

  uniqueSourceIdName <- metabefor::opts$get("uniqueSourceIdName");
  sourceIdTitle <- metabefor::opts$get("sourceIdTitle");
  sourceIdDescription <- metabefor::opts$get("sourceIdDescription");
  sourceIdValueTemplateDescription <- metabefor::opts$get("sourceIdValueTemplateDescription");
  sourceIdValueTemplateExamples <- metabefor::opts$get("sourceIdValueTemplateExamples");
  sourceIdDefaultValue <- metabefor::opts$get("sourceIdDefaultValue");

  lV <- rxs_fg_layoutVars(level = level,
                          indent = indent,
                          indentSpaces = indentSpaces,
                          fullWidth = fullWidth,
                          commentCharacter = commentCharacter,
                          fillerCharacter = fillerCharacter);
  
  # sourceIdOpeningTxt <-
  #   paste0(lV$indentSpaces,
  #          commentCharacter,
  #          repStr(fillerCharacter, 3),
  #          sourceIdTitle,
  #          repStr(lV$fullWidth - nchar(sourceIdTitle) - 4, fillerCharacter));

  assignment <- paste0(lV$indentSpaces,
                       uniqueSourceIdName,
                       " <- ");

  titleDescription <-
    rxs_fg_TitleDescription(title=sourceIdTitle,
                            description=sourceIdDescription,
                            level=level,
                            indent = indent,
                            indentSpaces = indentSpaces,
                            fullWidth = fullWidth,
                            commentCharacter = commentCharacter,
                            fillerCharacter = fillerCharacter);

  descriptionExamplesHeader <-
    paste0(lV$indentSpaces,
           commentCharacter,
           repStr(lV$fullWidth - nchar(" VALUE DESCRIPTION AND EXAMPLES ") - 4, fillerCharacter),
           " VALUE DESCRIPTION AND EXAMPLES ",
           repStr(fillerCharacter, 3));

  valueTemplateDescription <-
    strwrap(sourceIdValueTemplateDescription,
            width=lV$commentWidth,
            prefix=lV$commentPrefix);

  if (length(sourceIdValueTemplateExamples) > 1) {
    examplesTxt <- paste0(lV$commentPrefix, "EXAMPLES:");
  } else {
    examplesTxt <- paste0(lV$commentPrefix, "EXAMPLE:");
  }

  valueTemplateExamples <- c(examplesTxt,
                             lV$commentPrefix,
                             paste0(
                               lV$commentPrefix,
                               sourceIdValueTemplateExamples
                             ));
  
  openingTxt <- paste0(" START: ", uniqueSourceIdName, " ");
  closingTxt <- paste0(" END: ", uniqueSourceIdName, " ");
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
  
  # return(
  #   c(lV$lineFiller,
  #     sourceIdOpeningTxt,
  #     lV$lineFiller,
  #     lV$valuePrefix,
  #     paste0(lV$valuePrefix, uniqueSourceIdName, " <- \"\""),
  #     lV$valuePrefix,
  #     lV$lineFiller,
  #     lV$valuePrefix)
  # );

  return(list(c(lV$lineFiller,
                openingTxt,
                lV$lineFiller,
                assignment,
                titleDescription,
                lV$valuePrefix,
                paste0(lV$valuePrefix, sourceIdDefaultValue),
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
                closingTxt,
                lV$lineFiller)));

}
