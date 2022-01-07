rxs_fg_extractorId <- function(level = 1,
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

  extractorIdName <- metabefor::opts$get("extractorIdName");
  extractorIdTitle <- metabefor::opts$get("extractorIdTitle");
  extractorIdDescription <- metabefor::opts$get("extractorIdDescription");
  extractorIdValueTemplateDescription <- metabefor::opts$get("extractorIdValueTemplateDescription");
  extractorIdValueTemplateExamples <- metabefor::opts$get("extractorIdValueTemplateExamples");
  extractorIdDefaultValue <- metabefor::opts$get("extractorIdDefaultValue");
  extractorIdValidation <- metabefor::opts$get("extractorIdValidation");
  
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
                       extractorIdName,
                       " <- ");

  titleDescription <-
    rxs_fg_TitleDescription(title=extractorIdTitle,
                            description=extractorIdDescription,
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
    strwrap(extractorIdValueTemplateDescription,
            width=lV$commentWidth,
            prefix=lV$commentPrefix);

  if (length(extractorIdValueTemplateExamples) > 1) {
    examplesTxt <- paste0(lV$commentPrefix, "EXAMPLES:");
  } else {
    examplesTxt <- paste0(lV$commentPrefix, "EXAMPLE:");
  }

  valueTemplateExamples <- c(examplesTxt,
                             lV$commentPrefix,
                             paste0(
                               lV$commentPrefix,
                               extractorIdValueTemplateExamples
                             ));
  
  openingTxt <- paste0(" START: ", extractorIdName, " ");
  closingTxt <- paste0(" END: ", extractorIdName, " ");
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
                paste0(lV$valuePrefix, extractorIdDefaultValue),
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
