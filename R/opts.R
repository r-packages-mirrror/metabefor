#' Options for the metabefor package
#'
#' The `metabefor::opts` object contains three functions to set, get, and reset
#' options used by the metabefor package. Use `metabefor::opts$set` to set options,
#' `metabefor::opts$get` to get options, or `metabefor::opts$reset` to reset specific or
#' all options to their default values.
#'
#' It is normally not necessary to get or set `metabefor` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `metabefor::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example, `utteranceMarker = "\n"`. For
#'   `metabefor::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `metabefor::opts$set`, the name of the option to set.}
#'   \item{default}{For `metabefor::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' The following options can be set:
#'
#' \describe{
#'   \item{quridPrefix}{The prefix for quasi-unique record identifiers (QURIDs).}
#'
#'   \item{Two}{Second item}
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default Quasi-Unique Record Identifier prefix
#' metabefor::opts$get(quridPrefix);
#'
#' ### Set it to a custom version, so that every line starts with a pipe
#' metabefor::opts$set(quridPrefix = "QURID_");
#'
#' ### Check that it worked
#' metabefor::opts$get(quridPrefix);
#'
#' ### Reset this option to its default value
#' metabefor::opts$reset(quridPrefix);
#'
#' ### Check that the reset worked, too
#' metabefor::opts$get(quridPrefix);
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("metabefor.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option '", option, "' is not a valid (i.e. existing) option for metabefor!");
  }
}

opts$get <- function(option, default=FALSE) {
  option <- as.character(substitute(option));
  if (!option %in% names(opts$defaults)) {
    stop("Option '", option, "' is not a valid (i.e. existing) option for metabefor!");
  } else {
    return(getOption(paste0("metabefor.", option),
                     opts$defaults[[option]]));
  }
}

opts$reset <- function(...) {
  optionNames <-
    unlist(lapply(as.list(substitute(...)),
                  as.character));
  if (length(optionNames) == 0) {
    do.call(opts$set,
            opts$defaults);
  } else {
    prefixedOptionNames <-
      paste0("metabefor.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for metabefor!");
    }
  }
}

opts$defaults <-
  list(
    
    rxsVersion = "0.3.0",
    
    rxsRootName = "source",
    
    rxsObjectName = "rxsObject",
    rxsTemplateSpecName = "rxsTemplateSpec",
    rxsCurrentNodeName = "currentEntity",
    uniqueSourceIdName = "uniqueSourceIdentifier",
    extractorIdName = "extractorIdentifier",
    
    ### Used throughout
    debug = FALSE,
    
    quridPrefix = "qurid_",
    quridSuffix = "",
    
    idEntityName = "id",
    
    moduleWithoutName = "moduleWithoutName",
    
    ### Used to parse rxs specifications
    rxsSheetnames = list(entities = 'entities',
                         valueTemplates = 'valueTemplates',
                         definitions = 'definitions',
                         instructions = 'instructions'),
    
    entityColNames = list(titleCol = "title",
                          descriptionCol = "description",
                          identifierCol = "identifier",
                          valueTemplateCol = "valueTemplate",
                          validValuesCol = "validValues",
                          defaultCol = "default",
                          parentCol = "parent",
                          moduleCol = "module",
                          entityRefCol = "entityRef",
                          fieldRefCol = "fieldRef",
                          ownerCol = "owner",
                          listCol = "list",
                          collapsingCol = "collapsing",
                          repeatingCol = "repeating",
                          recurringCol = "recurring",
                          recursingCol = "recursing",
                          identifyingCol = "identifying"),
    
    valueTemplateColNames = list(identifierCol = "identifier",
                                 descriptionCol = "description",
                                 validValuesCol = "validValues",
                                 defaultCol = "default",
                                 examplesCol = "examples",
                                 validationCol = "validation",
                                 errorCol = "error"),
    
    instructionsColNames = list(headingCol = "heading",
                                descriptionCol = "description"),
    
    defininitionsColNames = list(termCol = "term",
                                 definitionCol = "definition"),
    
    txsColNames = list(study_identification_entity_id = 'study_identification_entity_id',
                       study_identification_value = 'study_identification_value',
                       parent_entity_id = 'parent_entity_id',
                       entity_id = 'entity_id',
                       value = 'value'),
    
    sourceIdDefaultValue = "\"\"",
    sourceIdTitle = " SET UNIQUE SOURCE IDENTIFIER ",
    sourceIdDescription =
      paste0("A unique identifier used in this systematic ",
             "review to refer to this source"),
    sourceIdValueTemplateDescription =
      paste0("A unique identifier to use in this systematic review. ",
             "For sources with a DOI, this is the last part of the shortDOI ",
             "as looked up through https://shortdoi.org (the part after ",
             "the \"10/\"). For studies without a DOI (and so, without a ",
             "shortDOI), this can be, for example, the QURID (Quasi-Unique ",
             "Record Identifier) that was designated during the screening ",
             "phase or which you can create with `metabefor::qurid()`."),
    sourceIdValueTemplateExamples = c("\"g5fj\"", "\"qurid_7h4pksl6\""),
    sourceIdValidation = "grepl(\"^[a-zA-Z0-9]{3,}$|^qurid_[a-zA-Z0-9]+$\", VALUE, ignore.case=TRUE)",
    
    identifierExplanationText =
      paste0("Identifiers can only consist of (lower or uppercase) ",
             "Latin letters [a-zA-Z], Arabic numerals [0-9], and ",
             "underscores [_], and always have to start with a letter ",
             "(as a regular expression: ^[a-zA-Z][a-zA-Z0-9_]*$)."),
    
    extractorIdDefaultValue = "\"\"",
    extractorIdTitle = " SPECIFY YOUR EXTRACTOR IDENTIFIER ",
    extractorIdDescription =
      paste0("An identifier unique to every extractor"),
    extractorIdValueTemplateDescription =
      paste0("Identifiers can only consist of (lower or uppercase) ",
             "Latin letters [a-zA-Z], Arabic numerals [0-9], and ",
             "underscores [_], and always have to start with a letter."),
    extractorIdValueTemplateExamples = c("\"extractor_1\"", "\"Alex\""),
    extractorIdValidation = "grepl(\"^[a-zA-Z][a-zA-Z0-9_]*$\", VALUE, ignore.case=TRUE)",
    
    texts = list(
      openingRemarks = 
        paste0(
          "Welcome to the R Extraction Script (.rxs.Rmd file) for this ",
          "source!\n\nYou can now start extracting. If you haven't yet ",
          "studied the extractor instructions, please do so first. If you're ",
          "all set, good luck!"
        ),
      closingRemarks =
        paste0(
          "Well done! You are now done extracting this source. Great job!!!\n\n",
          "Now, please knit the R Extraction Script into an HTML file and ",
          "carefully check whether you entered everything correctly, since it ",
          "will cost much less time to correct any errors, now that you still ",
          "have this source in your mind, than later on when you'll have to ",
          "dive into it all over."
        ),
      extractionOverview_list_intro =
        paste0(
          "This is an overview of the entities to extract, their titles ",
          "and descriptions, and other details that will become part of the ",
          "extraction script template that will be used for the actual ",
          "extraction."
        ),
      extractionOverview_compact_intro =
        paste0(
          "This is an overview of the entities to extract, their titles ",
          "and descriptions, and other details that will become part of the ",
          "extraction script template that will be used for the actual ",
          "extraction."
        )
    ),
    
    defaultGraphTheme = list(
      c("fontname", "Arial", "graph"),
      c("fontname", "Arial", "node"),
      c("fontname", "Arial", "edge"),
      c("layout", "dot", "graph"),
      c("rankdir", "LR", "graph"),
      c("outputorder", "edgesfirst", "graph"),
      c("fixedsize", "false", "node"),
      c("shape", "box", "node"),
      c("style", "filled", "node"),
      c("color", "#000000", "node"),
      c("color", "#888888", "edge"),
      c("dir", "none", "edge"),
      c("headclip", "false", "edge"),
      c("tailclip", "false", "edge"),
      c("fillcolor", "#FFFFFF", "node")
    ),
    
    rxsReservedNames = c("title", "value"),
    
    diagrammerSanitization = list(c("\\\"", "`"),
                                  c("\\'", "`"),
                                  c("\\\\", "/"),
                                  c("[^a-zA-Z0-9;)(,._/`-]", " ")),
    
    ### Regular expressions for Google Sheets
    gSheetId_extractionRegex =
      "^https://docs\\.google\\.com/spreadsheets/d/([a-zA-Z0-9_-]*)(/.*)?$",
    
    gSheetId_to_exportLink =
      "https://docs.google.com/spreadsheets/d/%s/export?format=xlsx",
    
    ### Used throughout for working with files
    encoding = "UTF-8",
    preventOverwriting = TRUE,
    
    ### Used throughout for suppressing messages
    silent = TRUE
    
  );
