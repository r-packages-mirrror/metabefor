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
#' 
#' \item{quridPrefix}{
#' The prefix for quasi-unique record identifiers (QURIDs).}
#'
#' \item{ws}{
#' The worksheet names: a named list with four character values
#' named `entities`, `valueTemplates`, `definitions`, and `instructions`.}
#' 
#' \item{eC}{
#' The entity columns; a named list with character values holding the
#' names of the columns in the `entities` worksheet of the spreadsheet. The
#' default values are stored in `metabefor::opts$get("entityColNames")` - if you
#' need to override these values, just reproduce that object.}
#' 
#' \item{valueTemplateCols}{
#' The value template columns; a named list with
#' character values holding the names of the columns in the `entities`
#' worksheet of the spreadsheet. The default values are stored
#' in `metabefor::opts$get("valueTemplateColNames")` - if you need to
#' override these values, just reproduce that object.}
#' 
#' \item{instructionsCols}{
#' The instructions worksheet columns: a names list with
#' character values holding the names of the columns in the `instructions`
#' worksheet of the spreadsheet. The default values are stored
#' in `metabefor::opts$get("instructionsColNames")` - if you need to
#' override these values, just reproduce that object.}
#' 
#' \item{definitionsCols}{
#' The definitions worksheet columns: a names list with
#' character values holding the names of the columns in the `definitions`
#' worksheet of the spreadsheet. The default values are stored
#' in `metabefor::opts$get("definitionsColNames")` - if you need to
#' override these values, just reproduce that object.}
#' 
#' \item{indentDefault}{
#' Whether to use indentation to visually organise the Rxs
#' template. If TRUE, deeper nesting in the Rxs specification's hierarchy
#' will be visible as deeper indentation.}
#' 
#' \item{indentSpaces}{
#' The number of spaces to use when identing.}
#' 
#' \item{fullWidth}{
#' The maximum width of the Rxs template in characters.}
#' 
#' \item{commentCharacter}{
#' The character used to signify comments - if this is
#' changed, R will throw errors (unless perhaps it once introduces another
#' comment symbol).}
#' 
#' \item{fillerCharacter}{
#' The character used after the first character for
#' filling up space.}
#' 
#' \item{repeatingSuffix}{
#' The suffix to use for the entity identifiers/names
#' of repeating entities.}
#' 
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
    
    maxCores = NULL,
    
    defaultHeadingLevel = 3,
    
    ### For creating the Rxs template
    indentDefault = TRUE,
    indentSpaces = 2,
    fullWidth = 78,
    commentCharacter = "#",
    fillerCharacter = "#",
    repeatingSuffix = "__1__",
    
    ### Used to parse rxs specifications
    rxsSheetnames = list(entities = 'entities',
                         valueTemplates = 'valueTemplates',
                         definitions = 'definitions',
                         instructions = 'instructions',
                         texts = 'texts'),
    
    entityColNames = list(titleCol = "title",
                          descriptionCol = "description",
                          instructionsCol = "instructions",
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
    
    definitionsColNames = list(termCol = "term",
                               definitionCol = "definition"),
    
    textsColNames = list(textIdCol = "textIdentifier",
                         contentCol = "content"),
    
    txsColNames = list(source_identification_entity_id = 'source_identification_entity_id',
                       source_identification_value = 'source_identification_value',
                       parent_entity_id = 'parent_entity_id',
                       entity_id = 'entity_id',
                       value = 'value'),
    
    rxs_fg_list_codingHelp =
      "<entityTitle>: <entityDescription> <entityInstructions> [Examples: <examples>] [Value description: <valueDescription>]",
    
    sourceIdDefaultValue = "NULL", # "\"\"",
    sourceIdTitle = " SET UNIQUE SOURCE IDENTIFIER ",
    sourceIdDescription =
      paste0("A unique identifier used in this systematic ",
             "review to refer to this source"),
    sourceIdValueTemplateDescription =
      paste0("A unique identifier to use in this systematic review. ",
             "For sources with a DOI, this is the last part of the shortDOI ",
             "as looked up through https://shortdoi.org (the part after ",
             "the \"10/\"). For sources without a DOI (and so, without a ",
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
    
    extractorIdDefaultValue = "NULL", # "\"\"",
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
          "source!\nThis version of the Rxs template was produced at <<date>>.",
          "\n\nYou can now start extracting. If you haven't yet ",
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
    
    rockInterfacing_rxsSourceId = "rxsSourceId",
    rockInterfacing_rxsEntityId = "rxsEntityId",
    
    ### Working with search results
    
    search_metadataRegex = "^(\\d\\d\\d\\d-\\d\\d-\\d\\d)_([[:alnum:]]+)_([[:alnum:]]+)_?(.*)?\\.ris$",
    search_originDir_col = "originDir",
    search_originFile_col = "originFile",
    search_originDatabase_col = "originDatabase",
    search_originInterface_col = "originInterface",
    search_originDate_col = "originDate",
    search_originQuerySpec_col = "querySpec",

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
    
    ### Working with OpenAlex exports
    openAlex_extract_csv_fields =
      c("id", "doi", "title", "display_name", "publication_year", "publication_date",
        "language", "type", "type_crossref", "authorships.raw_author_name"),
    
    ### Used throughout for working with files
    encoding = "UTF-8",
    preventOverwriting = TRUE,
    
    ### Used throughout for suppressing messages
    silent = TRUE
    
  );
