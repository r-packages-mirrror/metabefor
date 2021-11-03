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
    unlist(lapply(as.list(substitute(...())),
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
    ### Used throughout
    debug = FALSE,
    
    quridPrefix = "qurid_",
    quridSuffix = "",
    
    ### Used to parse rxs specifications
    entityColNames = list(titleCol = "title",
                          descriptionCol = "description",
                          identifierCol = "identifier",
                          valueTemplateCol = "valueTemplate",
                          validValuesCol = "validValues",
                          defaultCol = "default",
                          parentCol = "parent",
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
    
    txsColNames = list(study_identification_entity_id = 'study_identification_entity_id',
                       study_identification_value = 'study_identification_value',
                       parent_entity_id = 'parent_entity_id',
                       entity_id = 'entity_id',
                       value = 'value'),
    
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
    
    diagrammerSanitization = list(c("\\\"", "`"),
                                  c("\\'", "`"),
                                  c("\\\\", "/"),
                                  c("[^a-zA-Z0-9;)(,._/`-]", " ")),

    ### Used throughout for working with files
    encoding = "UTF-8",
    preventOverwriting = TRUE,
    
    ### Used throughout for suppressing messages
    silent = TRUE
    
  );
