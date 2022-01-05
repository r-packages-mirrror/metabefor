rxs_buildTemplate <- function(rxsStructure,
                              rxsSpecification,
                              yamlMetadata = list(title = "Systematic Review Extraction Script Template",
                                                  author = NULL,
                                                  date = format(Sys.time(), '%Y-%m-%d at %H:%M:%S %Z (UTC%z)')),
                              indent = TRUE,
                              indentSpaces = 2,
                              fullWidth = 78,
                              module = NULL,
                              commentCharacter = "#",
                              fillerCharacter = "#",
                              eC = metabefor::opts$get("entityColNames"),
                              repeatingSuffix = "__1__",
                              silent=FALSE) {
  
  rxsVersion <- metabefor::opts$get("rxsVersion");
  rxsCurrentNodeName <- metabefor::opts$get("rxsCurrentNodeName");
  rxsObjectName <- metabefor::opts$get("rxsObjectName");
  rxsTemplateSpecName <- metabefor::opts$get("rxsTemplateSpecName");
  uniqueSourceIdName <- metabefor::opts$get("uniqueSourceIdName");
  sourceIdValidation <- metabefor::opts$get("sourceIdValidation");
  
  if (!("rxsStructure" %IN% class(rxsStructure))) {
    stop("The class of the object provided as argument 'rxsStructure' is not ",
         "'rxsStructure' (but instead ", vecTxtQ(class(rxsStructure)), ").");
  }
  
  if (rxsVersion < "0.3.0") {
    rxsObjectName <- rxsStructure$parsedEntities$extractionScriptTree$root$name;
  }

  ###---------------------------------------------------------------------------
  ### Study identifier chunk
  ###---------------------------------------------------------------------------

  sourceIdFragment <-
    rxs_fg_sourceId();
    
  ###---------------------------------------------------------------------------
  ### Script chunk
  ###---------------------------------------------------------------------------
  
  scriptChunk <-
    rxs_fg_dispatcher(node = rxsStructure$parsedEntities$extractionScriptTree,
                      valueTemplates = rxsStructure$parsedValueTemplates,
                      indent = indent,
                      indentSpaces = indentSpaces,
                      fullWidth = fullWidth,
                      commentCharacter = commentCharacter,
                      fillerCharacter = fillerCharacter,
                      eC = eC,
                      repeatingSuffix = repeatingSuffix,
                      silent=silent);

  ### If this extraction script has any recursing entities, include them
  ### separately
  if (length(rxsStructure$parsedEntities$recursingNodes) > 0) {
    recursingEntitiesChunk <-
      rxs_fg_recursingEntities(rxsStructure$parsedEntities$recursingNodes,
                               valueTemplates = rxsStructure$parsedValueTemplates,
                               indent = indent,
                               indentSpaces = indentSpaces,
                               fullWidth = fullWidth,
                               commentCharacter = commentCharacter,
                               fillerCharacter = fillerCharacter,
                               eC = eC,
                               repeatingSuffix = repeatingSuffix,
                               silent=silent);
    recursingEntitiesChunk <-
      c("```{r rxsChunk-recursingEntities, eval=FALSE, echo=FALSE}",
        unlist(recursingEntitiesChunk),
        "```");
  } else {
    recursingEntitiesChunk <-
      NA;
  }

  if (is.null(yamlMetadata$title)) {
    yamlTitle <- NULL;
  } else {
    yamlTitle <- paste0("title: \"", yamlMetadata$title, "\"");
  }
  if (is.null(yamlMetadata$author)) {
    yamlAuthor <- NULL;
  } else {
    yamlAuthor <- paste0("author: \"", yamlMetadata$author, "\"");
  }
  if (is.null(yamlMetadata$date)) {
    yamlDate <- NULL;
  } else {
    yamlDate <- paste0("date: \"", yamlMetadata$date, "\"");
  }

  yamlHeader <-
    c("---",
      yamlTitle,
      yamlAuthor,
      yamlDate,
      "output:",
      "  html_document:",
      "    self-contained: yes",
      "    toc: false",
      "params:",
      paste0("  rxsVersion = \"", metabefor::opts$get("rxsVersion"), "\""),
      "editor_options:",
      "  chunk_output_type: console",
      "---",
      "");
  
  if (rxsVersion < "0.3.0") {
    setupChunkLabel <- "rxsChunk-setup";
    showExtractedDataChunkLabel <- "rxsChunk-show-extracted-data";
    validationChunkLabel <- "rxsChunk-validation";
  } else {
    setupChunkLabel <-
      paste0("rxs-setup-chunk-", metabefor::randomSlug(6));
    showExtractedDataChunkLabel <-
      paste0("rxs-show-extracted-data-chunk-", metabefor::randomSlug(6));
    validationChunkLabel <-
      paste0("rxs-validation-chunk-", metabefor::randomSlug(6));
  }

  ### Setup chunk inclusion (for Rxs version >= 0.3.0)
  setupChunkInclusion <-
    c(paste0("```{r ", setupChunkLabel, ", echo=FALSE, results='hide'}"),
      "```");
  
  ### Actual setup chunk
  setupChunk <- c(paste0("```{r ", setupChunkLabel, ", include=FALSE, messages=FALSE}"),
                  "### First check for (and perhaps install) metabefor",
                  "if (!('metabefor' %in% row.names(installed.packages()))) {",
                  "  install.packages('metabefor', repos='http://cran.rstudio.com');",
                  "}",
                  #"",
                  #"### Other packages",
                  #"metabefor::checkPkgs('googlesheets4');   ### To import data from google sheets in metabefor",
                  #"metabefor::checkPkgs('jsonlite');        ### To import a list of country codes in metabefor",
                  #"metabefor::checkPkgs('data.tree');       ### To work with data structured in a tree in metabefor",
                  "",
                  "### Settings",
                  "knitr::opts_chunk$set(echo = FALSE);     ### Suppress R command printing",
                  "knitr::opts_chunk$set(comment = NA);     ### Suppress output prefix",
                  "```");


  printable_eC <-
    paste0(c("eC <- list(", rep(repStr(11), length(eC)-1)),
           paste0(names(eC), "=", '"', unlist(eC), '"'),
           c(character(length(eC)-1), ");"), collapse=",\n");

  valueTemplateCols <-
    attributes(rxsStructure$parsedValueTemplates)$originalColNames;

  printableValueTemplateCols <-
    paste0(c("valueTemplateCols <- list(", rep(repStr(26), length(valueTemplateCols)-1)),
           paste0(names(valueTemplateCols), "=", '"', unlist(valueTemplateCols), '"'),
           c(character(length(valueTemplateCols)-1), ");"), collapse=",\n");


  fieldnameChunk <- c("```{r rxsChunk-fieldnames}",
                      printable_eC,
                      "",
                      printableValueTemplateCols,
                      "```");
  
  showExtractedDataChunk <-
    c(paste0("```{r ", showExtractedDataChunkLabel, "}"),
      paste0("metabefor::rxs_partial(", rxsObjectName, ");"),
      "```");

  validationChunk <-
    c(paste0("```{r ", validationChunkLabel, ", results='asis'}"),
      paste0("#metabefor::rxs_validation(", rxsObjectName, ");"),
      "metabefor::heading('Validation results', headingLevel = 1);",
      paste0("#rxs_validation(", rxsObjectName, ","),
      "#               rxsStructure = fullResults$rxsStructure);",
      paste0("if (length(", rxsObjectName, "$validationResults) > 2) {"),
      paste0("  cat(paste0('- ', ", rxsObjectName, "$validationResults), sep='\n');"),
      "} else {",
      "  cat('Validation successful!');",
      "}",
      "```");

  ### Rxs template specification
  rxsTemplateSpecificationChunkLabel <-
    paste0("rxs-template-specification-chunk-", metabefor::randomSlug(6));
  
  chunkOpts <- "echo=FALSE, results='hide'";
  
  ### Include form specification chunk
  rxsTemplateSpecificationChunkInclusion <-
    c(paste0("```{r ", rxsTemplateSpecificationChunkLabel, chunkOpts, "}"),
      "```");

  rxsTemplateSpecificationChunkSourceCode <- c(
    htmlComment("Here, the original Rxs template specification is included."),
    "",
    paste0("```{r ",
           rxsTemplateSpecificationChunkLabel,
           ", echo=FALSE}"),
    "",
    paste0(rxsTemplateSpecName, " <- "),
    paste0("  ", utils::capture.output(dput(rxsSpecification, control="all"))),
    "",
    "```"
  );
  
  setRxsObjectClass <-
    paste0("class(", rxsObjectName, ") <- c('rxs', 'rxsObject', class(", rxsObjectName, "));");
  rxsMetadata <-
    paste0(rxsObjectName, "$rxsMetadata <- list(rxsVersion='",
           rxsVersion, "', moduleId=",
           ifelse(is.null(module), "NULL", paste0('"', module, '"')),
           ", id=", uniqueSourceIdName, ");");
  
  sourceIdLocation <- paste0(rxsObjectName, "$rxsMetadata$id");
  
  validateSourceId <-
    paste0("if (!(",
           gsub("VALUE", sourceIdLocation, sourceIdValidation),
           ")) {\n  stop(\"The source identifier you specified, '\", ",
           sourceIdLocation, ", \n       \"', does not validate (i.e., it does ",
           "not match the predefined format)!\");\n}");

  if (rxsVersion < "0.3.0") {
    res <- c(yamlHeader,
             setupChunk,
             "",
             fieldnameChunk,
             "",
             "```{r rxsChunk, echo=FALSE}",
             studyIdChunk,
             scriptChunk,
             setRxsObjectClass,
             rxsMetadata,
             "```",
             "",
             ifelse(!is.na(recursingEntitiesChunk),
                    c(recursingEntitiesChunk, ""),
                    ""),
             showExtractedDataChunk,
             "",
             validationChunk,
             "",
             rxsTemplateSpecificationChunkSourceCode,
             "");
  } else {
    res <- c(yamlHeader,
             setupChunkInclusion,
             "",
             "```{r rxsChunk, echo=FALSE}",
             sourceIdFragment,
             "",
             "",
             scriptChunk,
             setRxsObjectClass,
             rxsMetadata,
             validateSourceId,
             "```",
             "",
             ifelse(!is.na(recursingEntitiesChunk),
                    c(recursingEntitiesChunk, ""),
                    ""),
             showExtractedDataChunk,
             "",
             validationChunk,
             "",
             setupChunk,
             "",
             rxsTemplateSpecificationChunkSourceCode,
             "");
  }

  return(res);

}
