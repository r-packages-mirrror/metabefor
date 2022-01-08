rxs_buildTemplate <- function(rxsStructure,
                              rxsSpecification,
                              errorOnFailingValidation = errorOnFailingValidation,
                              yamlMetadata = NULL,
                              module = NULL,
                              silent = metabefor::opts$get("silent")) {
  
  eC <- metabefor::opts$get("entityColNames");
  rxsVersion <- metabefor::opts$get("rxsVersion");
  rxsCurrentNodeName <- metabefor::opts$get("rxsCurrentNodeName");
  rxsObjectName <- metabefor::opts$get("rxsObjectName");
  rxsTemplateSpecName <- metabefor::opts$get("rxsTemplateSpecName");
  uniqueSourceIdName <- metabefor::opts$get("uniqueSourceIdName");
  extractorIdName <- metabefor::opts$get("extractorIdName");
  sourceIdValidation <- metabefor::opts$get("sourceIdValidation");
  extractorIdValidation <- metabefor::opts$get("extractorIdValidation");
  texts <- metabefor::opts$get("texts");
  
  indent <- metabefor::opts$get("indentDefault");
  indentSpaces <- metabefor::opts$get("indentSpaces");
  fullWidth <- metabefor::opts$get("fullWidth");
  commentCharacter <- metabefor::opts$get("commentCharacter");
  fillerCharacter <- metabefor::opts$get("fillerCharacter");
  repeatingSuffix <- metabefor::opts$get("repeatingSuffix");
  
  if (!("rxsStructure" %IN% class(rxsStructure))) {
    stop("The class of the object provided as argument 'rxsStructure' is not ",
         "'rxsStructure' (but instead ", vecTxtQ(class(rxsStructure)), ").");
  }

  rxsRootName <- rxsStructure$parsedEntities$extractionScriptTree$root$name;
  
  if (rxsVersion < "0.3.0") {
    rxsObjectName <- rxsRootName;
  }
  
  ###---------------------------------------------------------------------------
  ### Opening and closing remarks
  ###---------------------------------------------------------------------------

  openingBlock <-
    c(
      htmlComment(),
      htmlComment(" ", padding=" "),
      htmlComment(texts$openingRemarks, padding=" "),
      htmlComment(" ", padding=" "),
      htmlComment()
    );

  closingBlock <-
    c(
      htmlComment(),
      htmlComment(" ", padding=" "),
      htmlComment(texts$closingRemarks, padding=" "),
      htmlComment(" ", padding=" "),
      htmlComment()
    );
  
  ###---------------------------------------------------------------------------
  ### Source and extractor identifier chunk
  ###---------------------------------------------------------------------------

  sourceIdFragment <-
    rxs_fg_sourceId();
  
  extractorIdFragment <-
    rxs_fg_extractorId();
  
  ###---------------------------------------------------------------------------
  ### Script chunk
  ###---------------------------------------------------------------------------
  
  scriptChunk <-
    rxs_fg_dispatcher(node = rxsStructure$parsedEntities$extractionScriptTree,
                      valueTemplates = rxsStructure$parsedValueTemplates,
                      silent=silent);

  ### If this extraction script has any recursing entities, include them
  ### separately
  if (length(rxsStructure$parsedEntities$recursingNodes) > 0) {
    recursingEntitiesChunk <-
      rxs_fg_recursingEntities(rxsStructure$parsedEntities$recursingNodes,
                               valueTemplates = rxsStructure$parsedValueTemplates,
                               silent=silent);
    recursingEntitiesChunk <-
      c("```{r rxsChunk-recursingEntities, eval=FALSE, echo=FALSE}",
        unlist(recursingEntitiesChunk),
        "```");
  } else {
    recursingEntitiesChunk <-
      NA;
  }
  
  yamlTitle <-
    paste0(
      "title: \"`r paste0('Rxs for \\'', ",
      uniqueSourceIdName,
      ", '\\' (', metabefor::knittedFileSansExt(), ')')`\""
    );
  yamlAuthor <-
    paste0("author: \"`r paste0('Extractor: ', ", extractorIdName, ")`\"");
  yamlDate <-
    "date: \"`r format(Sys.time(), '%Y-%m-%d at %H:%M:%S %Z (UTC%z)')`\"";

  if (!is.null(yamlMetadata)) {
    if (!is.null(yamlMetadata$title)) {
      yamlTitle <- paste0("title: \"", yamlMetadata$title, "\"");
    }
    if (!is.null(yamlMetadata$author)) {
      yamlAuthor <- paste0("author: \"", yamlMetadata$author, "\"");
    }
    if (!is.null(yamlMetadata$date)) {
      yamlDate <- paste0("date: \"", yamlMetadata$date, "\"");
    }
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
      paste0("  rxsVersion: \"", rxsVersion, "\""),
      paste0("  rxsRootName: \"", rxsRootName, "\""),
      paste0("  rxsObjectName: \"", rxsObjectName, "\""),
      #paste0("  uniqueSourceIdName: \"", uniqueSourceIdName, "\""),
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
      "metabefor::heading('Validation results', headingLevel = 1);",
      paste0("metabefor::rxs_validation(\n  ", rxsObjectName,
             ",\n  stopOnFailure = ", errorOnFailingValidation, ");"),
      paste0("cat(paste0('- ', ", rxsObjectName, "$validationLog), sep='\\n');"),
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
           ", id=", uniqueSourceIdName,
           ", extractorId=", extractorIdName,
           ");");
  
  sourceIdLocation <- paste0(rxsObjectName, "$rxsMetadata$id");
  extractorIdLocation <- paste0(rxsObjectName, "$rxsMetadata$extractorId");
  
  validateSourceId <-
    paste0("if (!(",
           gsub("VALUE", sourceIdLocation, sourceIdValidation),
           ")) {\n  stop(metabefor::wrap_error(\"The source identifier you specified, '\", ",
           sourceIdLocation, ", \n       \"', does not validate (i.e., it does ",
           "not match the predefined format)!\"));\n}");

  validateExtractorId <-
    paste0("if (!(",
           gsub("VALUE", extractorIdLocation, extractorIdValidation),
           ")) {\n  stop(metabefor::wrap_error(\"The extractor identifier you specified, '\", ",
           extractorIdLocation, ", \n       \"', does not validate (i.e., it does ",
           "not match the predefined format)!\"));\n}");
  
  if (rxsVersion < "0.3.0") {
    res <- c(setupChunk,
             "",
             fieldnameChunk,
             "",
             openingBlock,
             "",
             "```{r rxs-extraction-chunk, echo=FALSE}",
             sourceIdFragment,
             "",
             extractorIdFragment,
             "",
             "",
             scriptChunk,
             setRxsObjectClass,
             rxsMetadata,
             rep(rxs_fg_layoutVars(indent = 0)$lineFiller, 3),
             "```",
             "",
             closingBlock,
             rep("", 10),
             "```{r rxs-metadata-and-validation-chunk, echo=FALSE}",
             validateSourceId,
             validateExtractorId,
             "```",
             "",
             ifelse(!is.na(recursingEntitiesChunk),
                    c(recursingEntitiesChunk, ""),
                    ""),
             showExtractedDataChunk,
             "",
             validationChunk,
             "",
             yamlHeader,
             "",
             rxsTemplateSpecificationChunkSourceCode,
             "");
  } else {
    res <- c(setupChunkInclusion,
             "",
             openingBlock,
             "",
             "```{r rxs-extraction-chunk, echo=FALSE}",
             sourceIdFragment,
             "",
             extractorIdFragment,
             "",
             "",
             scriptChunk,
             setRxsObjectClass,
             rxsMetadata,
             rep(rxs_fg_layoutVars(indent = 0)$lineFiller, 3),
             "```",
             "",
             closingBlock,
             rep("", 10),
             "```{r rxs-metadata-and-validation-chunk, echo=FALSE}",
             validateSourceId,
             validateExtractorId,
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
             yamlHeader,
             "",
             rxsTemplateSpecificationChunkSourceCode,
             "");
  }

  return(res);

}
