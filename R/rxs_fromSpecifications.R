#' Title
#'
#' @param gs_url 
#' @param ws 
#' @param entitiesFilename 
#' @param valueTemplatesFilename 
#' @param definitionsFilename 
#' @param localBackup 
#' @param outputFile 
#' @param yamlMetadata 
#' @param author 
#' @param indent 
#' @param indentSpaces 
#' @param fullWidth 
#' @param commentCharacter 
#' @param fillerCharacter 
#' @param eC 
#' @param valueTemplateCols 
#' @param repeatingSuffix 
#' @param rootName 
#' @param silent 
#' @param instructionHeadingLevel
#' @param returnFullObject 
#'
#' @return
#' @export
#'
#' @examples
rxs_fromSpecifications <- function(gs_url = NULL,
                                   ws = list(entities = 'entities',
                                             valueTemplates = 'valueTemplates',
                                             definitions = 'definitions',
                                             instructions = 'instructions'),
                                   entitiesFilename = NULL,
                                   valueTemplatesFilename = NULL,
                                   definitionsFilename = NULL,
                                   localBackup = list(entities = NULL,
                                                      valueTemplates= NULL,
                                                      definitions = NULL),
                                   outputFile = NULL,
                                   yamlMetadata = list(title = "Systematic Review Extraction Script Template",
                                                       author = NULL,
                                                       date = format(Sys.time(), '%d %b %Y at %H:%M:%S')),
                                   author = NULL,
                                   indent = TRUE,
                                   indentSpaces = 2,
                                   fullWidth = 80,
                                   commentCharacter = "#",
                                   fillerCharacter = "#",
                                   eC = metabefor::opts$get(entityColNames),
                                   valueTemplateCols = metabefor::opts$get(valueTemplateColNames),
                                   repeatingSuffix = "__1__",
                                   rootName = "study",
                                   silent=FALSE,
                                   instructionHeadingLevel = 3,
                                   returnFullObject = TRUE) {
  
  ###---------------------------------------------------------------------------
  ### Get options
  ###---------------------------------------------------------------------------
  
  diagrammerSanitization <-
    metabefor::opts$get('diagrammerSanitization');
  
  extractionOverview_list_intro <-
    metabefor::opts$get('extractionOverview_list_intro');
  
  extractionOverview_compact_intro <-
    metabefor::opts$get('extractionOverview_compact_intro');
  
  ###---------------------------------------------------------------------------
  ### Import sheets, if sheets identifier (gs_url) was provided
  ###---------------------------------------------------------------------------
  
  entities <- FALSE;
  definitions <- NULL; ### In case the full object is requested but
                       ### no definitions are loaded
  if (!is.null(gs_url)) {
    tryCatch({
      googlesheets4::gs4_deauth();
      sheetNames <- googlesheets4::sheet_names(gs_url);
    },
    error = function(e) {
      if (!silent) {
        cat("You specified a google sheet, but I have problems",
            "accessing it (error: '",
            e$message,
            "'). Trying to access local files.\n");
      }
      if (getOption("metabefor.debug", FALSE)) {
        cat0("Error message:\n  ",
             e$message,
             "\n");
      }
    });
    
    if (!(ws$entities %in% sheetNames)) {
      stop("In the google sheet you specified, the worksheet names are ",
           vecTxtQ(sheetNames), ", while in argument `ws$entities`, you ",
           "passed '", ws$entities, "' as the name of the worksheet ",
           "that specifies the entities to extract.");
    }
    
    if (!(ws$valueTemplates %in% sheetNames)) {
      stop("In the google sheet you specified, the worksheet names are ",
           vecTxtQ(sheetNames), ", while in argument `ws$valueTemplates`, ",
           "you passed '", ws$valueTemplates, "' as the name of the worksheet ",
           "that specifies the value templates to use.");
    }
    
    entities <- as.data.frame(
      googlesheets4::read_sheet(gs_url, sheet = ws$entities)
    );

    valueTemplates <- as.data.frame(
      googlesheets4::read_sheet(gs_url, sheet = ws$valueTemplates)
    );
    
    if (!is.null(ws$definitions) && (ws$definitions %in% sheetNames)) {
      definitions <- as.data.frame(
        googlesheets4::read_sheet(gs_url, sheet = ws$definitions)
      );
    } else {
      definitions <- NULL;
    }
    
    if (!is.null(ws$instructions) && (ws$instructions %in% sheetNames)) {
      instructionSheet <- as.data.frame(
        googlesheets4::read_sheet(gs_url, sheet = ws$instructions)
      );
    } else {
      instructionSheet <- NULL;
    }
    
    if (!silent) {
      cat("Successfully read the extraction script specifications from Google sheets.\n");
    }
    
  }      
  
  ###---------------------------------------------------------------------------
  ### Read sheets from local file
  ###---------------------------------------------------------------------------
  
  ### If the sheets identifier was not provided, or loading it failed,
  ### load from a local file
  if (!is.data.frame(entities)) {

    ### Check whether the files exist
    if (!is.null(entitiesFilename)) {
      if (!file.exists(entitiesFilename)) {
        stop("You specified a filename for 'entitiesFilename' ('",
             entitiesFilename, "'), but it does not exist.");
      }
    } else {
      stop("Either a google sheets URL was not provided in gs_url, ",
           "or loading the sheets failed; and you did not provide ",
           "a filename in 'entitiesFilename'. That means that I cannot ",
           "load the extraction script specifications.");
    }

    if (!is.null(valueTemplatesFilename)) {
      if (!file.exists(valueTemplatesFilename)) {
        stop("You specified a filename for 'valueTemplatesFilename' ('",
             valueTemplatesFilename, "'), but it does not exist.");
      }
    } else {
      stop("Either a google sheets URL was not provided in gs_url, ",
           "or loading the sheets failed; and you did not provide ",
           "a filename in 'valueTemplatesFilename'. That means that I cannot ",
           "load the extraction script specifications.");
    }

    entities <- utils::read.csv(entitiesFilename,
                                stringsAsFactors = FALSE);
    valueTemplates <- utils::read.csv(valueTemplatesFilename,
                                      stringsAsFactors = FALSE);
    if (!is.null(definitionsFilename)) {
      definitions <- utils::read.csv(definitionsFilename,
                                     stringsAsFactors = FALSE);
    }

    if (!silent) {
      cat("Succesfully read the extraction script specifications from local files.\n");
    }

  }
  
  ###---------------------------------------------------------------------------
  ### Sanitize identifiers
  ###---------------------------------------------------------------------------
  
  ### Sanitize whitespace and unpermitted characters
  entities[[eC$identifierCol]] <- gsub("[^a-zA-Z0-9_.]+", "",
                                       entities[[eC$identifierCol]]);
  entities[[eC$parentCol]] <- gsub("[^a-zA-Z0-9_.]+", "",
                                   entities[[eC$parentCol]]);
  entities[[eC$entityRefCol]] <- gsub("[^a-zA-Z0-9_.]+", "",
                                      entities[[eC$entityRefCol]]);
  entities[[eC$valueTemplateCol]] <- gsub("[^a-zA-Z0-9_.]+", "",
                                          entities[[eC$valueTemplateCol]]);
  valueTemplates[[valueTemplateCols$identifierCol]] <-
    gsub("[^a-zA-Z0-9_.]+", "",
         valueTemplates[[valueTemplateCols$identifierCol]]);
  
  ###---------------------------------------------------------------------------
  ### Write local backup, if need be
  ###---------------------------------------------------------------------------
  
  if (!is.null(localBackup$entities)) {

    if (any(unlist(lapply(entities, is.list)))) {
      entitiesToWrite <-
        as.data.frame(
          lapply(
            entities,
            function(column) {
              if (is.list(column)) {
                return(
                  vecTxtQ(
                    as.character(
                      unlist(
                        column
                      )
                    )
                  )
                );
              } else {
                return(column);
              }
            }
          )
        );
    } else {
      entitiesToWrite <- entities;
    }
    
    if (tolower(tools::file_ext(localBackup$entities)) == "csv") {
      utils::write.csv(entitiesToWrite,
                       row.names=FALSE,
                       localBackup$entities);
    } else if (tolower(tools::file_ext(localBackup$entities)) == "xlsx") {
      openxlsx::write.xlsx(
        entitiesToWrite,
        localBackup$entities
      );
    } else {
      stop("For the entities spreadsheet, you passed an extension implying ",
           "you want me to export to a format I don't know (",
           tools::file_ext(localBackup$entities), ")!");
    }
    
    if (!silent) {
      cat0("Stored local backup of entities to '", localBackup$entities, "'.\n");
    }
  }
  if (!is.null(localBackup$valueTemplates)) {
    utils::write.csv(valueTemplates,
                     row.names=FALSE,
                     localBackup$valueTemplates);
    if (!silent) {
      cat0("Stored local backup of value templates to '", localBackup$valueTemplates, "'.\n");
    }
  }
  if (!is.null(localBackup$definitions) && !is.null(definitions)) {
    utils::write.csv(definitions,
                     row.names=FALSE,
                     localBackup$definitions);
    if (!silent) {
      cat0("Stored local backup of definitions to '", localBackup$definitions, "'.\n");
    }
  }
  if (!is.null(localBackup$instructions) && !is.null(instructions)) {
    utils::write.csv(instructions,
                     row.names=FALSE,
                     localBackup$instructions);
    if (!silent) {
      cat0("Stored local backup of instructions to '", localBackup$instructions, "'.\n");
    }
  }
  
  ###---------------------------------------------------------------------------
  ### Process entities and value templates into rxsStructure
  ###---------------------------------------------------------------------------
  
  rxsStructure <- rxs_parseSpecifications(
    entities = entities,
    valueTemplates = valueTemplates,
    definitions = definitions,
    eC = eC,
    valueTemplateCols = valueTemplateCols,
    rootName = rootName
  );

  rxsTemplate <- rxs_buildTemplate(
    rxsStructure = rxsStructure,
    yamlMetadata = yamlMetadata,
    indent = indent,
    indentSpaces = indentSpaces,
    fullWidth = fullWidth,
    commentCharacter = commentCharacter,
    fillerCharacter = fillerCharacter,
    eC = eC,
    repeatingSuffix = repeatingSuffix,
    silent=silent
  );
  
  if (!silent) {
    cat("Parsed extraction script specifications into extraction script template.\n");
  }
  
  ###---------------------------------------------------------------------------
  ### rxsTree Diagram
  ###---------------------------------------------------------------------------
  
  rxsTreeDiagram_simple_prep <-
    data.tree::Clone(
      rxsStructure$parsedEntities$extractionScriptTree
    );
  
  rxsTreeDiagram_simple_prep$Do(
    function(node) {
      data.tree::SetNodeStyle(
        node,
        label =
          sanitize_for_DiagrammeR(
            node$title
          )
      );
    }
  );
  
  rxsTreeDiagram_simple <-
    data.tree::ToDiagrammeRGraph(
      rxsTreeDiagram_simple_prep
    );
  
  rxsTreeDiagram_simple <-
    apply_graph_theme(rxsTreeDiagram_simple,
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
                      c("fillcolor", "#FFFFFF", "node"));
  
  
  if (!silent) {
    cat("Created diagrams representing the extraction tree.\n");
  }
  
  ###---------------------------------------------------------------------------
  ### Extraction instructions
  ###---------------------------------------------------------------------------
  
  if (!is.null(instructionSheet)) {
    instructions <-
      paste0(
        "\n\n",
        ufs::repStr("#", instructionHeadingLevel), " ",
        " Extraction instructions\n\n",
        paste0(
          lapply(
            1:nrow(instructionSheet),
            function(i) {
              return(
                paste0(
                  "\n\n",
                  ufs::repStr("#", instructionHeadingLevel+1), " ",
                  instructionSheet$title[i], "\n\n",
                  instructionSheet$description[i]
                )
              );
            }
          ),
          collapse = "\n\n"
        )
      );
  } else {
    instructions <- "No extraction instructions specified.";
  }
  
  ###---------------------------------------------------------------------------
  ### Entity overview: list
  ###---------------------------------------------------------------------------
  
  entityOverview_list <-
    rxsStructure$parsedEntities$extractionScriptTree$Get(
      function(node) {
        if (node$isRoot) {
          return(NULL);
        } else {
          res <- ufs::heading(
            node$title,
            headingLevel = instructionHeadingLevel + 1,
            cat = FALSE
          );
          
          if (is.null(node$valueTemplate)) {
            type <- "Entity Container";
          } else {
            type <- "Extractable Entity";
          }
          
          res <-
            paste0(
              res,
              node$description,
              "\n\n**Type:** ",
              type,
              "  \n**Identifier:** `",
              node$name
            );
          
          if (!is.null(node$valueTemplate)) {
            res <-
              paste0(
                res,
                "`  \n**Value template**: `",
                node$valueTemplate
              );
          }
          
          res <-
            paste0(
              res,
              "`  \n**Repeating**: `",
              ifelse(is.null(node$repeating) || !node$repeating,
                     "FALSE",
                     "TRUE")
            );
          
          res <-
            paste0(
              res,
              "`  \n**Path in extraction script tree:** `",
              paste0(node$path, collapse=" > "),
              "`\n\n-----\n\n"
            );
          return(res);
        }
      }
    );
  
  entityOverview_list <-
    paste0(
      ufs::heading(
        "Entity overview (list)",
        headingLevel = instructionHeadingLevel,
        cat = FALSE
      ),
      extractionOverview_list_intro,
      "\n\n",
      paste0(
        unlist(
          entityOverview_list[!unlist(lapply(entityOverview_list, is.na))]
        ),
        collapse = ""
      )
    );
  
  ###---------------------------------------------------------------------------
  ### Entity overview: compact
  ###---------------------------------------------------------------------------
  
  entityOverview_compact <-
    rxsStructure$parsedEntities$extractionScriptTree$Get(
      function(node) {
        if (node$isRoot) {
          return(NULL);
        } else {
          res <- ufs::heading(
            node$title,
            headingLevel = instructionHeadingLevel + 1,
            cat = FALSE
          );
          
          if (is.null(node$valueTemplate)) {
            type <- "Entity Container";
          } else {
            type <- "Extractable Entity";
          }
          
          res <-
            paste0(
              res,
              node$description,
              "\n\n**Type:** ",
              type,
              "  \n**Identifier:** `",
              node$name
            );
          
          if (!is.null(node$valueTemplate)) {
            res <-
              paste0(
                res,
                "`  \n**Value template**: `",
                node$valueTemplate
              );
          }
          
          res <-
            paste0(
              res,
              "`  \n**Repeating**: `",
              ifelse(is.null(node$repeating) || !node$repeating,
                     "FALSE",
                     "TRUE")
            );
          
          res <-
            paste0(
              res,
              "`  \n**Path in extraction script tree:** `",
              paste0(node$path, collapse=" > "),
              "`\n\n-----\n\n"
            );
          return(res);
        }
      }
    );

  entityOverview_compact <-
    paste0(
      ufs::heading(
        "Entity overview (compact)",
        headingLevel = instructionHeadingLevel,
        cat = FALSE
      ),
      extractionOverview_compact_intro,
      "\n\n",
      paste0(
        unlist(
          entityOverview_compact[!unlist(lapply(entityOverview_compact, is.na))]
        ),
        collapse = ""
      )
    );
  
  ###---------------------------------------------------------------------------
  ### Prepare result
  ###---------------------------------------------------------------------------
  
  if (returnFullObject) {
    res <- list(rxsSpecification = list(entities = entities,
                                        valueTemplates = valueTemplates,
                                        definitions = definitions,
                                        eC = eC,
                                        valueTemplateCols = valueTemplateCols,
                                        rootName = rootName,
                                        yamlMetadata = yamlMetadata,
                                        indent=indent,
                                        indentSpaces=indentSpaces,
                                        commentCharacter = commentCharacter,
                                        fillerCharacter = fillerCharacter),
                rxsStructure = rxsStructure,
                rxsTreeDiagram_simple = rxsTreeDiagram_simple,
                rxsTemplate = rxsTemplate,
                rxsInstructions = instructions,
                entityOverview_list = entityOverview_list,
                entityOverview_compact = entityOverview_compact);
    class(res) <- "rxsStructure";
  } else {
    res <- rxsTemplate;
  }
  
  ###---------------------------------------------------------------------------
  ### Potential write template and then return result
  ###---------------------------------------------------------------------------
  
  if (!is.null(outputFile)) {
    if (isTRUE(outputFile)) {
      ### Write to current working directory
      fileToWriteTo <- file.path(getwd(), "template.rxs.Rmd");
    } else if (is.character(outputFile)) {
      ### Path is specified in 'outputFile'
      fileToWriteTo <- outputFile;
    }
    writeLines(paste0(unlist(rxsTemplate), collapse="\n"),
               fileToWriteTo);
    if (!silent) {
      cat0("Successfully wrote extraction script template to '",
           fileToWriteTo, "'.\n");
    }
    invisible(res);
  } else {
    return(res);
  }

}
