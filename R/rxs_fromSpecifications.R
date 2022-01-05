#' Generate an Rxs template
#' 
#' This function processes an R extraction script template specification in
#' a spreadsheet format and produce the corresponding R extraction script
#' template file (an R Markdown file with the extension `.rxs.Rmd`)
#'
#' @param x A Google Sheets URL (make sure it's viewable by anybody
#' with the link!) or either a character
#' value with a valid path to a spreadsheet (`.xlsx`) with the worksheets, or
#' a named list of character strings each pointing to a `.csv` file with the
#' relevant worksheet, where the names of each character value represent the
#' names of the worksheets contained in each file.
#' @param outputFile When not working with modules, the path to a directory
#' and file where the Rxs template will be stored. If `NULL`, the produced
#' Rxs template (or the full object, depending on `returnFullObject`) is
#' returned visibly.
#' @param outputPath,outputFilenamePattern If `NULL`, the produced Rxs
#' templates are returned (visibly) in a list (or the full object is, depending
#' on `returnFullObject`). If not `NULL`, `outputPath` has
#' to be a valid path where the Rxs templates will be written to, and
#' `outputFilenamePattern` has to be a valid character value (i.e. a text
#' string) containing `%s` in the position where the module identifier will
#' be inserted in the filenames.
#' @param gs_url DEPRECATED - please use `x` instead!
#' A Google Sheets
#' URL (make sure it's viewable by anybody with the link!).
#' @param localFile DEPRECATED - please use `x` instead!
#' When reading from a local file, pass either a character
#' value with a valid path to a spreadsheet (`.xlsx`) with the worksheets, or
#' a named list of character strings each pointing to a `.csv` file with the
#' relevant worksheet, where the names of each character value represent the
#' names of the worksheets contained in each file.
#' @param graphTheme The graph theme to use.
#' @param ws The worksheet names: a named list with four character values
#' named `entities`, `valueTemplates`, `definitions`, and `instructions`.
#' @param localBackup If not `NULL`, either a path to a single `.xlsx` file to
#' save a local backup spreadsheet to, or a named list with character values,
#' where each name is the name of a worksheet (e.g. `entities`,
#' `valueTemplates`, `definitions`, and `instructions`), and each named value
#' is the path to store the corresponding worksheets will be stored as `.csv`
#' file.
#' @param yamlMetadata A names list containing character values named
#' `title`, `author`, and `date` which will be set in the Rmd file that is
#' the Rxs template as metadata.
#' @param indent Whether to use indentation to visually organise the Rxs
#' template. If TRUE, deeper nesting in the Rxs specification's hierarchy
#' will be visible as deeper indentation.
#' @param indentSpaces The number of spaces to use when identing.
#' @param fullWidth The maximum width of the Rxs template in characters.
#' @param commentCharacter The character used to signify comments - if this is
#' changed, R will throw errors (unless perhaps it once introduces another
#' comment symbol).
#' @param fillerCharacter The character used after the first character for
#' filling up space.
#' @param eC The entity columns; a named list with character values holding the
#' names of the columns in the `entities` worksheet of the spreadsheet. The
#' default values are stored in `metabefor::opts$get("entityColNames")` - if you
#' need to override these values, just reproduce that object.
#' @param valueTemplateCols The value template columns; a named list with
#' character values holding the names of the columns in the `entities`
#' worksheet of the spreadsheet. The default values are stored
#' in `metabefor::opts$get("valueTemplateColNames")` - if you need to
#' override these values, just reproduce that object.
#' @param instructionsCols The instructions worksheet columns: a names list with
#' character values holding the names of the columns in the `instructions`
#' worksheet of the spreadsheet. The default values are stored
#' in `metabefor::opts$get("instructionsColNames")` - if you need to
#' override these values, just reproduce that object.
#' @param definitionsCols The definitions worksheet columns: a names list with
#' character values holding the names of the columns in the `definitions`
#' worksheet of the spreadsheet. The default values are stored
#' in `metabefor::opts$get("definitionsColNames")` - if you need to
#' override these values, just reproduce that object.
#' @param repeatingSuffix The suffix to use for the entity identifiers/names
#' of repeating entities.
#' @param rootName The name of the root element
#' @param preventOverwriting Whether to prevent accidental overwriting of the
#' extraction templates.
#' @param ignoreModules Optionally, you can ignore modules specified in the
#' Rxs specification spreadsheet by setting `ignoreModules` to `TRUE`.
#' @param silent Whether to be silent or chatty.
#' @param instructionHeadingLevel The top-most heading level for the
#' instructions.
#' @param returnFullObject Whether to return the full object or just the
#' template.
#'
#' @return Either a full object, containing the template and other products,
#' or just the template.
#' 
#' @rdname rxs_templateBuilding
#' 
#' @export
#'
#' @examples
rxs_fromSpecifications <- function(x = NULL,
                                   outputFile = NULL,
                                   outputPath = NULL,
                                   outputFilenamePattern = NULL,
                                   localBackup = NULL,
                                   yamlMetadata = list(title = "Systematic Review Extraction Script Template",
                                                       author = NULL,
                                                       date = format(Sys.time(), '%Y-%m-%d at %H:%M:%S %Z (UTC%z)')),
                                   indent = TRUE,
                                   indentSpaces = 2,
                                   fullWidth = 78,
                                   commentCharacter = "#",
                                   fillerCharacter = "#",
                                   ws = metabefor::opts$get("rxsSheetnames"),
                                   eC = metabefor::opts$get("entityColNames"),
                                   valueTemplateCols = metabefor::opts$get("valueTemplateColNames"),
                                   instructionsCols = metabefor::opts$get("instructionsColNames"),
                                   definitionsCols = metabefor::opts$get("definitionsColNames"),
                                   repeatingSuffix = "__1__",
                                   rootName = "source",
                                   preventOverwriting = FALSE,
                                   silent=metabefor::opts$get("silent"),
                                   instructionHeadingLevel = 3,
                                   graphTheme = list(c("fontname", "Arial", "node")),
                                   ignoreModules = FALSE,
                                   returnFullObject = TRUE,
                                   gs_url = NULL,
                                   localFile = NULL) {
  
  ###---------------------------------------------------------------------------
  ### Get options
  ###---------------------------------------------------------------------------
  
  diagrammerSanitization <-
    metabefor::opts$get('diagrammerSanitization');
  
  extractionOverview_compact_intro <-
    metabefor::opts$get('extractionOverview_compact_intro');
  
  
  rxsVersion <- metabefor::opts$get("rxsVersion");
  rxsCurrentNodeName <- metabefor::opts$get("rxsCurrentNodeName");
  rxsObjectName <- metabefor::opts$get("rxsObjectName");
  rxsTemplateSpecName <- metabefor::opts$get("rxsTemplateSpecName");
  
  ###---------------------------------------------------------------------------
  ### Import sheets, if sheets identifier (gs_url) was provided
  ###---------------------------------------------------------------------------
  
  entities <- FALSE;
  definitions <- NULL; ### In case the full object is requested but
                       ### no definitions are loaded

  ### This function covers a situation where people specified
  ### the old, deprecated arguments.
  if (is.null(x)) {
    if (!is.null(gs_url)) {
      x <- gs_url;
    } else if (!is.null(localFile)) {
      x <- localFile;
    } else {
      stop("Specify either a `gs_url` or a `localFile`.");
    }
  }
  
  res <-
    metabefor::read_spreadsheet(
      x = x,
      localBackup = localBackup
    );

  entities <- res[[ws$entities]];
  valueTemplates <- res[[ws$valueTemplates]];
  if (!is.null(ws$definitions) && (ws$definitions %in% names(res))) {
    definitions <- res[[ws$definitions]];
  } else {
    definitions <- NULL;
  }
  if (!is.null(ws$instructions) && (ws$instructions %in% names(res))) {
    instructionSheet <- res[[ws$instructions]];
  } else {
    instructionSheet <- NULL;
  }
  
  msg(
    "Successfully read the extraction script ",
    "specifications from Google sheets.\n",
    silent = silent
  );
  
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
  entities[[eC$moduleCol]] <- gsub("[^a-zA-Z0-9_.]+", "",
                                   entities[[eC$moduleCol]]);
  valueTemplates[[valueTemplateCols$identifierCol]] <-
    gsub("[^a-zA-Z0-9_.]+", "",
         valueTemplates[[valueTemplateCols$identifierCol]]);
  
  # if (!is.null(gs_url)) {
  #   tryCatch({
  #     googlesheets4::gs4_deauth();
  #     sheetNames <- googlesheets4::sheet_names(gs_url);
  #   },
  #   error = function(e) {
  #     if (!silent) {
  #       cat("You specified a google sheet, but I have problems",
  #           "accessing it (error: '",
  #           e$message,
  #           "'). Trying to access local files.\n");
  #     }
  #     if (getOption("metabefor.debug", FALSE)) {
  #       cat0("Error message:\n  ",
  #            e$message,
  #            "\n");
  #     }
  #   });
  #   
  #   if (exists("sheetNames")) {
  # 
  #     if (!(ws$entities %in% sheetNames)) {
  #       stop("In the google sheet you specified, the worksheet names are ",
  #            vecTxtQ(sheetNames), ", while in argument `ws$entities`, you ",
  #            "passed '", ws$entities, "' as the name of the worksheet ",
  #            "that specifies the entities to extract.");
  #     }
  #     
  #     if (!(ws$valueTemplates %in% sheetNames)) {
  #       stop("In the google sheet you specified, the worksheet names are ",
  #            vecTxtQ(sheetNames), ", while in argument `ws$valueTemplates`, ",
  #            "you passed '", ws$valueTemplates, "' as the name of the worksheet ",
  #            "that specifies the value templates to use.");
  #     }
  #     
  #     entities <- as.data.frame(
  #       googlesheets4::read_sheet(gs_url, sheet = ws$entities)
  #     );
  # 
  #     valueTemplates <- as.data.frame(
  #       googlesheets4::read_sheet(gs_url, sheet = ws$valueTemplates)
  #     );
  #     
  #     if (!is.null(ws$definitions) && (ws$definitions %in% sheetNames)) {
  #       definitions <- as.data.frame(
  #         googlesheets4::read_sheet(gs_url, sheet = ws$definitions)
  #       );
  #     } else {
  #       definitions <- NULL;
  #     }
  #     
  #     if (!is.null(ws$instructions) && (ws$instructions %in% sheetNames)) {
  #       instructionSheet <- as.data.frame(
  #         googlesheets4::read_sheet(gs_url, sheet = ws$instructions)
  #       );
  #     } else {
  #       instructionSheet <- NULL;
  #     }
  #     
  #     if (!silent) {
  #       cat("Successfully read the extraction script specifications from Google sheets.\n");
  #     }
  #     
  #   }     
  #   
  # }
  
  ###---------------------------------------------------------------------------
  ### Read sheets from local file
  ###---------------------------------------------------------------------------
  
  # ### If the sheets identifier was not provided, or loading it failed,
  # ### load from a local file
  # if (!is.data.frame(entities)) {
  # 
  #   ### Check whether the files exist
  #   if (!is.null(entitiesFilename)) {
  #     if (!file.exists(entitiesFilename)) {
  #       stop("You specified a filename for 'entitiesFilename' ('",
  #            entitiesFilename, "'), but it does not exist.");
  #     }
  #   } else {
  #     stop("Either a google sheets URL was not provided in gs_url, ",
  #          "or loading the sheets failed; and you did not provide ",
  #          "a filename in 'entitiesFilename'. That means that I cannot ",
  #          "load the extraction script specifications.");
  #   }
  # 
  #   if (!is.null(valueTemplatesFilename)) {
  #     if (!file.exists(valueTemplatesFilename)) {
  #       stop("You specified a filename for 'valueTemplatesFilename' ('",
  #            valueTemplatesFilename, "'), but it does not exist.");
  #     }
  #   } else {
  #     stop("Either a google sheets URL was not provided in gs_url, ",
  #          "or loading the sheets failed; and you did not provide ",
  #          "a filename in 'valueTemplatesFilename'. That means that I cannot ",
  #          "load the extraction script specifications.");
  #   }
  # 
  #   entities <- utils::read.csv(entitiesFilename,
  #                               stringsAsFactors = FALSE);
  #   valueTemplates <- utils::read.csv(valueTemplatesFilename,
  #                                     stringsAsFactors = FALSE);
  #   if (!is.null(definitionsFilename)) {
  #     definitions <- utils::read.csv(definitionsFilename,
  #                                    stringsAsFactors = FALSE);
  #   }
  # 
  #   if (!silent) {
  #     cat("Succesfully read the extraction script specifications from local files.\n");
  #   }
  # 
  # }

  ###---------------------------------------------------------------------------
  ### Write local backup, if need be
  ###---------------------------------------------------------------------------
  
  # if (!is.null(localBackup$entities)) {
  #   
  #   ### Sometimes, for some odd reason, columns have the 'list' class;
  #   ### convert those to character.
  #   entitiesToWrite <-
  #     purgeListsFromDf(entities);
  # 
  #   if (tolower(tools::file_ext(localBackup$entities)) == "csv") {
  #     utils::write.csv(entitiesToWrite,
  #                      row.names=FALSE,
  #                      localBackup$entities);
  #   } else if (tolower(tools::file_ext(localBackup$entities)) == "xlsx") {
  #     openxlsx::write.xlsx(
  #       entitiesToWrite,
  #       localBackup$entities
  #     );
  #   } else {
  #     stop("For the entities spreadsheet, you passed an extension implying ",
  #          "you want me to export to a format I don't know (",
  #          tools::file_ext(localBackup$entities), ")!");
  #   }
  #   
  #   if (!silent) {
  #     cat0("Stored local backup of entities to '", localBackup$entities, "'.\n");
  #   }
  # }
  # if (!is.null(localBackup$valueTemplates)) {
  #   
  #   ### Sometimes, for some odd reason, columns have the 'list' class;
  #   ### convert those to character.
  #   valueTemplatesToWrite <-
  #     purgeListsFromDf(valueTemplates);
  #   
  #   utils::write.csv(valueTemplatesToWrite,
  #                    row.names=FALSE,
  #                    localBackup$valueTemplates);
  #   if (!silent) {
  #     cat0("Stored local backup of value templates to '", localBackup$valueTemplates, "'.\n");
  #   }
  # }
  # if (!is.null(localBackup$definitions) && !is.null(definitions)) {
  #   utils::write.csv(definitions,
  #                    row.names=FALSE,
  #                    localBackup$definitions);
  #   if (!silent) {
  #     cat0("Stored local backup of definitions to '", localBackup$definitions, "'.\n");
  #   }
  # }
  # if (!is.null(localBackup$instructions) && !is.null(instructions)) {
  #   utils::write.csv(instructions,
  #                    row.names=FALSE,
  #                    localBackup$instructions);
  #   if (!silent) {
  #     cat0("Stored local backup of instructions to '", localBackup$instructions, "'.\n");
  #   }
  # }
  
  
  ###---------------------------------------------------------------------------
  ### Extraction instructions
  ###---------------------------------------------------------------------------
  
  if (!is.null(instructionSheet)) {
    instructions <-
      paste0(
        "\n\n",
        repStr("#", instructionHeadingLevel), " ",
        " Extraction instructions\n\n",
        paste0(
          lapply(
            1:nrow(instructionSheet),
            function(i) {
              return(
                paste0(
                  "\n\n",
                  repStr("#", instructionHeadingLevel+1), " ",
                  instructionSheet[[instructionsCols$headingCol]][i], "\n\n",
                  instructionSheet[[instructionsCols$descriptionCol]][i]
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
  ### Set up results object
  ###---------------------------------------------------------------------------
  
  res <- list(rxsSpecification = list(entities = entities,
                                      valueTemplates = valueTemplates,
                                      definitions = definitions,
                                      instructionSheet = instructionSheet,
                                      eC = eC,
                                      valueTemplateCols = valueTemplateCols,
                                      rootName = rootName,
                                      yamlMetadata = yamlMetadata,
                                      indent=indent,
                                      indentSpaces=indentSpaces,
                                      commentCharacter = commentCharacter,
                                      fillerCharacter = fillerCharacter),
              rxsInstructions = instructions);
  
  class(res) <- "rxsStructure";

  ###---------------------------------------------------------------------------
  ### Process entities and value templates into rxsStructure
  ###---------------------------------------------------------------------------

  if (eC$moduleCol %in% names(entities)) {
    modules <- unique(entities[[eC$moduleCol]]);
    modules <- modules[!is.na(modules)];
    if ((length(modules) == 0) ||
        ((length(modules) == 1) && (nchar(modules) == 0))) {
      ### The column is there, but it's empty, so ignore it
      workingModularly <- FALSE;
    } else {
      ### We have at least one value
      workingModularly <- TRUE;
    }
  } else {
    workingModularly <- FALSE;
  }
  
  if (ignoreModules) {
    workingModularly <- FALSE;
  }
  
  if (workingModularly) {

    ### We have modules - write separate templates
    
    if (!silent) {
      msg("A `modules` column was found. Starting to parse the extraction ",
          "script specifications into extraction script templates for each ",
          "module.\n",
          silent=silent);
    }
    
    ### Fill in default name for entities with an omitted module
    emptyModuleIdentifiers <-
      is.na(entities[[eC$moduleCol]]) | (nchar(entities[[eC$moduleCol]]) == 0);

    if (any(emptyModuleIdentifiers)) {
      entities[emptyModuleIdentifiers, eC$moduleCol] <-
        metabefor::opts$get("moduleWithoutName");
    }
    
    ### Get full list of module identifiers again
    modules <- unique(entities[[eC$moduleCol]]);

    if (!silent) {
      msg("Found modules with identifiers ", vecTxtQ(modules), ".\n",
          silent=silent);
    }
    
    rxsStructure_modules <- list();
    rxsTemplate_modules <- list();
    
    for (currentModule in modules) {

      rxsStructure_modules[[currentModule]] <- rxs_parseSpecifications(
        entities = entities[entities[[eC$moduleCol]] == currentModule, ],
        valueTemplates = valueTemplates,
        definitions = definitions,
        eC = eC,
        valueTemplateCols = valueTemplateCols,
        rootName = rootName
      );

      rxsTemplate_modules[[currentModule]] <- rxs_buildTemplate(
        rxsStructure = rxsStructure_modules[[currentModule]],
        rxsSpecification = res$rxsSpecification,
        yamlMetadata = yamlMetadata,
        indent = indent,
        indentSpaces = indentSpaces,
        fullWidth = fullWidth,
        module = currentModule,
        commentCharacter = commentCharacter,
        fillerCharacter = fillerCharacter,
        eC = eC,
        repeatingSuffix = repeatingSuffix,
        silent=silent
      );      
      
    }
    
    if (!silent) {
      msg("Parsed extraction script specifications into ", length(modules),
          " extraction script templates.\n",
          silent=silent);
    }

  } else {
    
    ### No modules - just write one template

    if (!silent) {
      if (ignoreModules) {
        msg("You set `ignoreModules` to FALSE - starting to parse the extraction script ",
            "specifications into a single extraction script template.\n",
            silent=silent);
      } else {
        msg("No modules specified - starting to parse the extraction script ",
            "specifications into a single extraction script template.\n",
            silent=silent);
      }
    }
    
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
      rxsSpecification = res$rxsSpecification,
      yamlMetadata = yamlMetadata,
      indent = indent,
      indentSpaces = indentSpaces,
      fullWidth = fullWidth,
      module = NULL,
      commentCharacter = commentCharacter,
      fillerCharacter = fillerCharacter,
      eC = eC,
      repeatingSuffix = repeatingSuffix,
      silent=silent
    );
  
    if (!silent) {
      msg("Parsed extraction script specifications into extraction script template.\n",
          silent=silent);
    }
    
  }
  
  ###---------------------------------------------------------------------------
  ### rxsTree Diagram
  ###---------------------------------------------------------------------------
  
  if (workingModularly) {
    
    rxsTreeDiagram_simple_modules <- list();
    
    for (currentModule in modules) {

      rxsTreeDiagram_simple_modules[[currentModule]] <-
        rxsTreeDiagram(
          extractionScriptTree =
            rxsStructure_modules[[currentModule]]$parsedEntities$extractionScriptTree,
          graphTheme = graphTheme
        );

    }
    
  } else {
    
    rxsTreeDiagram_simple <-
      rxsTreeDiagram(
        extractionScriptTree = rxsStructure$parsedEntities$extractionScriptTree,
        graphTheme = graphTheme
      );
   
  }
  
  if (!silent) {
    cat("Created diagram representing the extraction tree.\n");
  }

  ###---------------------------------------------------------------------------
  ### Entity overview: list
  ###---------------------------------------------------------------------------
  
  if (workingModularly) {
    
    entityOverview_list_modules <- list();
    
    for (currentModule in modules) {
      
      entityOverview_list_modules[[currentModule]] <-
        rxsTree_to_entityOverview_list(
          rxsStructure_modules[[currentModule]]$parsedEntities$extractionScriptTree,
          valueTemplates = rxsStructure_modules[[currentModule]]$parsedValueTemplates,
          headingLevel = instructionHeadingLevel
        );
      
    }
    
  } else {
  
    entityOverview_list <-
      rxsTree_to_entityOverview_list(
        rxsStructure$parsedEntities$extractionScriptTree,
        valueTemplates = rxsStructure$parsedValueTemplates,
        headingLevel = instructionHeadingLevel
      );
  
  }

  ###---------------------------------------------------------------------------
  ### Entity overview: compact
  ###---------------------------------------------------------------------------
  
  if (workingModularly) {
    
    entityOverview_compact_modules <- list();
    
    for (currentModule in modules) {
      
      entityOverview_compact_modules[[currentModule]] <-
        rxsTree_to_entityOverview_compact(
          extractionScriptTree =
            rxsStructure_modules[[currentModule]]$parsedEntities$extractionScriptTree,
          instructionHeadingLevel = instructionHeadingLevel,
          extractionOverview_compact_intro = extractionOverview_compact_intro
        );
      
    }
    
  } else {
  
    entityOverview_compact <-
      rxsTree_to_entityOverview_compact(
        extractionScriptTree = rxsStructure$parsedEntities$extractionScriptTree,
        instructionHeadingLevel = instructionHeadingLevel,
        extractionOverview_compact_intro = extractionOverview_compact_intro
      );
    
  }

  ###---------------------------------------------------------------------------
  ### Finish result object
  ###---------------------------------------------------------------------------

  if (workingModularly) {
    
    if (returnFullObject) {
      
      res <- c(res,
               list(rxsStructures = rxsStructure_modules,
                    rxsTreeDiagrams_simple = rxsTreeDiagram_simple_modules,
                    rxsTemplates = rxsTemplate_modules,
                    entityOverviews_list = entityOverview_list_modules,
                    entityOverviews_compact = entityOverview_compact_modules));
      
    } else {
      res <- rxsTemplate_modules;
    }
    
  } else {

    if (returnFullObject) {
      
      res <- c(res,
               list(rxsStructure = rxsStructure,
                    rxsTreeDiagram_simple = rxsTreeDiagram_simple,
                    rxsTemplate = rxsTemplate,
                    entityOverview_list = entityOverview_list,
                    entityOverview_compact = entityOverview_compact));
      
      class(res) <- "rxsStructure";
      
    } else {
      res <- rxsTemplate;
    }
    
  }
  
  ###---------------------------------------------------------------------------
  ### Potential write template and then return result
  ###---------------------------------------------------------------------------
  
  if (workingModularly) {

    if (!is.null(outputPath) && !is.null(outputFilenamePattern)) {
      
      for (currentModule in modules) {
        
        fileToWriteTo <- sprintf(outputFilenamePattern, currentModule);
        
        if (!grepl("\\.rxs.Rmd$", fileToWriteTo, ignore.case = TRUE)) {
          fileToWriteTo <- paste0(fileToWriteTo, ".rxs.Rmd");
        }
        
        fileToWriteTo <- file.path(outputPath, fileToWriteTo);
        
        if (file.exists(fileToWriteTo) && preventOverwriting) {
          warning("The file to write to, '", fileToWriteTo, "', exists, and ",
                  "you set `preventOverwriting` to `TRUE`, so I am not ",
                  "writing the extraction script template to disk.");
        } else {
          writeLines(paste0(unlist(rxsTemplate_modules[[currentModule]]), collapse="\n"),
                     fileToWriteTo);
          if (!silent) {
            cat0("Successfully wrote extraction script template to '",
                 fileToWriteTo, "'.\n");
          }
        }

      }
      
      return(invisible(res));
      
    } else {
      return(res);
    }
    
  } else {
  
    if (!is.null(outputFile)) {
      if (isTRUE(outputFile)) {
        ### Write to current working directory
        fileToWriteTo <- file.path(getwd(), "template.rxs.Rmd");
      } else if (is.character(outputFile)) {
        ### Path is specified in 'outputFile'
        fileToWriteTo <- outputFile;
      }
      
      if (!grepl("\\.rxs.Rmd$", fileToWriteTo, ignore.case = TRUE)) {
        fileToWriteTo <- paste0(fileToWriteTo, ".rxs.Rmd");
      }
      
      if (file.exists(fileToWriteTo) && preventOverwriting) {
        warning("The file to write to, '", fileToWriteTo, "', exists, and ",
                "you set `preventOverwriting` to `TRUE`, so I am not ",
                "writing the extraction script template to disk, instead ",
                "just returning it invisibly.");
      } else {
        writeLines(paste0(unlist(rxsTemplate), collapse="\n"),
                   fileToWriteTo);
        if (!silent) {
          cat0("Successfully wrote extraction script template to '",
               fileToWriteTo, "'.\n");
        }
      }
      return(invisible(res));
    } else {
      return(res);
    }

  }

}
