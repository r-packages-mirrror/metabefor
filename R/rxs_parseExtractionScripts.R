#' Read and parse the extraction scripts in a directory
#'
#' @param path The path containing the extraction scripts.
#' @param pattern The regular expression specifying which files to read.
#' @param exclude A regular expression specifying which files to ignore.
#' @param ignore.case Whether the regular expression is case sensitive.
#' @param recursive Whether to also look in subdirectories.
#' @param silent Whether to be quiet or chatty.
#' @param progressBar Whether to show the progress bar.
#' @param showErrors Whether to show or hide errors that are encountered.
#' @param encoding The files' encoding.
#'
#' @return A list of parsed extraction scripts.
#' @export
rxs_parseExtractionScripts <- function(path,
                                       pattern="\\.rxs\\.Rmd$|\\.rxs$",
                                       exclude=c("extractionScriptTemplate.rxs.Rmd",
                                                 "\\[EXCLUDED]"),
                                       ignore.case=TRUE,
                                       recursive=TRUE,
                                       silent=metabefor::opts$get("silent"),
                                       progressBar = TRUE,
                                       showErrors=TRUE,
                                       encoding="UTF-8") {

  res <- list(input = as.list(environment()));

  allScripts <- list.files(path,
                           pattern=pattern,
                           ignore.case=ignore.case,
                           recursive=recursive);

  for (exclusionPattern in exclude) {
    allScripts <- grep(exclusionPattern,
                       allScripts,
                       value=TRUE,
                       invert=TRUE);
  }

  res$rxsOutput <- list();
  res$rxsTrees_raw <- list();
  res$rxsTrees <- list();
  res$log <- c();

  if (anyDuplicated(allScripts)) {
    warning("Warning: two rxs files with the same name found: ",
            vecTxtQ(allScripts[duplicated(allScripts)]),
            ".");
    allScripts <- unique(allScripts);
  }

  res$input$allScripts <- allScripts;
  
  if (progressBar) {
    if (!interactive()) {
      progressBar <- FALSE;
    }
    if (!requireNamespace("progress", quietly = TRUE)) {
      progressBar <- FALSE;
    }
  }

  if (progressBar) {
    p <- progress::progress_bar$new(
      total = length(allScripts),
      format = ":spin [:bar] :percent in :elapsedfull, :eta to go");
    #p <- dplyr::progress_estimated(length(allScripts));
  };

  res$log <- c(
    res$log,
    msg("\nStarting to process ", length(allScripts),
        " Rxs (extraction script) files ",
        "in path ", path, " matching regular expression ", pattern,
        " but excluding all files matching regular expression ",
        exclude, ".", silent = silent)
  );

  for (filename in allScripts) {
    
    res$log <- c(
      res$log,
      msg("\n\nStarting to process extraction script ", filename, "...",
          silent = silent)
    );
    
    ###-------------------------------------------------------------------------
    ### Get YAML parameters
    ###-------------------------------------------------------------------------
    
    yamlParams <-
      yum::load_and_simplify(file = file.path(path, filename));
    
    if (("params" %in% names(yamlParams)) &&
        ("rxsVersion" %in% names(yamlParams$params))) {
      res$log <- c(
        res$log,
        msg("\n  - This is a valid rxs file, version ",
            yamlParams$params$rxsVersion, ".",
            silent = silent)
      );
      ###-----------------------------------------------------------------------
      ### rxsRootName
      ###-----------------------------------------------------------------------
      if ("rxsRootName" %in% names(yamlParams$params)) {
        rxsRootName <- yamlParams$params$rxsRootName;
        res$log <- c(
          res$log,
          msg("\n  - The rxsRootName stored in this rxs file is '",
              rxsRootName, "'.",
              silent = silent)
        );
      } else {
        rxsRootName <- metabefor::opts$get("rxsRootName");
        res$log <- c(
          res$log,
          msg("\n  - The rxsRootName was not stored in this rxs file. Using the ",
              "name stored in the options ('",
              rxsRootName, "'. That is, however, a relatively new name - if ",
              "you get any errors, you may want to set it to e.g. 'study' ",
              "using:\n\n    metabefor::opts$set(rxsRootName = 'study');\n",
              silent = silent)
        );
      }
      ###-----------------------------------------------------------------------
      ### rxsObjectName
      ###-----------------------------------------------------------------------
      if ("rxsObjectName" %in% names(yamlParams$params)) {
        rxsObjectName <- yamlParams$params$rxsObjectName;
        res$log <- c(
          res$log,
          msg("\n  - The rxsObjectName stored in this rxs file is '",
              rxsObjectName, "'.",
              silent = silent)
        );
      } else {
        rxsObjectName <- metabefor::opts$get("rxsObjectName");
        res$log <- c(
          res$log,
          msg("\n  - The rxsObjectName was not stored in this rxs file. Using the ",
              "name stored in the options ('",
              rxsObjectName, "'. That is, however, a relatively new name - if ",
              "you get any errors, you may want to set it to e.g. 'study' ",
              "using:\n\n    metabefor::opts$set(rxsObjectName = 'study');\n",
              silent = silent)
        );
      }
      ###-----------------------------------------------------------------------
      ### uniqueSourceIdName
      ###-----------------------------------------------------------------------
      if ("uniqueSourceIdName" %in% names(yamlParams$params)) {
        uniqueSourceIdName <- yamlParams$params$uniqueSourceIdName;
        res$log <- c(
          res$log,
          msg("\n  - The uniqueSourceIdName stored in this rxs file is '",
              uniqueSourceIdName, "'.",
              silent = silent)
        );
      } else {
        uniqueSourceIdName <- metabefor::opts$get("uniqueSourceIdName");
        res$log <- c(
          res$log,
          msg("\n  - The uniqueSourceIdName was not stored in this rxs file. Using the ",
              "name stored in the options ('",
              uniqueSourceIdName, "'. That is, however, a relatively new name - if ",
              "you get any errors, you may want to set it using e.g.:\n\n",
              "    metabefor::opts$set(uniqueSourceIdName = 'uniqueSourceIdentifier');\n",
              silent = silent)
        );
      }
    } else {
      res$log <- c(
        res$log,
        msg("\n  - This is either a very old rxs file, or not a valid rxs ",
            "file at all. I will use the settings from the options. If you ",
            "see any errors, you may want to change the root and object ",
            "names to an old value, using e.g. :\n\n",
            "    metabefor::opts$set(rxsRootName = 'study');\n",
            "    metabefor::opts$set(rxsObjectName = 'study');\n",
            "    metabefor::opts$set(uniqueSourceIdName = 'uniqueSourceIdentifier');\n",
            silent = silent)
      );
      rxsVersion <- "0.0.1.9999";
      rxsRootName <- metabefor::opts$get("rxsRootName");
      rxsRootName <- metabefor::opts$get("rxsObjectName");
      uniqueSourceIdName <- metabefor::opts$get("uniqueSourceIdName");
    }

    ###-------------------------------------------------------------------------
    ### Extract R chunks
    ###-------------------------------------------------------------------------
    
    ### From https://stackoverflow.com/questions/24753969/knitr-run-all-chunks-in-an-rmarkdown-document

    ### Create temporary file
    tempR <- tempfile(fileext = ".R");

    ### Make sure it's deleted when we're done
    on.exit(unlink(tempR));

    if (filename %in% names (res$rxsPurlingOutput)) {
      warning("RXS purling output was already stored for file '",
              filename,
              "'. Storing existing version as 'BACKUP-",
              filename,
              "'.");
      res$rxsPurlingOutput[[paste0("BACKUP-",filename)]] <-
        res$rxsPurlingOutput[[filename]];
      res$rxsPurlingOutput[[filename]] <- NULL;
    }

    ### Extract R chunks and write them to another file
    purlingOutput <-
      utils::capture.output(
        tryCatch(
          knitr::purl(
            file.path(path, filename),
            output=tempR,
            quiet=TRUE,
            encoding=encoding),
          error = function(e) {
            cat(paste0("In file '",
                       filename,
                       "', encountered error while purling: \n",
                       e$message,
                       "\n\n",
                       collapse="\n"));
            invisible(e);
          })
      );

    ###-------------------------------------------------------------------------
    ### Evaluate R chunks
    ###-------------------------------------------------------------------------
    
    tryCatch({res$rxsPurlingOutput[[filename]] <-
                purlingOutput;},
             error = function(e) {
               stop("Error saving purling output to rxs object! The error is:\n\n",
                    e$message,
                    "\n\nEncountered while processing file '", filename, "'.\n");
             });
    
    res$log <- c(
      res$log,
      msg("\n  - Extracted R script fragments: ",
          length(res$rxsPurlingOutput[[filename]]),
          " lines extracted.",
          silent = silent)
    );

    if (any(grepl("In file '",
                    filename,
                    "', encountered error while purling",
                    res$rxsPurlingOutput[[filename]]))) {
      res$rxsOutput[[filename]] <-
        "Could not run this extraction script because of purling problems."
      if (showErrors) {
        cat(paste0(res$rxsPurlingOutput[[filename]], collapse="\n"));
      }
    } else {
      # oldEncoding <- getOption("encoding");
      # on.exit(options(encoding = oldEncoding));
      # options(encoding = "UTF-8");
      # ###
      # ### If the above doesn't work, use source instead of sys.source
      # ### so that it's possible to pass an encoding.
      # ###
      
      ###-----------------------------------------------------------------------
      ### Can't see how to resolve this; any attempt to force UTF-8 encoding
      ### results in 
      ###-----------------------------------------------------------------------
      
      ### Run the other file with error handling
      rxsOutput <-
        # capture.output(tryCatch(sys.source(tempR, envir=globalenv()),
        utils::capture.output(
          tryCatch(
            source(
              tempR,
              local=globalenv(),
              encoding = "UTF-8"
            ),
            error = function(e) {
              cat(
                paste0(
                  "In file '",
                  filename,
                  "', encountered error while running rxs: \n",
                  e$message,
                  "\n\n",
                  collapse="\n"
                )
              );
        # cat(e$message);
            invisible(e);
          }
        )
      );
      
      res$log <- c(
        res$log,
        msg("\n  - Also executed R script fragments: ", length(rxsOutput),
            " lines of output generated and stored.",
            silent = silent)
      );

      tryCatch({
        res$rxsOutput[[filename]] <- rxsOutput;
        },
        error = function(e) {
          stop("Error saving rxs evaluation output to rxs object! The error is:\n\n",
               e$message,
               "\n\nEncountered while processing file '", filename, "'.\n");
        });

      if (showErrors) {
        if (any(grepl("In file '",
                      filename,
                      "', encountered error while running rxs",
                      res$rxsOutput[[filename]]))) {
          cat(paste0(res$rxsOutput[[filename]], collapse="\n"));
        }
      }
    }

    # ###-------------------------------------------------------------------------
    # ### Get the identifier of this source
    # ###-------------------------------------------------------------------------
    # 
    # ### NOTE: No longer using this, only using the version in the metadata!
    # 
    # if (exists(uniqueSourceIdName, envir=globalenv())) {
    #   sourceId <- get(uniqueSourceIdName, envir=globalenv());
    # 
    #   res$log <- c(
    #     res$log,
    #     msg("\n  - The unique source identifier was succesfully retrieved: ",
    #         sourceId, ".",
    #         silent = silent)
    #   );
    #   
    # } else {
    #   
    #   sourceId <- filename;
    #   
    #   res$log <- c(
    #     res$log,
    #     msg("\n  - The unique source identifier could not be retrieved, using filename instead: ",
    #         sourceId, ".",
    #         silent = silent)
    #   );
    #   
    # }
    
    ###-------------------------------------------------------------------------
    ### Make sure we have a unique name to store this object in
    ###-------------------------------------------------------------------------
    
    currentTreeName <- filename;
    
    if (currentTreeName %in% names(res$rxsTrees_raw)) {
      currentTreeName <- paste0(currentTreeName, "__1");
    }
    
    while (currentTreeName %in% names(res$rxsTrees_raw)) {
      currentNumber <- gsub(".*__([0-9]+)$", "\\1", currentTreeName);
      currentTreeName <- paste0(currentTreeName, "__", currentNumber+1);
    }
    
    ###-------------------------------------------------------------------------
    ### Get the result and store it in our results object
    ###-------------------------------------------------------------------------
    
    ### If successful, store the result and delete object; otherwise set to NA
    if (exists(rxsObjectName, envir=globalenv())) {
      
      tmpRxsObject <- get(rxsObjectName, envir=globalenv());

      res$rxsTrees_raw[[currentTreeName]] <-
        data.tree::Clone(tmpRxsObject);
      
      if (is.null(res$rxsTrees_raw[[currentTreeName]]$rxsMetadata)) {
        res$rxsTrees_raw[[currentTreeName]]$rxsMetadata <-
          list(#id_from_parsing = sourceId,
               id = sanitize_filename_to_identifier(filename),   
               filename = filename);
      } else {
        # res$rxsTrees_raw[[currentTreeName]]$rxsMetadata$id_from_parsing <-
        #   sourceId;
        res$rxsTrees_raw[[currentTreeName]]$rxsMetadata$filename <-
          filename
        if (!("id" %in% names(res$rxsTrees_raw[[currentTreeName]]$rxsMetadata))) {
          res$rxsTrees_raw[[currentTreeName]]$rxsMetadata$id <-
            sanitize_filename_to_identifier(filename);
        }
        # if ("id" %in% names(res$rxsTrees_raw[[currentTreeName]]$rxsMetadata)) {
        #   if (!(res$rxsTrees_raw[[currentTreeName]]$rxsMetadata$id == sourceId)) {
        #     res$log <- c(
        #       res$log,
        #       msg("\n  - Warning: the source identifier as specified at the ",
        #           "top of the rxs file ('", sourceId, "') is different from ",
        #           "the source identifier specified in the rxs object's ",
        #           "metadata ('",
        #           res$rxsTrees_raw[[currentTreeName]]$rxsMetadata$id,
        #           "'), even though these should be identical! If this ",
        #           "source file was manually altered (e.g. updated from a ",
        #           "version where source identifiers did not exist yet) this ",
        #           "is expected, so then there's no need to worry. Otherwise, ",
        #           "however, it may be a sign that somebody (e.g. an ",
        #           "extractor) accidently changed a part of the rxs file that ",
        #           "should not have changed, so you may want to check ",
        #           "it carefully",
        #           silent = silent)
        #     );
        #   }
        # }
      }
      sourceId <- res$rxsTrees_raw[[currentTreeName]]$rxsMetadata$id;
      
      nrOfEntities <-
        length(res$rxsTrees_raw[[currentTreeName]]$Get("name"))
      
      res$log <- c(
        res$log,
        msg("\n  - Finally, successfully stored the object with extracted data, which ",
            "itself contains ", nrOfEntities, " entities ",
            "(some of which may be clustering entities (i.e. lists of other ",
            "entities), in which case even more than ", nrOfEntities,
            " entities may have been extracted from this source).",
            silent = silent)
      );

      allValues <- res$rxsTrees_raw[[currentTreeName]]$Get('value');

      if (any(unlist(lapply(allValues, is.expression)))) {
        res$log <- c(
          res$log,
          msg("  - In the extracted entities in filename ", filename,
              " for source with identifier '", sourceId, "', one or more ",
              "extracted values are not just values, but ",
              "instead R expressions! This is probably a symptom of ",
              "a syntax error made during extraction (e.g. omitted quotes). ",
              "Carefully check the extraction script file!",
              silent = silent)
        );
      } else {
        res$log <- c(
          res$log,
          msg("\n  - Checked (and discovered) that none of the extracted ",
              "`values` is in fact an R expression, which means that no ",
              "R syntax errors were probably made (e.g. omitted quotes).",
              silent = silent)
        );
      }
      
      rm(list = rxsObjectName, envir=globalenv());
      
    } else {
      res$rxsTrees_raw[[currentTreeName]] <- NA;
    }
    
    res$log <- c(
      res$log,
      msg("\n  - Done with source with identifier ", sourceId,
          " from file ", filename, ".\n",
          silent = silent)
    );
    
    if (progressBar) {
      #p$tick()$print();
      p$tick();
    };

  }
  
  res$log <- c(
    res$log,
    msg("\n\nFinished processing all Rxs files. ",
        "Starting merging the Rxs trees.\n",
        silent = silent
    )
  );
  
  ###-------------------------------------------------------------------------
  ### Verification of raw trees
  ###-------------------------------------------------------------------------
  
  validRawTrees <- unlist(lapply(res$rxsTrees_raw, inherits, "Node"));
  validRawTreeNames <- names(res$rxsTrees_raw[validRawTrees]);
  invalidRawTreeNames <- names(res$rxsTrees_raw[!validRawTrees]);
  
  res$convenience <- list();
  res$convenience$validRawTrees <- validRawTrees;
  res$convenience$validRawTreeNames <- validRawTreeNames;
  res$convenience$invalidRawTreeNames <- invalidRawTreeNames;
  
  if (sum(!validRawTrees) > 0) {
    warningMessage <-
      paste0("When reading the raw rxs trees, I read ", sum(!validRawTrees),
             " invalid trees, specifically those ",
             "from rxs files ", vecTxtQ(invalidTreeNames),
             ". You will probably want to check and correct those, and then ",
             " re-run this command, before continuing.");
    res$log <- c(
      res$log,
      msg(warningMessage,
          silent = silent
      )
    );
    if (silent) {
      warning(warningMessage);
    }
  } else {
    res$log <- c(
      res$log,
      msg("\nWhen reading the raw rxs trees, I encountered no invalid trees.\n",
          silent = silent
      )
    );
  }
  
  ###-------------------------------------------------------------------------
  ### Merging trees
  ###-------------------------------------------------------------------------
  
  sourceIds <-
    lapply(
      res$rxsTrees_raw[validRawTrees],
      function(node) {
        if (is.null(node$root$rxsMetadata)) {
        } else {
          if ("id" %in% names(node$root$rxsMetadata)) {
            return(node$root$rxsMetadata$id);
          } else {
            return(node$root$rxsMetadata$id_from_parsing);
          }
        }
      }
    );
  
  filenames_by_sourceId <- stats::setNames(names(sourceIds),
                                           nm = sourceIds);
  
  uniqueSourceIds <- unique(sourceIds);
    
  duplicatedSourceIds <- duplicated(unlist(sourceIds));
  duplicatedSourceIds <- unique(unlist(sourceIds)[duplicatedSourceIds]);
  
  res$convenience$sourceIds <- sourceIds;
  res$convenience$filenames_by_sourceId <- filenames_by_sourceId;
  res$convenience$uniqueSourceIds <- uniqueSourceIds;
  res$convenience$duplicatedSourceIds <- duplicatedSourceIds;
  
  if (length(duplicatedSourceIds) == 0) {
    res$log <- c(
      res$log,
      msg("\nAll raw rxs trees had unique source identifiers. Therefore, I ",
          "will not merge any trees.\n",
          silent = silent
      )
    );
    res$rxsTrees <- res$rxsTrees_raw[validRawTrees];
  } else {
    res$log <- c(
      res$log,
      msg("\nSome of the raw rxs trees had the same source identifiers. ",
          "Therefore, I will now attempt to merge those trees.\n",
          silent = silent
      )
    );
    
    ###-------------------------------------------------------------------------
    ### Start of actual merging activity
    ###-------------------------------------------------------------------------

    res$rxsTrees <-
      lapply(
        uniqueSourceIds,
        function(sourceId) {
          
          res$log <- c(
            res$log,
            msg("\n  - Starting to merge the trees for source identifier ",
                sourceId, ".",
                silent = silent
            )
          );
          
          indices <- which(sourceIds == sourceId);

          mergingResult <-
            data.tree::Clone(res$rxsTrees_raw[validRawTrees][[indices[1]]]);

          res$log <- c(
            res$log,
            msg("\n    - Taking Rxs file '",
                mergingResult$rxsMetadata$filename, "' as basis.",
                silent = silent
            )
          );
          
          if (length(indices) > 1) {
            
            for (i in tail(indices, -1)) {
              
              res$log <- c(
                res$log,
                msg("\n    - Merging in entities from Rxs file '",
                    res$rxsTrees_raw[validRawTrees][[i]]$rxsMetadata$filename,
                    "'.",
                    silent = silent
                )
              );
              
              mergingResult <-
                mergeTrees(
                  tree1 = mergingResult,
                  tree2 = res$rxsTrees_raw[validRawTrees][[i]],
                  spaces = 6,
                  silent = silent
                );
              
            }
            
          } 
          
          return(mergingResult);
          
        }
      );
    
    names(res$rxsTrees) <- uniqueSourceIds;

  }
  
  res$log <- c(
    res$log,
    msg("\n\nFinished merging the Rxs trees. ",
        "Starting verification of Rxs trees.\n",
        silent = silent
    )
  );
  
  ###-------------------------------------------------------------------------
  ### Verification of raw trees
  ###-------------------------------------------------------------------------

  for (currentTree in names(res$rxsTrees)) {
    if (!data.tree::AreNamesUnique(res$rxsTrees[[currentTree]])) {
      res$log <- c(
        res$log,
        msg("\nIn the Rxs study tree for source identifier '", currentTree,
            "', not all node names (i.e. entity names) are unique!",
            silent = silent)
      );
      if (silent) {
        warning(msg);
      }
    }
  }

  res$log <- c(
    res$log,
    msg("\nFinished verifying all Rxs study trees.\n",
        silent = silent)
  );

  class(res) <- "rxs_parsedExtractionScripts";

  return(res);

}
