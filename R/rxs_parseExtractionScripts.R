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
#' @param parallel Whether to use parallel processing.
#'
#' @return A list of parsed extraction scripts.
#' @export
rxs_parseExtractionScripts <- function(path,
                                       pattern = "\\.[Rr]xs\\.[Rr]md$|\\.Rxs$",
                                       exclude = c("extractionScriptTemplate.[Rr]xs.[Rr]md",
                                                   "\\[EXCLUDED]"),
                                       ignore.case = TRUE,
                                       recursive = TRUE,
                                       silent = metabefor::opts$get("silent"),
                                       progressBar = TRUE,
                                       showErrors = TRUE,
                                       encoding = "UTF-8",
                                       parallel = FALSE) {

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
            ". This does not need to be a problem if you expect this, for ",
            "example if you want to merge the Rxs trees specified in these ",
            "files. However even in that scenario, it would be wise to ",
            "give them different names.");
    allScripts <- unique(allScripts);
  }

  res$input$allScripts <- allScripts;
  
  res$log <- c(
    res$log,
    msg("\nStarting to process ", length(allScripts),
        " Rxs (extraction script) files ",
        "in path ", path, " matching regular expression ", pattern,
        " but excluding all files matching regular expression ",
        exclude, ".", silent = silent)
  );
  
  if (parallel) {
    
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("If you want to use parallel processing, ",
           "you need to have the parallel package, which ",
           "*should* normally be part of base R.");
    }
    
    ### Then for all other languages in parallel; detect number of cores
    ### and create a cluster
    nCores <- parallel::detectCores();
    
    ### Because the trick below doesn't seem to work
    maxCores <- metabefor::opts$get("maxCores");
    if (!is.null(maxCores) && is.numeric(maxCores)) {
      nCores <- min(maxCores, nCores);
    }
    
    ### From https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      nCores <- min(2L, nCores);
    }
    
    res$log <- c(
      res$log,
      msg("\nI will use ", nCores,
          " processor cores.", silent = silent)
    );
    
  } else {
    
    nCores <- 1;
    
    res$log <- c(
      res$log,
      msg("\nI will not use multiple processor cores.",
          silent = silent)
    );
    
  }

  if (nCores == 1) {
    
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
    };
    
    parsedRxsFiles <-
      lapply(
        allScripts,
        rxs_parseSingleExtractionScript,
        path = path,
        silent = silent,
        progress = p,
        showErrors = showErrors,
        encoding = encoding
      );
    
  } else {
    
    ### Multi core approach
    
    cl <- parallel::makeCluster(nCores);
    
    ### Load the limonaid package in each cluster
    parallel::clusterEvalQ(
      cl,
      library(metabefor)
    );
    
    parallel_options <-
      list(
        rxsRootName = metabefor::opts$get("rxsRootName"),
        rxsObjectName = metabefor::opts$get("rxsObjectName"),
        uniqueSourceIdName = metabefor::opts$get("uniqueSourceIdName")
      );

    ### Export these objects and the 'silent' setting
    parallel::clusterExport(
      cl,
      c('path',
        'silent',
        'parallel_options',
        'showErrors',
        'encoding'),
      envir = environment()
    );
    
    ### Perform the parallel computations
    parsedRxsFiles <-
      parallel::parLapply(
        cl,
        allScripts,
        metabefor::rxs_parseSingleExtractionScript,
        path = path,
        silent = silent,
        parallel_options = parallel_options,
        progress = NULL,
        showErrors = showErrors,
        encoding = encoding
      );
    
    ### Stop the cluster
    parallel::stopCluster(cl);

  }

  ### Make sure we have unique names to store the objects in

  cumulativeDuplicateCounts <-
    cumulativeDuplicateCount(allScripts) - 1;
  
  filenameSuffixes <-
    ifelse(cumulativeDuplicateCounts == 0,
           "",
           paste0("__", cumulativeDuplicateCounts));
  
  parsedRxsFiles_names <-
    paste0(allScripts, filenameSuffixes);
  
  ### Get information from the parsedRxsFiles object

  res$log <-
    c(res$log,
      unlist(lapply(parsedRxsFiles,
                    function(x) {
                      return(x$log);
                    })));
  
  res$parsingProducts <-
    lapply(parsedRxsFiles,
           function(x) {
             return(list(yamlResult = x$yamlResult,
                         yamlParams = x$yamlParams,
                         rxsPurlingOutput = x$rxsPurlingOutput,
                         parsedRxsFiles = x$parsedRxsFiles));
           });

  res$rxsTrees_raw <-
    lapply(parsedRxsFiles,
           function(x) {
             return(x$rxsTrees_raw);
           });
  
  names(res$parsingProducts) <- parsedRxsFiles_names;
  names(res$rxsTrees_raw) <- parsedRxsFiles_names;
  
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
             "from rxs files ", vecTxtQ(invalidRawTreeNames),
             ". You will probably want to check and correct those, and then ",
             "re-run this command, before continuing.\n\n",
             "",
             
             "Alternatively, some or more files may use the old rxs file ",
             "format. If that is the case, you may want to change the root ",
             "and object names used when parsing the files using:\n\n",
             "    metabefor::opts$set(rxsRootName = 'study');\n",
             "    metabefor::opts$set(rxsObjectName = 'study');\n");
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
      names(res$rxsTrees_raw[validRawTrees]),
      function(treeName) {
        node <- res$rxsTrees_raw[validRawTrees][[treeName]];
        if (is.null(node$root$rxsMetadata)) {
          stop("No Rxs metadata found in raw Rxs tree '", treeName, "'!");
        } else {
          if ("id" %in% names(node$root$rxsMetadata)) {
            return(node$root$rxsMetadata$id);
          } else {
            stop("In the Rxs metadata found in raw Rxs tree '", treeName,
                 "', no source identifier was stored!");
          }
        }
      }
    );

  treeFilenames <-
    lapply(
      names(res$rxsTrees_raw[validRawTrees]),
      function(treeName) {
        node <- res$rxsTrees_raw[validRawTrees][[treeName]];
        if ("filename" %in% names(node$root$rxsMetadata)) {
          return(node$root$rxsMetadata$filename);
        } else {
          stop("In the Rxs metadata found in raw Rxs tree '", treeName,
               "', no original Rxs filename was stored!");
        }
      }
    );
  
  filenames_by_sourceId <- stats::setNames(treeFilenames,
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
            
            for (i in utils::tail(indices, -1)) {
              
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
        "Starting superficial verification (i.e. without validation) of Rxs trees.\n",
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
        msg("\nIn the Rxs tree for source identifier '", currentTree,
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
    msg("\nFinished superficially verifying all Rxs trees.\n",
        silent = silent)
  );

  class(res) <- "rxs_parsedExtractionScripts";

  return(res);

}
