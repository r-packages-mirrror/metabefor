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
  res$rxsTrees <- list();
  res$log <- c();

  if (anyDuplicated(allScripts)) {
    warning("Warning: two rxs files with the same name found: ",
            vecTxtQ(allScripts[duplicated(allScripts)]),
            ".");
    allScripts <- unique(allScripts);
  }

  res$input$allScripts <- allScripts;

  if (interactive() && (progressBar)) {
    p <- progress::progress_bar(total = length(allScripts));
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
            file.path(path,
                      filename),
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

    ### If successful, store the result and delete object; otherwise set to NA
    if (exists('study', envir=globalenv())) {
      
      study <- get('study', envir=globalenv());
      
      res$rxsTrees[[filename]] <-
        data.tree::Clone();
      
      res$log <- c(
        res$log,
        msg("\n  - Finally, successfully stored the `study` object, which ",
            "itself contains ", length(study), " objects.",
            silent = silent)
      );

      allValues <- study$Get('value');

      if (any(unlist(lapply(allValues, is.expression)))) {
        res$log <- c(
          res$log,
          msg("One or more extracted values are not just values, but ",
              "instead R expressions! This is probably a symptom of ",
              "a syntax error made during extraction. Carefully check the ",
              "extraction script file!",
              silent = silent)
        );
      } else {
        res$log <- c(
          res$log,
          msg("\n  - Checked (and discovered) that none of the extracted ",
              "`values` is in fact an R expression.",
              silent = silent)
        );
      }
      
      rm(study, envir=globalenv());
      
    } else {
      res$rxsTrees[[filename]] <- NA;
    }

    if (interactive() && (silent)) {
      #p$tick()$print();
      p$tick();
    };

  }
  
  res$log <- c(
    res$log,
    msg("\n\nFinished processing all Rxs files. ",
        "Starting verification of Rxs study trees.\n",
        silent = silent
    )
  );
  
  validTrees <- unlist(lapply(res$rxsTrees, inherits, "Node"));
  validTreeNames <- names(res$rxsTrees[validTrees]);
  invalidTreeNames <- names(res$rxsTrees[!validTrees]);
  
  if (sum(!validTrees) > 0) {
    warning("I read ", sum(!validTrees), " invalid study trees, specifically ",
            "from files ", vecTxtQ(invalidTreeNames), ". You will probably ",
            "want to check and correct those, and the re-run this command, ",
            "before continuing.");
  }
  
  res$convenience <- list();
  res$convenience$validTrees <- validTrees;
  res$convenience$validTreeNames <- validTreeNames;
  res$convenience$invalidTreeNames <- invalidTreeNames;
  
  ### Remove invalid trees
  res$rxsTrees <- res$rxsTrees[validTreeNames];
  
  for (currentTree in validTreeNames) {
    if (!data.tree::AreNamesUnique(res$rxsTrees[[currentTree]])) {
      res$log <- c(
        res$log,
        msg("\nIn the Rxs study tree from file '", currentTree,
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
