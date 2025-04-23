#' Read and parse one single extraction script
#'
#' @param filename The filename with the extraction script.
#' @param path The path to the directory with that extraction script.
#' @param progress Optionally, a progress bar to tick after the file
#' has been parsed  (must be a [progress::progress_bar()] object.
#' @param silent Whether to be quiet or chatty.
#' @param parallel_options Used to pass options for parallel processing.
#' @param showErrors Whether to show or hide errors that are encountered.
#' @param encoding The files' encoding.
#'
#' @return A list of parsed extraction scripts.
#' @export
rxs_parseSingleExtractionScript <- function(filename,
                                            path,
                                            progress = NULL,
                                            silent = metabefor::opts$get("silent"),
                                            parallel_options = NULL,
                                            showErrors = TRUE,
                                            encoding = "UTF-8") {

  res <- list();

  res$log <-
    msg("\n\nStarting to process extraction script ", filename, "...",
        silent = silent);
  
  ###-------------------------------------------------------------------------
  ### Get YAML parameters
  ###-------------------------------------------------------------------------
  
  ### Extract R chunks and write them to another file
  res$yamlResult <-
    utils::capture.output(
      tryCatch(
        yum::load_and_simplify(file = file.path(path, filename)),
        error = function(e) {
          cat(paste0("In file '",
                     filename,
                     "', encountered error while parsing extracted YAML: \n",
                     e$message,
                     "\n\n",
                     collapse="\n"));
          invisible(e);
        })
    );
  
  if (is.list(res$yamlResult)) {
    yamlParams <- res$yamlResult;
    res$yamlParams <- yamlParams;
  } else {
    yamlParams <- list();
    res$yamlParams <- NA;
    res$log <- msg(res$yamlResult, silent = silent);
  }

  if (("params" %in% names(yamlParams)) &&
      ("rxsVersion" %in% names(yamlParams$params))) {
    res$log <- c(
      res$log,
      msg("\n  - This is a valid Rxs file, version ",
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
        msg("\n  - The rxsRootName stored in this Rxs file is '",
            rxsRootName, "'.",
            silent = silent)
      );
    } else {
      rxsRootName <- metabefor::opts$get("rxsRootName");
      res$log <- c(
        res$log,
        msg("\n  - The rxsRootName was not stored in this Rxs file. Using the ",
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
        msg("\n  - The rxsObjectName stored in this Rxs file is '",
            rxsObjectName, "'.",
            silent = silent)
      );
    } else {
      rxsObjectName <- metabefor::opts$get("rxsObjectName");
      res$log <- c(
        res$log,
        msg("\n  - The rxsObjectName was not stored in this Rxs file. Using the ",
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
        msg("\n  - The uniqueSourceIdName stored in this Rxs file is '",
            uniqueSourceIdName, "'.",
            silent = silent)
      );
    } else {
      uniqueSourceIdName <- metabefor::opts$get("uniqueSourceIdName");
      res$log <- c(
        res$log,
        msg("\n  - The uniqueSourceIdName was not stored in this Rxs file. Using the ",
            "name stored in the options ('",
            uniqueSourceIdName, "'. That is, however, a relatively new name - if ",
            "you get any errors, you may want to set it using e.g.:\n\n",
            "    metabefor::opts$set(uniqueSourceIdName = 'uniqueSourceIdentifier');\n",
            silent = silent)
      );
    }
  } else {
    rxsVersion <- "0.0.1.9999";
    if (is.null(parallel_options)) {
      rxsRootName <- metabefor::opts$get("rxsRootName");
      rxsObjectName <- metabefor::opts$get("rxsObjectName");
      uniqueSourceIdName <- metabefor::opts$get("uniqueSourceIdName");
    } else {
      rxsRootName <- parallel_options[["rxsRootName"]];
      rxsObjectName <- parallel_options[["rxsObjectName"]];
      uniqueSourceIdName <- parallel_options[["uniqueSourceIdName"]];
    }
    res$log <- c(
      res$log,
      msg("\n  - This is either a very old rxs file, or not a valid rxs ",
          "file at all. I will use the settings from the options ",
          "(specifically, setting rxsVersion to '", rxsVersion,
          "', and based on the options, using rxsRootName='", rxsRootName,
          "', rxsObjectName='", rxsObjectName,
          "', uniqueSourceIdName='", uniqueSourceIdName, "'). If you ",
          "see any errors, you may want to change the root and object ",
          "names to something else, using e.g. the old default:\n\n",
          "    metabefor::opts$set(rxsRootName = 'study');\n",
          "    metabefor::opts$set(rxsObjectName = 'study');\n",
          silent = silent)
    );
  }
  
  ###-------------------------------------------------------------------------
  ### Extract R chunks
  ###-------------------------------------------------------------------------
  
  ### From https://stackoverflow.com/questions/24753969/knitr-run-all-chunks-in-an-rmarkdown-document

  ### Create temporary file
  tempR <- tempfile(fileext = ".R");

  ### Make sure it's deleted when we're done
  on.exit(unlink(tempR));

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

  tryCatch({res$rxsPurlingOutput <- purlingOutput;},
    error = function(e) {
      stop("Error saving purling output to Rxs object! The error is:\n\n",
           e$message,
           "\n\nEncountered while processing file '", filename, "'.\n");
    });
  
  ###-------------------------------------------------------------------------
  ### Evaluate R chunks
  ###-------------------------------------------------------------------------
  
  res$log <- c(
    res$log,
    msg("\n  - Extracted R script fragments and stored them in ",
        "a temporary file ('", res$rxsPurlingOutput, "').",
        silent = silent)
  );

  if (any(grepl(
    paste0(
      "In file '",
      filename,
      "', encountered error while purling"
    ),
    res$rxsPurlingOutput
  ))) {
    res$rxsOutput <-
      "Could not run this extraction script because of purling problems."
    if (showErrors) {
      cat(paste0(res$rxsPurlingOutput, collapse="\n"));
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
    
    ### Create a fresh environment and store our activity as parsing
    ### multiple extraction files, preventing the calling to knitr.
    parsingEnv <- new.env();
    
    base::assign('parsingMultipleRxsFiles', TRUE, envir=parsingEnv);

    #base::assign('parsingMultipleRxsFiles', TRUE, envir=globalenv());
    
    ### Run the other file with error handling
    rxsOutput <-
      # capture.output(tryCatch(sys.source(tempR, envir=globalenv()),
      utils::capture.output(
        tryCatch(
          base::source(
          #base::sys.source(
            tempR,
            #envir = parsingEnv,
            local = parsingEnv, #globalenv()
            encoding = "UTF-8"
          ),
          error = function(e) {
            cat(
              paste0(
                "In file '",
                filename,
                "', encountered error while running Rxs: \n",
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
          " lines of output generated and stored (",
          sum(nchar(rxsOutput)), " characters).",
          silent = silent)
    );

    tryCatch({
      res$rxsOutput <- rxsOutput;
      },
      error = function(e) {
        stop("Error saving rxs evaluation output to Rxs object! The error is:\n\n",
             e$message,
             "\n\nEncountered while processing file '", filename, "'.\n");
      });

    if (showErrors) {
      if (any(grepl(paste0("In file '",
                           filename,
                           "', encountered error while running Rxs"),
                           res$rxsOutput))) {
        cat(paste0(res$rxsOutput, collapse="\n"));
      }
    }
  }

  ###-------------------------------------------------------------------------
  ### Get the result and store it in our results object
  ###-------------------------------------------------------------------------

  ### If successful, store the result and delete object; otherwise set to NA
  if (exists('parsingEnv') && ## If there was a purling problem, it doesn't
      exists(rxsObjectName, envir=parsingEnv)) { #envir=globalenv())) {

    tmpRxsObject <- get(rxsObjectName, envir=parsingEnv); # envir=globalenv());

    res$rxsTrees_raw <-
      data.tree::Clone(tmpRxsObject);
    
    class(res$rxsTrees_raw) <-
      union(c("rxs", "rxsObject"),
            class(res$rxsTrees_raw));
    
    if (is.null(res$rxsTrees_raw$rxsMetadata)) {
      res$rxsTrees_raw$rxsMetadata <-
        list(#id_from_parsing = sourceId,
             id = sanitize_filename_to_identifier(filename),   
             filename = filename);
    } else {
      # res$rxsTrees_raw$rxsMetadata$id_from_parsing <-
      #   sourceId;
      res$rxsTrees_raw$rxsMetadata$filename <-
        filename
      if (!("id" %in% names(res$rxsTrees_raw$rxsMetadata))) {
        res$rxsTrees_raw$rxsMetadata$id <-
          sanitize_filename_to_identifier(filename);
      }
    }
    sourceId <- res$rxsTrees_raw$rxsMetadata$id;
    
    nrOfEntities <-
      length(res$rxsTrees_raw$Get("name"))
    
    res$log <- c(
      res$log,
      msg("\n  - Finally, successfully stored the object with extracted data, which ",
          "itself contains ", nrOfEntities, " entities ",
          "(some of which may be clustering entities (i.e. lists of other ",
          "entities), in which case even more than ", nrOfEntities,
          " entities may have been extracted from this source).",
          silent = silent)
    );

    allValues <-
      res$rxsTrees_raw$Get('value');
    
    valuesThatAreExpressions <-
      unlist(lapply(allValues, is.expression));

    if (any(valuesThatAreExpressions)) {
      res$log <- c(
        res$log,
        msg("  - In the extracted entities in filename ", filename,
            " for source with identifier '", sourceId, "', one or more ",
            "extracted values are not just values, but ",
            "instead R expressions! This is probably a symptom of ",
            "a syntax error made during extraction (e.g. omitted quotes). ",
            "Carefully check the extraction script file, specifically ",
            "the entities with the following identifiers: ",
            vecTxtQ(names(allValues)[valuesThatAreExpressions]),
            ".",
            silent = silent)
      );
    } else {
      res$log <- c(
        res$log,
        msg("\n  - Checked (and discovered) that none of the ",
            length(allValues), " extracted ",
            "`values` is in fact an R expression, which means that no ",
            "R syntax errors were probably made (e.g. omitted quotes).",
            silent = silent)
      );
    }
    
    #  ### No longer necessary, now we do everything in 'parsingEnv'
    #rm(list = rxsObjectName, envir=globalenv());
    #rm(list = 'parsingMultipleRxsFiles', envir=globalenv());
    
    res$log <- c(
      res$log,
      msg("\n  - Done with source with identifier ", sourceId,
          " from file ", filename, ".\n",
          silent = silent)
    );
    
  } else {
    
    res$rxsTrees_raw <- NA;
    
    res$log <- c(
      res$log,
      msg("\n  - Parsed file ", filename, ", but no object named '",
          rxsObjectName,
          "' was produced, and so no Rxs tree was obtained.\n",
          silent = silent)
    );
    
  }
  
  if (!is.null(progress)) {
    if (inherits(progress, "progress_bar")) {
      progress$tick();
    }
  }
  
  class(res) <- c("rxs", "rxs_singleParsedExtractionScript");

  return(res);

}
