#' Validate an Rxs tree
#'
#' @param rxsTree The Rxs tree.
#' @param stopOnFailure Whether to throw an error when validation fails.
#' @param rxsStructure Optionally, the `rxsStructure` as resulting from
#' a call to [rxs_parseSpecifications()].
#'
#' @return Invisibly, the Rxs tree (which was altered in place, consistent
#' with the reference semantics employed by [data.tree::Node()] - so you can
#' just discard the result).
#' 
#' @export
rxs_validation <- function(rxsTree,
                           stopOnFailure = FALSE,
                           rxsStructure = NULL) {
  
  warning('bla');
  
  passedValidation <- function(...) {
    return(paste("V", paste0(..., sep="", collapse="")));
  }
  failedValidation <- function(...) {
    return(paste("X", paste0(..., sep="", collapse="")));
  }
  
  eC <- metabefor::opts$get("entityColNames");

  rxsTree$validationLog <-
    paste0("Validation run starting at ",
           format(Sys.time(),
                  "%Y-%m-%d at %H:%M:%S %Z (UTC%z)"));

  if (!data.tree::AreNamesUnique(rxsTree)) {
    rxsTree$validationLog <-
      c(rxsTree$validationLog,
        failedValidation(
          "Failed validation: not all entity identifiers are unique!"
        ));
    rxsTree$validationResults <-
      list(list(entityId = rxsTree$root$name,
                entityPath = rxsTree$root$name,
                validation = "All entity identifiers must be unique",
                validated = FALSE));
  } else {
    rxsTree$validationLog <-
      c(rxsTree$validationLog,
        passedValidation(
          "Passed validation: all entity identifiers are unique!"
        ));
    rxsTree$validationResults <-
      list(list(entityId = rxsTree$root$name,
                entityPath = rxsTree$root$name,
                validation = "All entity identifiers must be unique",
                validated = TRUE));
  }

  rxsTree$Do(function(node) {

    if (is.list(node$value)) {
      
      ###-----------------------------------------------------------------------
      ### Clustering entity: loop through the contained clustered entities
      ###-----------------------------------------------------------------------
      
      for (valueName in names(node$value)) {

        relevantNode <- NULL;
        
        if (!is.null(node$validation[[valueName]])) {
          
          VALUE <- node$value[[valueName]];
          errorMsg <- "";
          
          ### Execution of validation code
          validationOutcome <- eval(node$validation[[valueName]]);
          
          if (is.logical(validationOutcome) && validationOutcome) {
            
            validationMsg <-
              passedValidation(
                "Passed validation for clustered entity '",
                valueName, "' (in clustering entity '",
                node$name, "'!"
              );
            validationStatus <- TRUE;
            
          } else if (is.logical(validationOutcome) && !validationOutcome) {
            if (!is.null(rxsStructure)) {
              ### Find relevant node (entity) and get the valueTemplate
              ### Then get the error message to use.
              relevantNode <-
                data.tree::FindNode(
                  rxsStructure$parsedEntities$extractionScriptTree,
                  valueName
                );
              if (is.null(relevantNode)) {
                ### Either a node with this name does not exist,
                ### or it's a recursive node
                relevantNode <-
                  data.tree::FindNode(
                    rxsStructure$parsedEntities$recursingNodes,
                    valueName
                  );
              }

              if (!is.null(relevantNode)) {
                valueTemplateName <- relevantNode[[eC$valueTemplateCol]];
                errorMsg <-
                  rxsStructure$parsedValueTemplates[[valueTemplateName]]$error;
                if (is.null(errorMsg) ||
                    is.na(errorMsg) ||
                    (nchar(trimws(errorMsg)) == 0)) {
                  errorMsg <- "(no custom error message specified)";
                } else {
                  errorMsg <- gsub('NAME',
                                   valueName,
                                   errorMsg);
                }
              }
            }
            if (nchar(trimws(errorMsg)) > 0) {
              errorMsg <- paste0(": ", errorMsg);
            }
            
            validationMsg <-
              failedValidation(
                "Failed validation for clustered entity '",
                valueName, "' (in clustering entity '",
                node$name,
                "')", errorMsg
              );
            validationStatus <- FALSE;
            
          } else if (is.na(validationOutcome)) {
            
            errorMsg <-
              paste0("Validation code for clustered entity '",
                     valueName,
                     "', in clustering entity '",
                     node$name,
                     "', evaluated to NA.");
            
          } else {
            validationMsg <-
              passedValidation(
                "Unexpected validation result for clustered entity '",
                valueName, "' (in clustering entity '",
                node$name, "':", validationOutcome
              );
            validationStatus <- TRUE;
          }
        } else {
          validationMsg <-
            passedValidation(
              "Passed validation for clustered entity '",
              valueName, "' (in clustering entity '",
              node$name, "'!"
            );
          validationStatus <- TRUE;
        }
        
        ### Set result
        rxsTree$validationLog <-
          c(rxsTree$validationLog,
            validationMsg);
        rxsTree$validationResults <-
          c(rxsTree$validationResults,
            list(list(entityId = valueName,
                      entityPath = paste0(node$pathString, "\\", valueName),
                      validation = validationMsg,
                      validated = validationStatus)));
        
      }
        
    } else if (is.expression(node$validation)) {
      
      browser();
      
      ###-----------------------------------------------------------------------
      ### Single entity with a set validation
      ###-----------------------------------------------------------------------
      
      VALUE <- node$value;
      errorMsg <- "";
      
      ### Execution of validation code
      validationOutcome <- eval(node$validation);
      
      if (is.logical(validationOutcome) && validationOutcome) {
        
        if (node$name == "terminology") {
          browser();
        }
        
        validationMsg <-
          passedValidation(
            "Passed validation for entity '", node$name, "'!"
          );
        validationStatus <- TRUE;
        
      } else if (is.logical(validationOutcome) && !validationOutcome) {
        
        if (!is.null(rxsStructure)) {
          ### Find relevant node (entity) and get the valueTemplate
          ### Then get the error message to use.
          relevantNode <-
            data.tree::FindNode(
              rxsStructure$parsedEntities$extractionScriptTree,
              node$name
            );
          if (is.null(relevantNode)) {
            ### Either a node with this name does not exist,
            ### or it's a recursive node
            relevantNode <-
              data.tree::FindNode(
                rxsStructure$parsedEntities$recursingNodes,
                node$name
              );
          }
          if (is.null(relevantNode)) {
            valueTemplateName <- relevantNode[[eC$valueTemplateCol]];
            errorMsg <-
              rxsStructure$parsedValueTemplates[[valueTemplateName]]$error;
            if (is.null(errorMsg) ||
                is.na(errorMsg) ||
                (nchar(trimws(errorMsg)) == 0)) {
              errorMsg <- "";
            } else {
              errorMsg <- gsub('NAME',
                               node$name,
                               errorMsg);
            }
          }
        }
        
        if (nchar(trimws(errorMsg)) > 0) {
          
          validationMsg <-
            failedValidation(
              "Failed validation for entity '", node$name, "': ", errorMsg
            );
          validationStatus <- FALSE;
          
        } else {

          validationMsg <-
            passedValidation(
              "Passed validation for entity '", node$name, "'!"
            );
          validationStatus <- TRUE;
          
        }

      } else {
        
        validationMsg <-
          failedValidation(
            "Unexpected validation result for entity '", node$name, "': ",
            validationOutcome
          );
        validationStatus <- FALSE;
        
      }
      
      ### Set result
      rxsTree$validationLog <-
        c(rxsTree$validationLog,
          validationMsg);
      rxsTree$validationResults <-
        c(rxsTree$validationResults,
          list(list(entityId = node$name,
                    entityPath = node$pathString,
                    validation = validationMsg,
                    validated = validationStatus)));
      
    } else {
      
      ###-----------------------------------------------------------------------
      ### No validation set: probably a container entity
      ###-----------------------------------------------------------------------
      
      if (!is.null(node$value)) {
        
        validationMsg <-
          passedValidation(
            "No validation directives specified for entity '", node$name, "'."
          );
        validationStatus <- NA;
        
        ### Set result
        rxsTree$validationLog <-
          c(rxsTree$validationLog,
            validationMsg);
        rxsTree$validationResults <-
          c(rxsTree$validationResults,
            list(list(entityId = node$name,
                      entityPath = node$pathString,
                      validation = validationMsg,
                      validated = validationStatus)));
        
      }
      
    }

  });
  
  rxsTree$validationLog <-
    c(rxsTree$validationLog,
      paste0("Validation run ending at ",
             format(Sys.time(),
                    "%Y-%m-%d at %H:%M:%S %Z (UTC%z)")));

  if (stopOnFailure) {
    
    failedValidations <- unlist(lapply(
      rxsTree$validationResults,
      function(x) {
        return(!x$validated);
      }));

    if (any(failedValidations)) {
      stop(
        metabefor::wrap_error(
          paste0(rxsTree$validationLog[c(FALSE, failedValidations, FALSE)],
                 collapse = "\n\n")
        )
      )
    }
  }

  return(invisible(rxsTree));
  
}
