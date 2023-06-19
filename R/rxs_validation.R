#' Validate an Rxs tree
#'
#' @param rxsTree The Rxs tree.
#' @param stopOnFailure Whether to throw an error when validation fails.
#' @param stopOnFailure_metadata Whether to throw an error when validation
#' fails, but only if validation fails for the metadata.
#' @param stopOnFailure_entities Whether to throw an error when validation
#' fails, but only if validation fails for the entity values.
#' @param rxsTemplateSpec Optionally, the rxs template specification as it is
#' typically included in rxs templates.
#' @param silent Whether to be silent or chatty.
#'
#' @return Invisibly, the Rxs tree (which was altered in place, consistent
#' with the reference semantics employed by [data.tree::Node()] - so you can
#' just discard the result).
#' 
#' @export
rxs_validation <- function(rxsTree,
                           stopOnFailure = FALSE,
                           stopOnFailure_metadata = stopOnFailure,
                           stopOnFailure_entities = stopOnFailure,
                           rxsTemplateSpec = NULL,
                           silent = metabefor::opts$get("silent")) {

  passedValidation <- function(...) {
    return(msg("\n", paste("\U2705", paste0(..., sep="", collapse="")), silent=silent));
  }
  failedValidation <- function(...) {
    return(msg("\n", paste("\U26D4", paste0(..., sep="", collapse="")), silent=silent));
  }
  
  eC <- metabefor::opts$get("entityColNames");

  rxsTree$validationLog <-
    paste0("Validation run starting at ",
           format(Sys.time(),
                  "%Y-%m-%d at %H:%M:%S %Z (UTC%z)"));
  
  rxsTree$validationResults <- list();
    
  if (!is.null(rxsTemplateSpec)) {
    rxsStructure <- list();
    if ("entities" %in% names(rxsTemplateSpec)) {
      rxsStructure$parsedEntities <-
        rxs_parseEntities(
          rxsTemplateSpec$entities
        );
    }
    if ("valueTemplates" %in% names(rxsTemplateSpec)) {
      rxsStructure$parsedValueTemplates <-
        rxs_parseValueTemplates(
          rxsTemplateSpec$valueTemplates
        );
    }
  }
  
  ###---------------------------------------------------------------------------
  ### Validate source identifier
  ###---------------------------------------------------------------------------
  
  if (!(grepl("^[a-zA-Z0-9]{3,}$|^qurid_[a-zA-Z0-9]+$",
              rxsObject$rxsMetadata$id,
              ignore.case=TRUE))) {
    
    rxsTree$validationResults$sourceIdValidation <-
      list(
        entityId = "sourceId",
        entityPath = "metadata",
        
        validation = failedValidation(
          "Failed validation: the source identifier you specified, '",
          rxsObject$rxsMetadata$id, 
          "', does not validate - it does not match the predefined format!"
        ),
        validated = FALSE
      );
    
  } else {
    
    rxsTree$validationResults$sourceIdValidation <-
      list(
        entityId = "sourceId",
        entityPath = "metadata",
        
        validation = passedValidation(
          "Passed validation: the source identifier you specified, '",
          rxsObject$rxsMetadata$id, 
          "', matches the predefined format!"
        ),
        validated = TRUE
      );
    
  }
  
  rxsTree$validationLog <-
    c(rxsTree$validationLog,
      rxsTree$validationResults$sourceIdValidation$validation
    );
  
  ###---------------------------------------------------------------------------
  ### Validate extractor identifier
  ###---------------------------------------------------------------------------
  
  if (!(grepl("^[a-zA-Z][a-zA-Z0-9_]*$", rxsObject$rxsMetadata$extractorId, ignore.case=TRUE))) {
    
    rxsTree$validationResults$extractorIdValidation <-
      list(
        entityId = "extractorId",
        entityPath = "metadata",
        
        validation = failedValidation(
          "Failed validation: the extractor identifier you specified, '",
          rxsObject$rxsMetadata$id, 
          "', does not validate - it does not match the predefined format!"
        ),
        validated = FALSE
      );
    
  } else {
    
    rxsTree$validationResults$extractorIdValidation <-
      list(
        entityId = "extractorId",
        entityPath = "metadata",
        
        validation = passedValidation(
          "Passed validation: the extractor identifier you specified, '",
          rxsObject$rxsMetadata$extractorId, 
          "', matches the predefined format!"
        ),
        validated = TRUE
      );
    
  }
  
  rxsTree$validationLog <-
    c(rxsTree$validationLog,
      rxsTree$validationResults$extractorIdValidation$validation
    );
  
  ###---------------------------------------------------------------------------
  ### Validate entity identifier uniqueness
  ###---------------------------------------------------------------------------

  if (!data.tree::AreNamesUnique(rxsTree)) {

    rxsTree$validationResults$entityIdValidation <-
      list(
        entityId = rxsTree$root$name,
        entityPath = rxsTree$root$name,
        validation = failedValidation(
          "Failed validation: not all entity identifiers are unique!"),
        validated = FALSE
      );
    
  } else {
    
    rxsTree$validationResults$entityIdValidation <-
      list(
        entityId = rxsTree$root$name,
        entityPath = rxsTree$root$name,
        validation = passedValidation(
          "Passed validation: all entity identifiers are unique!"),
        validated = TRUE
      );
    
  }

  rxsTree$validationLog <-
    c(rxsTree$validationLog,
      rxsTree$validationResults$entityIdValidation$validation
    );

  if (stopOnFailure_metadata) {
    tmpValidationMsg <- c();
    if (!rxsTree$validationResults$sourceIdValidation$validated) {
      tmpValidationMsg <-
        c(tmpValidationMsg,
          rxsTree$validationResults$sourceIdValidation$validation);          
    }
    if (!rxsTree$validationResults$extractorIdValidation$validated) {
      tmpValidationMsg <-
        c(tmpValidationMsg,
          rxsTree$validationResults$extractorIdValidation$validation);          
    }
    if (!rxsTree$validationResults$entityIdValidation$validated) {
      tmpValidationMsg <-
        c(tmpValidationMsg,
          rxsTree$validationResults$entityIdValidation$validation);          
    }
    if (length(tmpValidationMsg) > 0) {
      stop(
        metabefor::wrap_error(
          paste(
            tmpValidationMsg,
            collapse = " Also, "
          )
        )
      );
    }
  }

###---------------------------------------------------------------------------
### Validate values entered for each entity
###---------------------------------------------------------------------------

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
                    all(is.na(errorMsg)) ||
                    (all(nchar(trimws(errorMsg)) == 0))) {
                  errorMsg <-
                    paste0("(no custom error message specified: ",
                           "entity identifier = '", valueName,
                           "', value = '", VALUE, "')");
                } else {
                  errorMsg <- gsub('NAME',
                                   valueName,
                                   errorMsg);
                  errorMsg <- gsub('VALUE',
                                   paste0(VALUE, collape=" || "),
                                   errorMsg);
                }
              }
            }
            if (any(nchar(trimws(errorMsg)) > 0)) {
              errorMsg <- paste0(": ", paste(errorMsg, collapse=" | "));
            }
            
            validationMsg <-
              failedValidation(
                "Failed validation for clustered entity '",
                valueName, "' (in clustering entity '",
                node$name,
                "'): ", errorMsg
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
                node$name, "': ", validationOutcome
              );
            validationStatus <- TRUE;
          }
        } else {
          validationMsg <-
            passedValidation(
              "Passed validation for clustered entity '",
              valueName, "' (in clustering entity '",
              node$name, "')!"
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

      ###-----------------------------------------------------------------------
      ### Single entity with a set validation
      ###-----------------------------------------------------------------------
      
      VALUE <- node$value;
      errorMsg <- "";
      
      ### Execution of validation code
      validationOutcome <- eval(node$validation);
      
      if (is.logical(validationOutcome) && validationOutcome) {
        
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
          if (!is.null(relevantNode)) {
            valueTemplateName <- relevantNode[[eC$valueTemplateCol]];
            errorMsg <-
              rxsStructure$parsedValueTemplates[[valueTemplateName]]$error;
            if (is.null(errorMsg) ||
                all(is.na(errorMsg)) ||
                (all(nchar(trimws(errorMsg)) == 0))) {
              errorMsg <- "";
            } else {
              errorMsg <- gsub('NAME',
                               node$name,
                               errorMsg);
            }
          } else {
          
            errorMsg <-
              paste0(
                "No custom error message was set, but the value that failed ",
                "the validation was '", VALUE, "'."
              );
          }
          
        }
        
        if (any(nchar(trimws(errorMsg)) > 0)) {
          
          validationMsg <-
            failedValidation(
              "Failed validation for entity '", node$name, "': ",
              paste0(errorMsg, collapse = " | ")
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

  if (stopOnFailure_entities) {
    
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
