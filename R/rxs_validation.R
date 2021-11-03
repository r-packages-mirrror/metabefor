#' Validate a study tree
#'
#' @param studyTree The study tree
#' @param eC The entity columns; a named list with character values holding the
#' names of the columns in the `entities` worksheet of the spreadsheet. The
#' default values are stored in `metabefor::opts$get("entityColNames")` - if you
#' need to override these values, just reproduce that object.
#' @param rxsStructure Optionally, the `rxsStructure` as resulting from
#' a call to [rxs_parseSpecifications()].
#'
#' @return Invisibly, the study tree (which was altered in place, consistent
#' with the reference semantics employed by [data.tree::Node()].
#' 
#' @export
rxs_validation <- function(studyTree,
                           eC = metabefor::opts$get("entityColNames"),
                           rxsStructure=NULL) {
  
  
  
  studyTree$Set(validationResults = paste0("Validation run starting at ",
                                           format(Sys.time(), "%Y-%m-%d %H:%S")));
  
  if (!data.tree::AreNamesUnique(studyTree)) {
    studyTree$Set(validationResults =
                  list(c(studyTree$root$validationResults,
                         "Failed validation: not all node (entity) names are unique!")),
             filterFun = data.tree::isRoot,
             traversal = 'ancestor');
  }

  studyTree$Do(function(node) {
    if (is.list(node$value)) {
      ### Loop through the elements
      listValidationMessages <-
          sapply(names(node$value), function(valueName) {
            relevantNode <- NULL;
            if (!is.null(node$validation[[valueName]])) {
              VALUE <- node$value[[valueName]];
              errorMsg <- "";
              validationOutcome <- eval(node$validation[[valueName]]);
              if (is.na(validationOutcome)) {
                errorMsg <- paste0("Validation code for value '",
                                   valueName,
                                   "' in node '",
                                   node$name,
                                   "' evaluated to NA.");
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
                      errorMsg <- "";
                    } else {
                      errorMsg <- gsub('NAME',
                                       valueName,
                                       errorMsg);
                    }
                  }
                }
                if (nchar(trimws(errorMsg)) > 0) {
                  errorMsg <-
                    paste0(": ", errorMsg);
                }
                return(paste0("Failed validation for '",
                              valueName,
                              "'",
                              errorMsg));
              } else {
                return (NULL);
              }
            } else {
              return(NULL);
            }
          });
      node$Set(validationResults =
                 list(c(node$root$validationResults,
                        listValidationMessages)),
               filterFun = data.tree::isRoot,
               traversal = 'ancestor');
    } else if (is.expression(node$validation)) {
      VALUE <- node$value;
      errorMsg <- "";
      validationOutcome <- eval(node$validation);
      if (is.logical(validationOutcome) && !validationOutcome) {
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
          errorMsg <-
            paste0(": ", errorMsg);
        }
        validationMsg <- paste0("Failed validation for '",
                                node$name, "'",
                                errorMsg);
        node$Set(validationResults =
                   list(c(node$root$validationResults,
                          validationMsg)),
                 filterFun = data.tree::isRoot,
                 traversal = 'ancestor');
        invisible(NULL);
      }
    } else {
      invisible(NULL);
    }
  });
  
  studyTree$Set(validationResults =
                  list(c(studyTree$validationResults,
                         paste0("Validation run ending at ",
                                format(Sys.time(), "%Y-%m-%d %H:%S")))),
                filterFun = data.tree::isRoot);
  
  studyTree$validationResults <- unlist(studyTree$validationResults);
  
  return(invisible(studyTree));
  
}
