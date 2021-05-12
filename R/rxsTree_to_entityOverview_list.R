rxsTree_to_entityOverview_list <- function(rxsTree,
                                           valueTemplates,
                                           headingLevel = 3) {
  
  extractionOverview_list_intro <-
    metabefor::opts$get('extractionOverview_list_intro');
  
  resTree <- data.tree::Clone(rxsTree);
  
  entityOverview_list <-
    resTree$Get(
      function(node) {
        if (node$isRoot) {
          return(NULL);
        } else {
          res <- ufs::heading(
            node$title,
            headingLevel = headingLevel + 1,
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
              node$name,
              "`"
            );
          
          if (!is.null(node$valueTemplate)) {
            res <-
              paste0(
                res,
                "  \n**Value description**: ",
                paste0(
                  trimws(
                    rxs_fg_valueTemplateDescription(
                      node,
                      valueTemplates,
                      commentCharacter = "",
                      fillerCharacter = "",
                      indentSpaces = 0,
                      indent = FALSE
                    )
                  ),
                  collapse=" "
                )
              );
          }
          
          res <-
            paste0(
              res,
              "  \n**Path in extraction script tree:** `",
              paste0(node$path, collapse=" > "),
              "`"
            );
          
          if (!is.null(node$valueTemplate)) {
            res <-
              paste0(
                res,
                "  \n**Value template**: `",
                node$valueTemplate,
                "`"
              );
          }
          
          res <-
            paste0(
              res,
              "  \n**Repeating**: `",
              ifelse(is.null(node$repeating) || !node$repeating,
                     "FALSE",
                     "TRUE"),
              "`"
            );
          
          res <- paste0(
            res,
            "\n\n-----\n\n"
          );
            
          
          return(res);
        }
      }
    );
  
  entityOverview_list <-
    paste0(
      ufs::heading(
        "Entity overview (list)",
        headingLevel = headingLevel,
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
  
  return(entityOverview_list);
  
}