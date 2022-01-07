rxsTree_to_entityOverview_list <- function(rxsTree,
                                           valueTemplates,
                                           headingLevel = 3) {
  
  texts <- metabefor::opts$get(texts);
  eC <- metabefor::opts$get("entityColNames");
  
  extractionOverview_list_intro <-
    texts$extractionOverview_list_intro;

  resTree <- data.tree::Clone(rxsTree);
  
  resTree$Do(
    function(node) {
      childTitles <- node$Get(eC$titleCol,
                              filterFun = data.tree::isLeaf);
      childDescriptions <- node$Get(eC$descriptionCol,
                                    filterFun = data.tree::isLeaf);
      node$entityOverview_list_fragment <-
        paste0(
          "\n\n<table style='margin:15px;0px;20px;0px;'>\n",
          paste0(
            "\n<tr><td style='font-weight: bold;'>", childTitles,
            "</td><td>", childDescriptions,
            "</td></tr>",
            collapse="\n"
          ),
          "\n</table>\n\n"
        );
    },
    filterFun = function(node) {
      return(is_TRUE(node[[eC$listCol]]));
    }
  );
    
  entityOverview_list <-
    resTree$Get(
      function(node) {
        if (node$isRoot) {
          return(NULL);
        } else if (is_TRUE(node$parent[[eC$listCol]])) {
          ### Skip children of entity lists
          return(NULL);
        } else {
          
          res <- heading(
            node[[eC$titleCol]],
            headingLevel = headingLevel + 1,
            cat = FALSE
          );
          
          if (!is.null(node[[eC$valueTemplateCol]])) {
            type <- "Extractable Entity";
            listFragment <- FALSE;
          } else if (is_TRUE(node[[eC$listCol]])) {
            type <- "Extractable Entity List";
            listFragment <- node$entityOverview_list_fragment;
          } else {
            type <- "Entity Container";
            listFragment <- FALSE;
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
          
          if (isFALSE(listFragment)) {
            if (!is.null(node$valueTemplate)) {
              res <-
                paste0(
                  res,
                  "  \n**Value description**: ",
                  paste0(
                    trimws(
                      rxs_fg_valueTemplateDescription(
                        node,
                        valueTemplates
                      )
                    ),
                    collapse=" "
                  )
                );
            }
          } else {
            res <-
              paste0(
                res,
                listFragment
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
              ifelse(is_TRUE(node$repeating),
                     "TRUE",
                     "FALSE"),
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
      heading(
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