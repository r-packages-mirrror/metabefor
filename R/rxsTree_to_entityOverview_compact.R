rxsTree_to_entityOverview_compact <- function(extractionScriptTree,
                                              instructionHeadingLevel,
                                              extractionOverview_compact_intro) {
  
  entityOverview_compact <- extractionScriptTree$Get(
    function(node) {
      if (node$isRoot) {
        return(NULL);
      } else {
        res <- heading(
          node$title,
          headingLevel = instructionHeadingLevel + 1,
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
            node$name
          );
        
        if (!is.null(node$valueTemplate)) {
          res <-
            paste0(
              res,
              "`  \n**Value template**: `",
              node$valueTemplate
            );
        }
        
        res <-
          paste0(
            res,
            "`  \n**Repeating**: `",
            ifelse(is.null(node$repeating) || !(isTRUE(node$repeating)),
                   "FALSE",
                   "TRUE")
          );
        
        res <-
          paste0(
            res,
            "`  \n**Path in extraction script tree:** `",
            paste0(node$path, collapse=" > "),
            "`\n\n-----\n\n"
          );
        return(res);
      }
    }
  );
  
  entityOverview_compact <-
    paste0(
      heading(
        "Entity overview (compact)",
        headingLevel = instructionHeadingLevel,
        cat = FALSE
      ),
      extractionOverview_compact_intro,
      "\n\n",
      paste0(
        unlist(
          entityOverview_compact[!unlist(lapply(entityOverview_compact, is.na))]
        ),
        collapse = ""
      )
    );
  
  return(entityOverview_compact);
  
}