rxs_fg_dispatcher <- function(node,
                              valueTemplates,
                              silent=metabefor::opts$get("silent")) {
  
  eC <- metabefor::opts$get("entityColNames");
  indent <- metabefor::opts$get("indentDefault");
  indentSpaces <- metabefor::opts$get("indentSpaces");
  fullWidth <- metabefor::opts$get("fullWidth");
  commentCharacter <- metabefor::opts$get("commentCharacter");
  fillerCharacter <- metabefor::opts$get("fillerCharacter");
  repeatingSuffix <- metabefor::opts$get("repeatingSuffix");
  
  if (!("parsedValueTemplates" %in% class(valueTemplates))) {
    if (!silent) {
      cat("\nThe argument you provided as 'valueTemplates' does not have ",
          "class 'parsedValuetemplates', which implies that you have ",
          "not yet parsed the original spreadsheet. I will attempt to ",
          "do so myself now. Note that this means I will use (i.e. expect) ",
          "the default column names as specified in the `valueTemplateColNames` option.\n");
    }
    valueTemplates <- rxs_parseValueTemplates(valueTemplates);
    if (!silent) {
      cat("\nSuccessfully parsed the value templates.\n");
    }
  }

  if (node$isRoot) {
    ### This is the root
    rxs_fg_function <- rxs_fg_root;
  } else if (node$isLeaf &&
             is.character(node[[eC$recursingCol]]) &&
             node[[eC$recursingCol]] %in%
               node$Get(eC$recursingCol, traversal="ancestor")) {
    ### This is a recursing node with a parent that is the same
    ### recursing node (i.e. true recursion). We can therefore not
    ### include it (we'd keep going), but instead include a
    ### message for the coder to manually copy-paste this.
    rxs_fg_function <- rxs_fg_trueRecursion;

  } else if (is.null(node[[eC$valueTemplateCol]]) ||
             is.na(node[[eC$valueTemplateCol]])) {
    ### This is an organising element, without content
    if (is_TRUE(node[[eC$listCol]])) {
      ### Node with children, but children are simple values that can be returned
      ### in a list
      rxs_fg_function <- rxs_fg_list;
    } else {
      rxs_fg_function <- rxs_fg_container;
    }
  } else {
    ### Node without children
    rxs_fg_function <- rxs_fg_single;
  }

  return(rxs_fg_function(node,
                         valueTemplates = valueTemplates,
                         silent=silent));

}
