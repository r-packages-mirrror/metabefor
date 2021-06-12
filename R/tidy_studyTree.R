#' @export
#' @rdname tidy_studyTrees
tidy_studyTree <- function(studyTree,
                           studyName = "",
                           eC = metabefor::opts$get('entityColNames'),
                           flattenToString = TRUE,
                           silent = metabefor::opts$get('silent')) {
  res <-
    metabefor::rbind_df_list(
      studyTree$Get(
        function(node) {
          if (is.null(node$value)) {
            value <- NA;
            name <- node$name;
          } else if (flattenToString) {
            value <- metabefor::flattenNodeValue(node$value);
            name <- node$name;
          } else if (length(node$value) > 1) {
            value <- unlist(node$value);
            name <- paste0(node$name, "__", names(value));
          } else {
            value <- metabefor::flattenNodeValue(node$value);
            name <- node$name;
          }
          path <-
            rep(paste0(node$path, collapse=".."),
                length(value));
          df <-
            data.frame(
              path = path,
              name = name,
              value = value
            );
          
          if (!silent) {
            if (is.expression(node$value)) {
              cat0("\n- Encountered a value that was an R expression in ",
                   "entity node with name '", node$name, "': deparsed it and ",
                   "retrieved the result.");
            } else {
              cat0("\n- Succesfully retrieved non-expression value from ",
                   "entity node with name '", node$name, "'.");
            }
          }
          return(df);
        },
        filterFun = data.tree::isNotRoot,
        simplify = FALSE
      )
    );
  res$study <- studyName;
  return(res);
}

