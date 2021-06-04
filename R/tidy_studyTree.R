#' @export
#' @rdname tidy_studyTrees
tidy_studyTree <- function(studyTree,
                           studyName = "",
                           eC = metabefor::opts$get('entityColNames'),
                           flattenToString = TRUE) {
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
            )
          return(df);
        },
        filterFun = data.tree::isNotRoot,
        simplify = FALSE
      )
    );
  res$study <- studyName;
  return(res);
}

