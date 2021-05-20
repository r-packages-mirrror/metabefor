#' @export
#' @rdname tidy_studyTrees
tidy_studyTree <- function(studyTree,
                           studyName = "",
                           eC = metabefor::opts$get('entityColNames')) {
  res <-
    metabefor::rbind_df_list(
      studyTree$Get(
        function(node) {
          return(
            data.frame(
              path = paste0(node$path, collapse=".."),
              name = node$name,
              value = ifelse(
                is.null(node$value),
                NA,
                metabefor::flattenNodeValue(node$value)
              )
            )
          );
        },
        filterFun = data.tree::isNotRoot,
        simplify = FALSE
      )
    );
  res$study <- studyName;
  return(res);
}

