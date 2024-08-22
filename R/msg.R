msg <- function(...,
                silent = metabefor::opts$get("silent")) {
  if (!silent) {
    cat0(...);
  }
  return(
    invisible(
      paste0(
        ...
      )
    )
  );
}
