returnPathToRoot <- function(node,
                             considerRoot = "") {
  
  rxsVersion <- metabefor::opts$get("rxsVersion");
  rxsObjectName <- metabefor::opts$get("rxsObjectName");
  
  res <- paste0(rev(unlist(sapply(data.tree::Traverse(node, traversal='ancestor'),
                                  function(x) return(x$name)),
                           recursive=TRUE)), collapse="$");
  res <- gsub(paste0("^.*\\$", considerRoot, "$"), "", res);
  res <- gsub(paste0("^.*\\$", considerRoot, "\\$"), "\\$", res);

  if (rxsVersion < "0.3.0") {
    return(res);
  } else {
    return(paste0(rxsObjectName, "$", res));
  }

}
