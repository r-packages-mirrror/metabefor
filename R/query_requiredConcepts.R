#' @rdname queries
#' @export
query_requiredConcepts <- function(...,
                                   conceptName = NULL) {
  res <- list(...);
  if (!all(sapply(res, function(x) class(x) == "mbf_query_conceptTerms"))) {
    stop("One or more of the argument you provided is not a list of concept terms. ",
         "Please only supply lists of terms for each concept as produced by a call to ",
         "the 'query_requiredConcepts' function. See ?query_requiredConcepts for more ",
         "information.");
  }
  resNode <- data.tree::Node$new(ifelse(is.null(conceptName), "concept", conceptName));
  resNode$operator <- "AND";
  for (i in seq_along(res)) {
    childName <-
      attr(res[[i]], ifelse(is.null("conceptName"),
                            #paste0(ordinalNr(i), " concept"),
                            paste0(i, ". concept"),
                            "conceptName"));
    resNode$AddChild(childName);
    resNode[[childName]]$object <-
      res[[i]];
    resNode[[childName]]$operator <-
      attr(res[[i]], "operator");
    for (j in seq_along(res[[i]])) {
      resNode[[childName]]$AddChild(res[[i]][j]);
    }
  }
  data.tree::SetGraphStyle(resNode, rankdir = "LR");
  data.tree::SetEdgeStyle(resNode,
                          arrowhead = "vee",
                          color = "#000000",
                          style="solid",
                          penwidth = 2);
  data.tree::SetNodeStyle(resNode,
                          style = "filled,rounded",
                          shape = "box",
                          fillcolor = "#DDDDDD",
                          fontname = "helvetica");
  
  operator_linetype_map <-
    c("OR" = "dotted",
      "AND" = "solid",
      "NOT" = "dashed");
  
  resNode$Do(function(node)
    data.tree::SetEdgeStyle(
      node,
      style =
        ifelse(
          node$parent$operator %in% names(operator_linetype_map),
          operator_linetype_map[node$parent$operator],
          "solid"
        )
    ),
    # style = dplyr::case_when(node$parent$operator=="OR" ~ "dotted",
    #                          node$parent$operator=="AND" ~ "solid",
    #                          node$parent$operator=="NOT" ~ "dashed",
    #                          TRUE ~ "solid")),
    traversal="level");
  
  
  attr(resNode, "conceptName") <- conceptName;
  class(resNode) <- c('mbf_query_requiredConcepts', class(resNode));
  return(resNode);
}

#' @rdname queries
#' @export
print.mbf_query_requiredConcepts <- function(x, ...) {
  x <- data.tree::Clone(x);
  class(x) <- setdiff(class(x), 'mbf_query_requiredConcepts');
  print(x);
  invisible(x);
}
