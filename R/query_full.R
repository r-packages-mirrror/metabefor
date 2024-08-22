#' Building queries
#'
#' This group of functions generates a symbolic representation of
#' a query that can then be translated to different interface 
#' languages. A query is built by specifying, for each required
#' concept, a list of concept terms (e.g. synonyms), as well as the
#' relationship between the concepts (normally they all have to occur;
#' sometimes there are also a number of terms to exclude).
#' 
#' `query_full` combines required concepts with a concept to exclude using
#' the `NOT` operator.
#' `query_requiredConcepts` combines sets of concept terms for each concept
#' using the `AND` operator.
#' `query_conceptTerms` combines terms within one concept using the `OR`
#' operator.
#' A query can be translated to one or more interface languages with
#' `query_toInterfaceLang`.
#'
#' @param ... For the query building functions, the concepts or terms
#' to combine; for the `print` methods, the object to print.
#' @param queryObject The query object.
#' @param x The query object to print.
#' @param headingLevel The level of the headings to use when printing.
#' @param queryName,conceptName The name of the query or concept.
#' @param operator The operator to use to combining concept terms.
#' @param inclusion The result of a call to `query_requiredConcepts`.
#' @param exclusion The result of a call to `query_conceptTerms`.
#' @param exclude Any terms to exclude as a character vector.
#' @param fields The fields in which to search; can currently 
#' only include 'title', 'abstract', or both.
#'
#' @rdname queries
#' 
#' @return `query_full` returns an `mbf_query_full` object;
#' `query_requiredConcepts` returns an `mbf_query_requiredConcepts`
#' object;
#' `query_conceptTerms` returns an `mbf_query_conceptTerms` object;
#' `query_toInterfaceLang` returns an `mbf_query_toInterfaceLang`
#' object; and the print methods return `x` invisibly.
#' @export
#'
#' @examples query_full(
#'   inclusion =
#'     query_requiredConcepts(
#'       conceptName = "inclusion",
#'       query_conceptTerms(
#'         conceptName="fluffiness",
#'         "fluffy",
#'         "hairy",
#'         "soft"),
#'       query_conceptTerms(
#'         conceptName="aggression",
#'         "aggressi*",
#'         "predator*",
#'         "kill*")),
#'   exclusion = 
#'     query_conceptTerms(
#'       conceptName = "exclude non-empirical work",
#'       "review",
#'       "meta-analysis"));
#'                          
query_full <- function(inclusion,
                       exclusion = NULL,
                       queryName = "query") {

  if (is.null(exclusion)) {
    return(inclusion);
  }

  resNode <- data.tree::Node$new(ifelse(is.null(queryName),
                                        "query",
                                        queryName));
  resNode$operator <- "NOT";

  resNode$AddChildNode(inclusion);

  if (inherits(exclusion, "mbf_query_conceptTerms")) {
    childName <- attr(exclusion, "conceptName");
    resNode$AddChild(childName);
    resNode[[childName]]$object <-
      exclusion;
    resNode[[childName]]$operator <-
      attr(exclusion, "operator");
    for (j in seq_along(exclusion)) {
      resNode[[childName]]$AddChild(exclusion[j]);
    }
  } else if (inherits(exclusion, "mbf_query_requiredConcepts")) {
    resNode$AddChildNode(exclusion);
  } else {
    stop("Argument `exclusion` has an invalid class!");
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

  attr(resNode, "queryName") <- queryName;
  class(resNode) <- c('mbf_query_full', class(resNode));
  return(resNode);
}

#' @rdname queries
#' @export
print.mbf_query_full <- function(x, ...) {
  x <- data.tree::Clone(x);
  class(x) <- setdiff(class(x), 'mbf_query_full');
  print(x);
  invisible(x);
}
