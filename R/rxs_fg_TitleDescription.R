rxs_fg_TitleDescription <- function(title,
                                    description,
                                    level = 0) {

  lV <- rxs_fg_layoutVars(level = level);

  res <- lV$lineFiller;

  if (!is.null(title)) {
    if ("Node" %in% class(title)) {
      title <- title$name;
    } else if (!("character" %in% class(title))) {
      stop("A 'title' was passed that has class '",
           vecTxtQ(class(title)), "', but should have class 'character' or 'Node'.");
    }
    res <- c(res,
             lV$commentPrefix,
             strwrap(toupper(title), width=lV$commentWidth, prefix=lV$commentPrefix));
  }

  if (!is.null(description)) {
    res <- c(res,
             c(lV$commentPrefix,
               strwrap(description, width=lV$commentWidth, prefix=lV$commentPrefix)));
  }

  ### Construct the character vector to return
  if (!is.null(title) || !is.null(description)) {
    res <- c(res,
             lV$commentPrefix,
             lV$lineFiller);
  }

  return(res);

}
