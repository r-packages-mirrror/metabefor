rxs_fg_TitleDescription <- function(title,
                                    description,
                                    instructions = NULL,
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
    description <-
      rxs_fragmentPrep_strwrap_with_prefix(description,
                                           width=lV$commentWidth,
                                           prefix=lV$commentPrefix);
    res <- c(res,
             c(lV$commentPrefix,
               description));
  }
  
  if (!is.null(instructions)) {
    instructions <-
      rxs_fragmentPrep_strwrap_with_prefix(instructions,
                                           width=lV$commentWidth,
                                           prefix=lV$commentPrefix);
    res <- c(res,
             c(lV$commentPrefix,
               instructions));
  }

  ### Construct the character vector to return
  if (!is.null(title) || !is.null(description) || !is.null(instructions)) {
    res <- c(res,
             lV$commentPrefix,
             lV$lineFiller);
  }

  return(res);

}
