rxs_fg_layoutVars <- function(level=1,
                              indent = metabefor::opts$get("indentDefault"),
                              indentSpaces = metabefor::opts$get("indentSpaces"),
                              fullWidth = metabefor::opts$get("fullWidth"),
                              commentCharacter = metabefor::opts$get("commentCharacter"),
                              fillerCharacter = metabefor::opts$get("fillerCharacter")) {

  res <- list();

  level <- max(level - 1, 0);

  if (indent) {
    res$indentSpaces <- repStr(level * indentSpaces);
    res$fullWidth <- fullWidth - nchar(res$indentSpaces);
  } else {
    res$indentSpaces <- "";
    res$fullWidth <- fullWidth;
  }
  res$lineFiller <- paste0(res$indentSpaces,
                           repStr(fillerCharacter,
                                  res$fullWidth));
  res$commentWidth <- res$fullWidth - 4;
  res$commentPrefix <- paste0(res$indentSpaces,
                              commentCharacter,
                              repStr(fillerCharacter, 2),
                              " ");
  res$valuePrefix <- paste0(res$indentSpaces, repStr(4));

  res$fillerCharacter <- fillerCharacter;
  res$commentCharacter <- commentCharacter;
  
  return(res);

}
