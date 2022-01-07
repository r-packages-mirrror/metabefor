sanitize_identifiers <- function(x,
                                 regex = "[^a-zA-Z0-9_]",
                                 warn = FALSE,
                                 warningMsg = "Found (and removed) illegal characters in code identifiers %s. They were changed to %s.") {

  x <- trimws(x);

  sanitizedIds <-
    gsub(regex, "", x);

  illegalIds <- x != sanitizedIds;
  if (warn && any(illegalIds)) {
    warning(
      sprintf(
        message,
        vecTxtQ(x[illegalIds]),
        vexTctQ(sanitizedIds[illegalIds])
      )
    );
  }

  return(sanitizedIds);

}
