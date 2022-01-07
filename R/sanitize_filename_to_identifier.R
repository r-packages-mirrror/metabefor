sanitize_filename_to_identifier <-
  function(x,
           regex = "[^a-zA-Z0-9_]",
           warningMsg = "Found (and removed) illegal characters in identifiers %s. They were changed to %s.",
           warn = FALSE) {

  x <- trimws(x);
  
  sanitizedIds <-
    gsub("\\.rxs\\.Rmd", "", x, ignore.case = TRUE);
  
  sanitizedIds <-
    gsub("[- .]", "_", sanitizedIds);
  
  sanitizedIds <-
    gsub(regex, "", sanitizedIds);
  
  sanitizedIds <-
    gsub("^_", "", sanitizedIds);
  
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
