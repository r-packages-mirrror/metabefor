wrap_error <- function(...) {
  return(
    paste0(
      "\n",
      "-------- ! metabefor error, please read carefully ! --------",
      "\n\n",
       wrapVector(paste0(...), 60),
       "\n\n",
      "____________________________________________________________",
      "\n\n"
    )
  );
}