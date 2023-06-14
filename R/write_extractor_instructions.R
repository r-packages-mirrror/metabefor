write_extractor_instructions <- function(x,
                                         output = NULL) {
  
  if (inherits(x, "rxsStructures")) {
    
    moduleInstructions <-
      lapply(
        x,
        write_extractor_instructions,
        output = NULL
      );
    
    res <- unlist(moduleInstructions);
    
  } else if (inherits(x, "rxsStructure")) {
    
    x$rxsSpecification$instructionSheet
    
    
    
    browser();
    
  } else {
    
    stop("You have to pass either an object of class `rxsStructure` or ",
         "an object of class `rxsStructures`; basically, the object ",
         "returned by metabefor::rxs_fromSpecifications().");

  }
  
  
  
}