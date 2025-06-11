#' Get string distances, optionally using multiple cores
#'
#' @param x The first vector
#' @param y The second vector
#' @param stringDistanceMethod The method to use (`"osa"`, `"lv"`, `"dl"`,
#' `"hamming"`, `"lcs"`, `"qgram"`, `"cosine"`, `"jaccard"`, `"jw"`, or
#' `"soundex"`)
#' @param parallel Whether to use parallel processing
#' @param silent Whether to be silent or chatty
#'
#' @return A data frame with the string distances between the elements, with the
#' first vector determining the rows, and the second vector, the columns.
#' @export
#'
#' @examples ### Create two vectors with strings
#' a <- c("Apple", "Blueberry", "Cherry", "Date");
#' b <- c("Airplane", "Bus", "Cycle");
#' 
#' dedup_get_stringDistances(
#'   a,
#'   b
#' );
dedup_get_stringDistances <- function(x,
                                      y = x,
                                      stringDistanceMethod = "osa",
                                      parallel = FALSE,
                                      silent = metabefor::opts$get("silent")) {
  
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("To deduplicate, you need the `stringdist` package! You can ",
         "install it with:\n\n  install.packages('stringdist');\n");
  }
  
  if (parallel) {
    
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("If you want to use parallel processing, ",
           "you need to have the parallel package, which ",
           "*should* normally be part of base R.");
    }
    
    ### Detect number of cores and create a cluster
    nCores <- parallel::detectCores();
    
    ### Because the trick below doesn't seem to work
    maxCores <- metabefor::opts$get("maxCores");
    if (!is.null(maxCores) && is.numeric(maxCores)) {
      nCores <- min(maxCores, nCores);
    }
    
    ### From https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      nCores <- min(2L, nCores);
    }
    
    msg("\nI will use ", nCores,
        " processor cores.", silent = silent);

  } else {
    
    nCores <- 1;
    
    msg("\nI will not use multiple processor cores.",
        silent = silent);
    
  }
  
  if (nCores == 1) {
    
    res <-
      stringDistances <-
      stringdist::stringdistmatrix(
        x,
        y,
        method = stringDistanceMethod
      );
    
  } else {
    
    ### Multi core approach
    
    cl <- parallel::makeCluster(nCores);

    ### Export the second vector (first will go through parLapplyLB) and
    ### the method
    parallel::clusterExport(
      cl,
      c('y',
        'stringDistanceMethod'),
      envir = environment()
    );
    
    ### Decide which bits of the matrix to produce in each core - based on
    ### https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
    x_in_parts <- split(x, ceiling(seq_along(x)/(length(x)/nCores)));
    
    ### Perform the parallel computations
    res_parts <-
      parallel::parLapplyLB(
        cl,
        x_in_parts,
        stringdist::stringdistmatrix,
        b = y,
        method = stringDistanceMethod
      );
    
    ### Stop the cluster
    parallel::stopCluster(cl);
    
    res <- rbind(res_parts);
    
  }
  
  return(res);
  
}