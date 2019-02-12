context("PRISMA flowchart tests")

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

test_that("reading a source with no ROCK stuff works properly", {
  
  res1 <- specify_search(date = "2019-09-03",
                         query = "Blabla",
                         databases = list(ebsco  = c('psycINFO',
                                                     'ERIC',
                                          pubmed = 'PubMed')),
                         fields = "title");

  res2 <- list(psycINFO = 500,
               ERIC = 200,
               PubMed = 1000);
  
  res3 <- list(notEmpirical = 200,
               notQuantitative = 1500,
               incl = 100);
    
  res <- build_prisma_flowchart(search = res1,
                                hits = res2,
                                screening = res3);
  
}

###-----------------------------------------------------------------------------



