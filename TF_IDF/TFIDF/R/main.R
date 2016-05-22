library(tm)

library(parallel)
library(stringr)

calculate_frequency = function(document)
{
  result1  = list()
  no_cores = detectCores() - 1
  cl       = makeCluster(no_cores, type = "FORK")
  result1  = parLapply(cl, document, pdf.vector)

  stopCluster(cl)
  
  save(result1, file="/home/cesandovalp/Documents/MDA/OUTPUT/documents.robject")
}

calculate_max_freq = function()
{
  load("/home/cesandovalp/Documents/MDA/OUTPUT/documents.robject")

  no_cores = detectCores() - 1
  cl       = makeCluster(no_cores, type = "FORK")
  max_freq = (unlist(parLapply(cl, result1, max)))

  save(max_freq, file="/home/cesandovalp/Documents/MDA/OUTPUT/max_freq.robject")
}

calculate_all_terms = function(document)
{
  load("/home/cesandovalp/Documents/MDA/OUTPUT/documents.robject")

  term    = c()
  for( d in 1:length(document) ) { term = join.aux( term, result1[[d]] ) }
  term = sort(term)

  save(term, file="/home/cesandovalp/Documents/MDA/OUTPUT/term.robject")
}

main = function(document)
{
  load("/home/cesandovalp/Documents/MDA/OUTPUT/documents.robject")
  load("/home/cesandovalp/Documents/MDA/OUTPUT/max_freq.robject")
  load("/home/cesandovalp/Documents/MDA/OUTPUT/term.robject")

  result2 = list()

  print("2")

#  print(result1[[1]][term])

  temp_fun = function(x)
  {
    x[setdiff(term, names(x))] = 0
    x
#    x[term]
#    result = replace(result, is.na(result), 0)
#    result[term[which(!term %in% names(x))]] = 0
#    result[sort(names(result))]
  }
  cl      = makeCluster(no_cores, type = "FORK")
  result2 = parLapply(cl, result1, temp_fun)
  stopCluster(cl)

  print("3")

  tid = result2[[1]]
  print(max(result2[[1]]))
  print(max(result2[[2]]))
  print(max(result2[[3]]))

  for(i in 2:length(document)) { tid = tid + result2[[i]]}

  frequency_mat = matrix(unlist(result2), nrow=length(document), ncol=length(term), byrow=TRUE)

  #tf_idf_mat = tf_idf(frequency_mat, tid, max_freq)

  tf_idf_mat = 0

  tf_idf_mat
}
