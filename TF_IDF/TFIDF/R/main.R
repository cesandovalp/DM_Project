main = function(document)
{
  result1 = list()
  result2 = list()

  term = c()

  no_cores = detectCores() - 1
  cl       = makeCluster(no_cores, type = "FORK")
  result1  = parLapply(cl, document, pdf.vector)

  for( d in 1:length(document) )
  {
#    result2[[d]] = c(0)
    term = join.aux( term, result1[[d]] )
  }

  term = sort(term)

#  print(length(term))

  temp_fun = function(x)
  {
    result = x
    result[term[which(!term %in% names(x))]] = 0
    result[sort(names(result))]
  }

  cl      = makeCluster(no_cores, type = "FORK")
  result2 = parLapply(cl, result1, temp_fun)

  tid = result2[[1]]
  print(max(result2[[1]]))
  print(max(result2[[2]]))
  print(max(result2[[3]]))

  for(i in 2:length(document)) { tid = tid + result2[[i]]}

  #print(tid)
  print(max(tid))

  frequency_mat = matrix(unlist(result2), nrow=length(document), ncol=length(term), byrow=TRUE)

  tf_idf_mat = tf_idf(frequency_mat, tid)

  tf_idf_mat
}
