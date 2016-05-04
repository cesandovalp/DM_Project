main = function(document)
{
  result1 = list()
  result2 = list()

  term = c()
  tid  = c()

  no_cores = detectCores() - 1
  cl       = makeCluster(no_cores, type = "FORK")
  result1  = parLapply(cl, document, pdf.vector)

  for( d in 1:length(document) )
  {
    result2[[d]] = c(0)
    term         = join.aux( term, result1[[d]] )
  }

#  stop(length(term))

  for( d in 1:length(result1) )
  {
#    print(document[d])
#    print(length(term))
    result2[[d]][term[which(!term %in% names(document[d]))]] = 0
  }

  frequency_mat = matrix(unlist(result2), nrow=length(document), ncol=length(term), byrow=TRUE)

  tf_idf_mat = tf_idf(frequency_mat, tid)

  tf_idf_mat
}
