main = function()
{
  document = c("/home/cristiansp/Documents/MDA/DM_Project/TF_IDF/Test/1.pdf",
               "/home/cristiansp/Documents/MDA/DM_Project/TF_IDF/Test/2.pdf",
               "/home/cristiansp/Documents/MDA/DM_Project/TF_IDF/Test/3.pdf")

  result1 = list()
  result2 = list()

  term = c()
  tid = c()

  for( d in 1:length(document) )
  {
    result1[[d]] = pdf.vector( document[d] )
    result2[[d]] = c(0)
    term        = join.aux( term, result1[[d]] )
  }

  for( d in 1:length(result1) )
  {
    for( t in 1:length(term) )
    {
      if( is.na(tid[t]) ) { tid[t] = 0 }

      if( is.na(result1[[d]][term[t]]) )
      {
        result2[[d]][term[t]] = 0
      }else
      {
        result2[[d]][term[t]] = result1[[d]][term[t]]
        tid[t] = tid[t] + 1
      }
    }
    result2[[d]] = result2[[d]][-1]
  }

  result2 = matrix(unlist(result2), nrow=length(document), ncol=length(term), byrow=TRUE)

  list(term, result2, tid)
}
