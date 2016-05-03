main = function()
{
  document = c("/home/cristian/Documents/DM_Project/TF_IDF/Test/1.pdf",
               "/home/cristian/Documents/DM_Project/TF_IDF/Test/2.pdf",
               "/home/cristian/Documents/DM_Project/TF_IDF/Test/3.pdf",
               "/home/cristian/Documents/DM_Project/TF_IDF/Test/4.pdf")

  result = list()

  term = c()

  for( d in 1:length(document) )
  {
    result[[d]] = pdf.vector( document[d] )
    term        = join.aux( term, result[[d]] )
  }

  for( d in result )
  {
    for( t in term )
    {
      if( is.na(d[t]) )
      {
        d[t] = 0
      }
    }
  }

  list(term, result)
}
