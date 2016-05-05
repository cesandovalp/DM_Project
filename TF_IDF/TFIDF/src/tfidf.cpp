#include <Rcpp.h>
#include <math.h>
#include <omp.h>

// [[Rcpp::plugins(openmp)]]
using namespace Rcpp;

/**********************************************************
_____FREQUENCY_____|_____TF-IDF______|_____Cosine_____
   |   Term        |  |    Term      |  |  Document
  D|------------   | D|------------  | D|------------
  o|------------   | o|------------  | o|------------
  c|------------   | c|------------  | c|------------
  u|------------   | u|------------  | u|------------
  m|------------   | m|------------  | m|------------
  e|------------   | e|------------  | e|------------
  n|------------   | n|------------  | n|------------
  t|------------   | t|------------  | t|------------
**********************************************************/

// tf(t,d) = the number of times that term t occurs in document d
double tf(double raw_frequency, double max);
// idf(t,D) = whether the term is common or rare across all documents
double idf(int N, double tid);
// Cosine similarity
NumericMatrix cos_similarity(NumericMatrix tf_idf_mat);

// [[Rcpp::export]]
NumericMatrix tf_idf(NumericMatrix raw_freq, NumericVector tid, NumericVector max_freq)
{
  NumericMatrix result(raw_freq.nrow(), raw_freq.ncol());

  omp_set_num_threads(32);
  #pragma omp parallel for
  for(int doc = 0; doc < raw_freq.nrow(); ++doc)
  {
    for(int term = 0; term < raw_freq.ncol(); ++term)
    {
      result(doc, term) = tf(raw_freq(doc, term), max_freq[doc]) * idf(raw_freq.nrow(), tid[term]);
    }
  }

  /*CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
  NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
  List z            = List::create( x, y ) ;*/

  return result;
}

double tf(double raw_frequency, double max)
{
  return 0.5 + (0.5*raw_frequency)/max;
}

double idf(int N, double tid)
{
  return log(1 + (N/tid));
}

NumericMatrix cos_similarity(NumericMatrix tf_idf_mat)
{
  NumericMatrix result(tf_idf_mat.nrow(),tf_idf_mat.nrow());

  for(int document_i = 0; document_i < tf_idf_mat.nrow(); ++document_i)
  {
    for(int document_j = 0; document_j < tf_idf_mat.nrow(); ++document_j)
    {
      double dotp = 0, maga = 0, magb = 0;
      double d;

      for(int term = 0; term < tf_idf_mat.ncol(); term++)
      {
        dotp += tf_idf_mat(document_i, term)*tf_idf_mat(document_j, term);
        maga += pow(tf_idf_mat(document_i, term), 2);
        magb += pow(tf_idf_mat(document_j, term), 2);
      }

      maga = sqrt(maga);
      magb = sqrt(magb);
      d = dotp / (maga * magb);

      result(document_i, document_j) = d;
    }
  }

  return result;
}
