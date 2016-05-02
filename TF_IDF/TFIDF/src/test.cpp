#include <Rcpp.h>
#include <math.h>
#include <iostream>
#include <omp.h>

// [[Rcpp::plugins(openmp)]]
using namespace Rcpp;
// using std::cout;
// using std::endl;

// tf(t,d) = the number of times that term t occurs in document d
double tf(double raw_frequency);
// idf(t,D) = whether the term is common or rare across all documents
double idf(int N, double tid);
/*
 |Term
D|------------
o|------------
c|------------
u|------------
m|------------
e|------------
n|------------
t|------------
 */

// [[Rcpp::export]]
NumericMatrix tf_idf(NumericMatrix raw_frequency, NumericVector tid)
{
  NumericMatrix result(raw_frequency.nrow(), raw_frequency.ncol());

  omp_set_num_threads(2);
  #pragma omp parallel for
  for(int document = 0; document < raw_frequency.nrow(); ++document)
  {
    for(int term = 0; term < raw_frequency.ncol(); ++term)
    {
      result(document, term) = tf(raw_frequency(document, term)) * idf(raw_frequency.nrow(), tid[term]);
    }
  }

  /*CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
  NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
  List z            = List::create( x, y ) ;*/

  return result;
}

double tf(double raw_frequency)
{
  return raw_frequency == 0 ? 0 : 1 + log(raw_frequency);
}

double idf(int N, double tid)
{
  return log(1 + (N/tid));
}