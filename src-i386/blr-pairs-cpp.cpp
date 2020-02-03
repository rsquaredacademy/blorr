#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
DataFrame blr_pairs_cpp(NumericVector x, NumericVector y) {

  // loop counters
  int n1 = x.size();
  int n2 = y.size();

  // count pairs
  int pairs_count = 0;
  int concordant = 0;
  int discordant = 0;
  int ties = 0;

  // compute
  for(int i = 0; i < n1; i++) {
    for(int j = 0; j < n2; j++) {

      pairs_count++;

      if (x[i] > y[j]) {
        concordant++;
      } else if (x[i] == y[j]) {
        ties++;
      } else {
        discordant++;
      }

    }
  }

  DataFrame df = DataFrame::create(Named("pairs") = pairs_count,
                                   Named("concordant") = concordant,
                                   Named("discordant") = discordant,
                                   Named("ties") = ties);

  return df;

}

