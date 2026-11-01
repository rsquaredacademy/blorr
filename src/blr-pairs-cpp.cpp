#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
DataFrame blr_pairs_cpp(NumericVector x, NumericVector y) {

  // loop counters
  R_xlen_t n1 = x.size();
  R_xlen_t n2 = y.size();

  // count pairs; use wide (double) counters so more than 2^31 - 1
  // cross-class pairs do not overflow
  double pairs_count = 0;
  double concordant = 0;
  double discordant = 0;
  double ties = 0;

  // compute
  for(R_xlen_t i = 0; i < n1; i++) {
    for(R_xlen_t j = 0; j < n2; j++) {

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

