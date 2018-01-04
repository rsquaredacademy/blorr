#' \code{blorr} package
#'
#' Tools for developing binary logistic regression models
#'
#' See the README on
#' \href{https://github.com/rsquaredacademy/blorr}{GitHub}
#'
#' @docType package
#' @name blorr
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "n", "data",
  "converged", "model_info", "log_lik", "df.null", "value", "value1", "prob",
  "decile", "response", "total", "1", "0", "cum_total", "cum_1s", "cum_0s",
  "cum_1s_%", "cum_0s_%", "tp", "tn", "fp", "fn", "sensitivity", "specificity",
  "1 - specificity", "sensitivity_per", "no", "yes", "total", "dist_yes",
  "dist_no", "dist_diff", "woe", "iv", "distribution", "approval", "gains_table",
  "cum_total_per", "cum_1s_per", "cum_total_y", "y", "fitted.values",
  "response", "fit_val", '0s_expected', '0s_observed', '1s', '1s%', '1s_expected', '1s_observed', '2.5 %', '97.5 %',
  'ann_loc', 'ann_locate', 'avg_prob', 'cum_0s_per', 'cum_total_%', 'd_f', 'dist_table',
  'group', 'honcomp', 'ks', 'lr_ratio', 'n%', 'negative', 'p_value', 'positive', 'predict',
  'predicted', 'prob_n', 'sec', 'test_result', 'variable', 'woe_iv_table'))
