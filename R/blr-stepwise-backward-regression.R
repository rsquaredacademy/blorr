#' Stepwise backward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on p values, in a stepwise manner until there is
#' no variable left to remove any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param prem p value; variables with p more than \code{prem} will be removed
#'   from the model.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{blr_step_p_backward}.
#' @param ... Other inputs.
#'
#' @return \code{blr_step_p_backward} returns an object of class \code{"blr_step_p_backward"}.
#' An object of class \code{"blr_step_p_backward"} is a list containing the
#' following components:
#'
#' \item{steps}{total number of steps}
#' \item{removed}{variables removed from the model}
#' \item{aic}{akaike information criteria}
#' \item{bic}{bayesian information criteria}
#' \item{dev}{deviance}
#' \item{indvar}{predictors}
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' @examples
#' \dontrun{
#' # stepwise backward regression
#' model <- glm(honcomp ~ female + read + science + math + prog + socst,
#'   data = hsb2, family = binomial(link = 'logit'))
#' blr_step_p_backward(model)
#'
#' # stepwise backward regression plot
#' model <- glm(honcomp ~ female + read + science + math + prog + socst,
#'   data = hsb2, family = binomial(link = 'logit'))
#' k <- blr_step_p_backward(model)
#' plot(k)
#' }
#'
#' @importFrom dplyr full_join select
#'
#' @family variable selection procedures
#'
#' @export
#'
blr_step_p_backward <- function(model, ...) UseMethod("blr_step_p_backward")

#' @export
#' @rdname blr_step_p_backward
#'
blr_step_p_backward.default <- function(model, prem = 0.3, details = FALSE, ...) {

  blr_check_model(model)
  blr_check_logic(details)
  blr_check_npredictors(model, 3)
  blr_check_values(prem, 0, 1)


  l        <- model$data
  nam      <- colnames(attr(model$terms, "factors"))
  response <- names(model$model)[1]
  preds    <- nam
  cterms   <- preds
  ilp      <- length(preds)
  end      <- FALSE
  step     <- 0
  rpred    <- c()
  aic      <- c()
  bic      <- c()
  dev      <- c()

  cat(format("Backward Elimination Method", justify = "left", width = 27), "\n")
  cat(rep("-", 27), sep = "", "\n\n")
  cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
  for (i in seq_len(length(nam))) {
    cat(paste(i, ".", nam[i]), "\n")
  }
  cat("\n")

  cat(crayon::bold$red("We are eliminating variables based on p value..."))
  cat("\n")

  cat("\n")
  if (!details) {
    cat("Variables Removed:", "\n\n")
  }

  while (!end) {
    m <- glm(paste(response, "~", paste(preds, collapse = " + ")), l, family = binomial(link = 'logit'))
    m_sum <- Anova(m, test.statistic = "Wald")
    pvals <- m_sum$`Pr(>Chisq)`
    # m_sum <- summary(m)
    # pvals <- unname(m_sum$coefficients[, 4])[-1]
    # m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
    # pvals <- m$pvalues[-1]
    maxp  <- which(pvals == max(pvals))

    suppressWarnings(
      if (pvals[maxp] > prem) {

        step   <- step + 1
        rpred  <- c(rpred, preds[maxp])
        preds  <- preds[-maxp]
        lp     <- length(rpred)
        fr     <- glm(paste(response, "~",
                                  paste(preds, collapse = " + ")), l, family = binomial(link = 'logit'))
        mfs    <- blr_model_fit_stats(fr)
        aic    <- c(aic, mfs$m_aic)
        bic    <- c(bic, mfs$m_bic)
        dev    <- c(dev, mfs$m_deviance)
        # fr     <- ols_regress(paste(response, "~",
        #                         paste(preds, collapse = " + ")), l)
        # aic    <- c(aic, ols_aic(fr$model))
        # bic    <- c(sbc, ols_sbc(fr$model))
        # dev    <- c(sbic, ols_sbic(fr$model, model))

        if (isRunning()) {
        cat(paste("-", dplyr::last(rpred), "added"), "\n")
        } else if (interactive()) {
          cat(crayon::red(clisymbols::symbol$cross), crayon::bold(dplyr::last(rpred)), "\n")
        } else {
          cat(paste("-", dplyr::last(rpred)), "\n")
        }

        if (details == TRUE) {
          cat("\n")
          cat(paste("Backward Elimination: Step", step, "\n\n"), paste("Variable", rpred[lp], "Removed"), "\n\n")
          m <- blr_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
          print(m)
          cat("\n\n")
        }
      } else {
        end <- TRUE
        cat("\n")
        cat(crayon::bold$red(glue("No more variables satisfy the condition of p value = {prem}")))
        cat("\n")
      }
    )
  }

  if (details == TRUE) {
    cat("\n\n")
    cat("Variables Removed:", "\n\n")
    for (i in seq_len(length(rpred))) {
      if (isRunning()) {
        cat(paste("-", rpred[i], "added"), "\n")
      } else if (interactive()) {
        cat(crayon::red(clisymbols::symbol$cross), crayon::bold(rpred[i]), "\n")
      } else {
        cat(paste("-", rpred[i]), "\n")
      }
    }
  }

  cat("\n\n")
  cat("Final Model Output", "\n")
  cat(rep("-", 18), sep = "", "\n\n")

  fi <- blr_regress(
    paste(response, "~", paste(preds, collapse = " + ")),
    data = l
  )
  print(fi)

  out <- list(removed    = rpred,
              indvar     = cterms,
              steps      = step,
              bic        = bic,
              aic        = aic,
              dev        = dev)

  class(out) <- "blr_step_p_backward"

  return(out)
}

#' @export
#'
print.blr_step_p_backward <- function(x, ...) {
  if (x$steps > 0) {
    print_step_backward(x)
  } else {
    print("No variables have been removed from the model.")
  }
}



#' @export
#' @rdname blr_step_p_backward
#'
plot.blr_step_p_backward <- function(x, model = NA, ...) {

  a <- NULL
  b <- NULL

  y <- seq_len(x$steps)

  d4 <- tibble(a = y, b = x$aic)
  d5 <- tibble(a = y, b = x$bic)
  d6 <- tibble(a = y, b = x$dev)

  p4 <- plot_stepwise(d4, "AIC")
  p5 <- plot_stepwise(d5, "BIC")
  p6 <- plot_stepwise(d6, "Deviance")

  # grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, top = "Stepwise Backward Regression")

  myplots <- list(plot_4 = p4, plot_5 = p5, plot_6 = p6)
  result <- marrangeGrob(myplots, nrow = 2, ncol = 2)
  result

}


