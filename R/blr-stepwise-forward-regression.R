#' Stepwise forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on p values, in a stepwise manner until there is
#' no variable left to enter any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param penter p value; variables with p value less than \code{penter} will
#'   enter into the model
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{blr_step_p_forward}.
#' @param ... Other arguments.
#'
#' @return \code{blr_step_p_forward} returns an object of class \code{"blr_step_p_forward"}.
#' An object of class \code{"blr_step_p_forward"} is a list containing the
#' following components:
#'
#' \item{steps}{number of steps}
#' \item{predictors}{variables added to the model}
#' \item{aic}{akaike information criteria}
#' \item{bic}{bayesian information criteria}
#' \item{dev}{deviance}
#' \item{indvar}{predictors}
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
#'
#'
#' @examples
#' # stepwise forward regression
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'   family = binomial(link = 'logit'))
#' blr_step_p_forward(model)
#'
#' # stepwise forward regression plot
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#'   family = binomial(link = 'logit'))
#' k <- blr_step_p_forward(model)
#' plot(k)
#'
#' @importFrom stats qt
#' @importFrom car Anova
#'
#' @family variable selection procedures
#'
#' @export
#'
blr_step_p_forward <- function(model, ...) UseMethod("blr_step_p_forward")

#' @export
#' @rdname blr_step_p_forward
#'
blr_step_p_forward.default <- function(model, penter = 0.3, details = FALSE, ...) {

  blr_check_model(model)
  blr_check_logic(details)
  blr_check_npredictors(model, 3)
  blr_check_values(penter, 0, 1)

  l        <- model$data
  nam      <- colnames(attr(model$terms, "factors"))
  df       <- nrow(l) - 2
  tenter   <- qt(1 - (penter) / 2, df)
  n        <- ncol(l)
  response <- names(model$model)[1]
  all_pred <- nam
  cterms   <- all_pred
  mlen_p   <- length(all_pred)

  step     <- 1
  ppos     <- step 
  preds    <- c()
  pvals    <- c()
  tvals    <- c()
  aic      <- c()
  bic      <- c()
  dev      <- c()

  cat(format("Forward Selection Method", justify = "left", width = 27), "\n")
  cat(rep("-", 27), sep = "", "\n\n")
  cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
  for (i in seq_len(length(nam))) {
    cat(paste0(i, ". ", nam[i]), "\n")
  }
  cat("\n")

  cat(crayon::bold$red("We are selecting variables based on p value..."))
  cat("\n")

  cat("\n")
  if (!details) {
    cat("Variables Entered:", "\n\n")
  }


  for (i in seq_len(mlen_p)) {
    predictors <- all_pred[i]
    m <- glm(paste(response, "~", paste(predictors, collapse = " + ")),
             l, family = binomial(link = 'logit'))
    m_sum <- Anova(m, test.statistic = "Wald")
    pvals[i] <- m_sum$`Pr(>Chisq)`[ppos]
    tvals[i] <- m_sum$Chisq[ppos]
  }

  minp   <- which(pvals == min(pvals))
  # tvals  <- abs(tvals)
  # maxt   <- which(tvals == max(tvals))
  preds  <- all_pred[minp]
  lpreds <- length(preds)
  fr     <- glm(paste(response, "~", paste(preds, collapse = " + ")), l, family = binomial(link = 'logit'))
  mfs    <- blr_model_fit_stats(fr)
  aic    <- mfs$m_aic
  bic    <- mfs$m_bic
  dev    <- mfs$m_deviance

  if (details == TRUE) {
    cat("\n")
    cat(paste("Forward Selection: Step", step), "\n\n")
  }

  if (isRunning()) {
    cat(paste("-", dplyr::last(preds), "added"), "\n")
  } else if (interactive()) {
    cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
  } else {
    cat(paste("-", dplyr::last(preds)), "\n")
  }

  if (details == TRUE) {
    cat("\n")
    m <- blr_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
    print(m)
    cat("\n\n")
  }

  while (step < mlen_p) {

    all_pred <- all_pred[-minp]
    len_p    <- length(all_pred)
    ppos     <- ppos + length(minp)
    pvals    <- c()
    tvals    <- c()

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      m <- glm(paste(response, "~", paste(predictors, collapse = " + ")), l, family = binomial(link = 'logit'))
      m_sum <- Anova(m, test.statistic = "Wald")
      pvals[i] <- m_sum$`Pr(>Chisq)`[ppos]
      tvals[i] <- m_sum$Chisq[ppos]
      # m_sum <- summary(m)
      # pvals[i] <- unname(m_sum$coefficients[, 4])[ppos]
      # tvals[i] <- unname(m_sum$coefficients[, 3])[ppos]
      # m <- blr_regress(paste(response, "~",
      #                        paste(predictors, collapse = " + ")), l)
      # pvals[i] <- m$pval[ppos]
      # tvals[i] <- m$zval[ppos]
    }

    minp  <- which(pvals == min(pvals))
    # tvals <- abs(tvals)
    # maxt  <- which(tvals == max(tvals))

    if (pvals[minp] <= penter) {

      step   <- step + 1
      preds  <- c(preds, all_pred[minp])
      lpreds <- length(preds)
      fr     <- glm(paste(response, "~",
                                  paste(preds, collapse = " + ")), l, family = binomial(link = 'logit'))
      mfs    <- blr_model_fit_stats(fr)
      aic    <- c(aic, mfs$m_aic)
      bic    <- c(bic, mfs$m_bic)
      dev    <- c(dev, mfs$m_deviance)

      if (details == TRUE) {
        cat("\n")
        cat(paste("Forward Selection: Step", step), "\n\n")
      }

      if (isRunning()) {
        cat(paste("-", dplyr::last(preds), "added"), "\n")
      } else if (interactive()) {
        cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
      } else {
        cat(paste("-", dplyr::last(preds)), "\n")
      }

      if (details == TRUE) {
        cat("\n")
        m <- blr_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        print(m)
        cat("\n\n")
      }
    } else {
      cat("\n")
      cat(crayon::bold$red("No more variables to be added."))
      break
    }
  }

  if (details == TRUE) {
    cat("\n\n")
    cat("Variables Entered:", "\n\n")
    for (i in seq_len(length(preds))) {
      if (isRunning()) {
        cat(paste("+", preds[i]), "\n")
      } else if (interactive()) {
        cat(crayon::green(clisymbols::symbol$tick), crayon::bold(preds[i]), "\n")
      } else {
        cat(paste("+", preds[i]), "\n")
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


  out <- list(predictors = preds,
              indvar     = cterms,
              steps      = step,
               bic       = bic,
              aic        = aic,
              dev        = dev)

  class(out) <- "blr_step_p_forward"

  return(out)
}

#' @export
#'
print.blr_step_p_forward <- function(x, ...) {
  if (x$steps > 0) {
    print_step_forward(x)
  } else {
    print("No variables have been added to the model.")
  }
}

#' @importFrom gridExtra marrangeGrob
#' @export
#' @rdname blr_step_p_forward
#'
plot.blr_step_p_forward <- function(x, model = NA, ...) {

  a <- NULL
  b <- NULL

  y <- seq_len(length(x$aic))

  d4 <- tibble(a = y, b = x$aic)
  d5 <- tibble(a = y, b = x$bic)
  d6 <- tibble(a = y, b = x$dev)

  p4 <- plot_stepwise(d4, "AIC")
  p5 <- plot_stepwise(d5, "BIC")
  p6 <- plot_stepwise(d6, "Deviance")

  # grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, top = "Stepwise Forward Regression")
  myplots <- list(plot_4 = p4, plot_5 = p5, plot_6 = p6)
  result <- marrangeGrob(myplots, nrow = 2, ncol = 2)
  result

}


