#' Stepwise regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering and removing predictors based on p values, in a stepwise manner
#' until there is no variable left to enter or remove any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param pent p value; variables with p value less than \code{pent} will enter
#'   into the model.
#' @param prem p value; variables with p more than \code{prem} will be removed
#'   from the model.
#' @param details Logical; if \code{TRUE}, will print the regression result at
  #' each step.
#' @param x An object of class \code{blr_step_p_both}.
#' @param ... Other arguments.
#' @return \code{blr_step_p_both} returns an object of class \code{"blr_step_p_both"}.
#' An object of class \code{"blr_step_p_both"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{glm}}
#' \item{orders}{candidate predictor variables according to the order by which they were added or removed from the model}
#' \item{method}{addition/deletion}
#' \item{steps}{total number of steps}
#' \item{predictors}{variables retained in the model (after addition)}
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
#' # stepwise regression
#' model <- glm(y ~ ., data = stepwise)
#' blr_step_p_both(model)
#'
#' # stepwise regression plot
#' model <- glm(y ~ ., data = stepwise)
#' k <- blr_step_p_both(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' }
#'
#' @family variable selection_procedures
#'
#' @export
#'
blr_step_p_both <- function(model, ...) UseMethod("blr_step_p_both")

#' @export
#' @rdname blr_step_p_both
#'
blr_step_p_both.default <- function(model, pent = 0.1, prem = 0.3, details = FALSE, ...) {

  blr_check_model(model)
  blr_check_logic(details)
  blr_check_npredictors(model, 3)
  blr_check_values(pent, 0, 1)
  blr_check_values(prem, 0, 1)

  response <-
    model %>%
    use_series(model) %>%
    names() %>%
    extract(1)

  l        <- model$data
  nam      <- colnames(attr(model$terms, "factors"))
  df       <- nrow(l) - 2
  tenter   <- qt(1 - (pent) / 2, df)
  trem     <- qt(1 - (prem) / 2, df)
  n        <- ncol(l)
  all_pred <- nam
  cterms   <- all_pred
  mlen_p   <- length(all_pred)


  pvalues <- c()
  lbetas  <- c()
  betas   <- c()
  preds   <- c()
  pvals   <- c()
  tvals   <- c()
  step    <- 1
  ppos    <- step
  aic     <- c()
  bic     <- c()
  dev     <- c()

  cat(format("Stepwise Selection Method", justify = "left", width = 27), "\n")
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
    cat("Variables Entered/Removed:", "\n\n")
  }


  for (i in seq_len(mlen_p)) {
    predictors <- all_pred[i]
    m <- glm(paste(response, "~", paste(predictors, collapse = " + ")),
             l, family = binomial(link = 'logit'))
    m_sum <- Anova(m, test.statistic = "Wald")
    pvals[i] <- m_sum$`Pr(>Chisq)`[ppos]
    tvals[i] <- m_sum$Chisq[ppos]
    # m_sum <- summary(m)
    # pvals[i] <- unname(m_sum$coefficients[, 4])[ppos]
    # tvals[i] <- unname(m_sum$coefficients[, 3])[ppos]
    # m <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), l)
    # pvals[i] <- m$pvalues[ppos]
    # tvals[i] <- m$tvalues[ppos]
  }

  minp    <- which(pvals == min(pvals))
  # tvals   <- abs(tvals)
  # maxt    <- which(tvals == max(tvals))
  preds   <- all_pred[minp]
  lpreds  <- length(preds)
  fr      <- glm(paste(response, "~", paste(preds, collapse = " + ")), l, family = binomial(link = 'logit'))
  mfs     <- blr_model_fit_stats(fr)
  aic     <- mfs$m_aic
  bic     <- mfs$m_bic
  dev     <- mfs$m_deviance
  # fr      <- ols_regress(paste(response, "~",
  #                              paste(preds, collapse = " + ")), l)
  # rsq     <- fr$rsq
  # adjrsq  <- fr$adjr
  # cp      <- ols_mallows_cp(fr$model, model)
  # aic     <- ols_aic(fr$model)
  # sbc     <- ols_sbc(fr$model)
  # sbic    <- ols_sbic(fr$model, model)
  # rmse    <- sqrt(fr$ems)
  # betas   <- append(betas, fr$betas)
  # lbetas  <- append(lbetas, length(fr$betas))
  # pvalues <- append(pvalues, fr$pvalues)

  if (details == TRUE) {
    cat("\n")
    cat(paste("Stepwise Selection: Step", step), "\n\n")
  }

  if (interactive()) {
    cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
  } else {
    cat(paste("-", dplyr::last(preds), "added"), "\n")
  }


  if (details == TRUE) {
    cat("\n")
    m <- blr_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
    print(m)
    cat("\n\n")
  }

  all_step  <- 1
  tech      <- c("addition", "removal")
  var_index <- preds
  method    <- tech[1]

  while (step < mlen_p) {

    all_pred <- all_pred[-minp]
    len_p    <- length(all_pred)
    step     <- step + 1
    ppos     <- ppos + length(minp)
    pvals    <- c()
    tvals    <- c()

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      m <- glm(paste(response, "~", paste(predictors, collapse = " + ")),
             l, family = binomial(link = 'logit'))
      m_sum <- Anova(m, test.statistic = "Wald")
      pvals[i] <- m_sum$`Pr(>Chisq)`[ppos]
      tvals[i] <- m_sum$Chisq[ppos]
      # m_sum <- summary(m)
      # pvals[i] <- unname(m_sum$coefficients[, 4])[ppos]
      # tvals[i] <- unname(m_sum$coefficients[, 3])[ppos]
      # m          <- ols_regress(paste(response, "~",
      #                                 paste(predictors, collapse = " + ")), l)
      # pvals[i]   <- m$pvalues[ppos]
      # tvals[i]   <- m$tvalues[ppos]
    }

    minp  <- which(pvals == min(pvals))
    # tvals <- abs(tvals)
    # maxt  <- which(tvals == max(tvals))

    if (pvals[minp] <= pent) {

      preds     <- c(preds, all_pred[minp])
      var_index <- c(var_index, all_pred[minp])
      method    <- c(method, tech[1])
      lpreds    <- length(preds)
      all_step  <- all_step + 1
      fr     <- glm(paste(response, "~", paste(preds, collapse = " + ")), l, family = binomial(link = 'logit'))
      mfs    <- blr_model_fit_stats(fr)
      aic    <- c(aic, mfs$m_aic)
      bic    <- c(bic, mfs$m_bic)
      dev    <- c(dev, mfs$m_deviance)
      # fr        <- ols_regress(paste(response, "~",
      #                                paste(preds, collapse = " + ")), l)
      # rsq       <- c(rsq, fr$rsq)
      # adjrsq    <- c(adjrsq, fr$adjr)
      # aic       <- c(aic, ols_aic(fr$model))
      # sbc       <- c(sbc, ols_sbc(fr$model))
      # sbic      <- c(sbic, ols_sbic(fr$model, model))
      # cp        <- c(cp, ols_mallows_cp(fr$model, model))
      # rmse      <- c(rmse, sqrt(fr$ems))
      # betas     <- append(betas, fr$betas)
      # lbetas    <- append(lbetas, length(fr$betas))
      # pvalues   <- append(pvalues, fr$pvalues)

      if (details == TRUE) {
        cat("\n")
        cat(paste("Stepwise Selection: Step", step), "\n\n")
      }

      if (interactive()) {
        cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
      } else {
        cat(paste("-", dplyr::last(preds), "added"), "\n")
      }


      if (details == TRUE) {
        cat("\n")
        m <- blr_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        print(m)
        cat("\n\n")
      }


      # if (details == TRUE) {
      #   cat("\n")
      #   m <- blr_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
      #   print(m)
      #   cat("\n\n")
      # }

      m2     <- glm(paste(response, "~", paste(preds, collapse = " + ")), l,
                  family = binomial(link = 'logit'))
      m_sum <- Anova(m2, test.statistic = "Wald")
      pvals_r <- m_sum$`Pr(>Chisq)`
      # tvals_r <- m_sum$Chisq[ppos]
      # tvals_r <- abs(unname(m_sum$coefficients[, 3])[-1])
      maxp    <- which(pvals_r == max(pvals_r))
      if (pvals_r[maxp] > prem) {

        var_index <- c(var_index, preds[maxp])
        lvar      <- length(var_index)
        method    <- c(method, tech[2])
        preds     <- preds[-maxp]
        all_step  <- all_step + 1
        ppos      <- ppos - length(maxp)
        fr        <- glm(paste(response, "~", paste(preds, collapse = " + ")), l, family = binomial(link = 'logit'))
        mfs       <- blr_model_fit_stats(fr)
        aic       <- c(aic, mfs$m_aic)
        bic       <- c(bic, mfs$m_bic)
        dev       <- c(dev, mfs$m_deviance)
        # rsq       <- c(rsq, fr$rsq)
        # adjrsq    <- c(adjrsq, fr$adjr)
        # aic       <- c(aic, ols_aic(fr$model))
        # sbc       <- c(sbc, ols_sbc(fr$model))
        # sbic      <- c(sbic, ols_sbic(fr$model, model))
        # cp        <- c(cp, ols_mallows_cp(fr$model, model))
        # rmse      <- c(rmse, sqrt(fr$ems))
        # betas     <- append(betas, fr$betas)
        # lbetas    <- append(lbetas, length(fr$betas))
        # pvalues   <- append(pvalues, fr$pvalues)

        if (details == TRUE) {
          cat("\n")
          cat(paste("Stepwise Selection: Step", all_step), "\n\n")
        }

        if (interactive()) {
          cat(crayon::red(clisymbols::symbol$cross), crayon::bold(dplyr::last(var_index)), "\n")
        } else {
          cat(paste("-", dplyr::last(var_index), "added"), "\n")
        }


        if (details == TRUE) {
          cat("\n")
          m <- blr_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
          print(m)
          cat("\n\n")
        }
      } else {
        preds <- preds
        all_step <- all_step
      }
    } else {
      cat("\n")
      cat(crayon::bold$red(paste0("No more variables to be added/removed.")))
      cat("\n")
      break
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

  final_model <- glm(paste(response, "~", paste(preds, collapse = " + ")),
    data = l, family = binomial(link = 'logit'))

  out <- list(
    orders     = var_index,
    method     = method,
    steps      = all_step,
    predictors = preds,
    aic        = aic,
    bic        = bic,
    dev        = dev,
    indvar     = cterms,
    model      = final_model
  )

  class(out) <- "blr_step_p_both"

  return(out)
}

#' @export
#'
print.blr_step_p_both <- function(x, ...) {
  if (x$steps > 0) {
    print_stepwise(x)
  } else {
    print("No variables have been added to or removed from the model.")
  }
}

#' @export
#' @rdname blr_step_p_both
#'
plot.blr_step_p_both <- function(x, model = NA, ...) {

  a <- NULL
  b <- NULL

  y <- seq_len(x$steps)

  d4 <- tibble(a = y, b = x$aic)
  d5 <- tibble(a = y, b = x$bic)
  d6 <- tibble(a = y, b = x$dev)

  p4 <- plot_stepwise(d4, "AIC")
  p5 <- plot_stepwise(d5, "BIC")
  p6 <- plot_stepwise(d6, "Deviance")

  # grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, top = "Stepwise Regression")
  myplots <- list(plot_4 = p4, plot_5 = p5, plot_6 = p6)
  result <- marrangeGrob(myplots, nrow = 2, ncol = 2)
  result


}

plot_stepwise <- function(d, title) {

  a <- NULL
  b <- NULL

  ggplot(d, aes(x = a, y = b)) +
    geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) +
    xlab("") + ylab("") + ggtitle(title) +
    theme(axis.ticks = element_blank())

}


