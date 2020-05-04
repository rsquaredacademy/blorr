#' Stepwise AIC backward elimination
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on akaike information criterion, in a stepwise
#' manner until there is no variable left to remove any more.
#'
#' @param model An object of class \code{glm}; the model should include all
#'   candidate predictor variables.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param ... Other arguments.
#' @param x An object of class \code{blr_step_aic_backward}.
#' @param text_size size of the text in the plot.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @return \code{blr_step_aic_backward} returns an object of class
#' \code{"blr_step_aic_backward"}. An object of class
#' \code{"blr_step_aic_backward"} is a list containing the following components:
#'
#' \item{model}{model with the least AIC; an object of class \code{glm}}
#' \item{candidates}{candidate predictor variables}
#' \item{steps}{total number of steps}
#' \item{predictors}{variables removed from the model}
#' \item{aics}{akaike information criteria}
#' \item{bics}{bayesian information criteria}
#' \item{devs}{deviances}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @examples
#' \dontrun{
#' model <- glm(honcomp ~ female + read + science + math + prog + socst,
#' data = hsb2, family = binomial(link = 'logit'))
#'
#' # elimination summary
#' blr_step_aic_backward(model)
#'
#' # print details of each step
#' blr_step_aic_backward(model, details = TRUE)
#'
#' # plot
#' plot(blr_step_aic_backward(model))
#'
#' # final model
#' k <- blr_step_aic_backward(model)
#' k$model
#'
#' }
#'
#' @family variable selection procedures
#'
#' @export
#'
blr_step_aic_backward <- function(model, ...) UseMethod("blr_step_aic_backward")


#' @export
#' @rdname blr_step_aic_backward
#'
blr_step_aic_backward.default <- function(model, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- TRUE
  }

  blr_check_model(model)
  blr_check_logic(details)
  blr_check_npredictors(model, 3)

  response <- names(model$model)[1]
  l        <- model$model
  nam      <- coeff_names(model)
  preds    <- nam
  aic_f    <- model_aic(model)

    mi <- glm(
    paste(response, "~", paste(preds, collapse = " + ")),
    data = l, family = binomial(link = "logit")
  )

  laic <- aic_f
  lbic <- model_bic(mi)
  ldev <- model_deviance(mi)

  if (progress) {
    cat(format("Backward Elimination Method", justify = "left", width = 27), "\n")
    cat(rep("-", 27), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(nam))) {
      cat(paste(i, ".", nam[i]), "\n")
    }
    cat("\n")
  }

  if (details) {
    cat(" Step 0: AIC =", aic_f, "\n", paste(response, "~", paste(preds, collapse = " + "), "\n\n"))
  }

  ilp   <- length(preds)
  end   <- FALSE
  step  <- 0
  rpred <- c()
  aics  <- c()
  bics  <- c()
  devs  <- c()

  for (i in seq_len(ilp)) {
    predictors <- preds[-i]
    m <- glm(
      paste(response, "~", paste(predictors, collapse = " + ")),
      data = l, family = binomial(link = "logit")
    )
    aics[i] <- model_aic(m)
    bics[i] <- model_bic(m)
    devs[i] <- model_deviance(m)
  }

  da  <- data.frame(predictors = preds, aics = aics, bics = bics, devs = devs)
  # da2 <- arrange(da, aics)
  da2 <- da[order(da[['aics']]), ]

  if (details == TRUE) {
    w1 <- max(nchar("Predictor"), nchar(predictors))
    w2 <- 2
    w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
    w4 <- max(nchar("BIC"), nchar(format(round(bics, 3), nsmall = 3)))
    w5 <- max(nchar("Deviance"), nchar(format(round(devs, 3), nsmall = 3)))
    w  <- sum(w1, w2, w3, w4, w5, 16)
    ln <- length(aics)

    cat(rep("-", w), sep = "", "\n")
    cat(
      fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
      fc("BIC", w4), fs(), fc("Deviance", w5), "\n"
    )
    cat(rep("-", w), sep = "", "\n")

    for (i in seq_len(ln)) {
      cat(
        fl(da2[i, 1], w1), fs(), fc(1, w2), fs(),
        fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
        fg(format(round(da2[i, 3], 3), nsmall = 3), w4), fs(),
        fg(format(round(da2[i, 4], 3), nsmall = 3), w5), "\n"
      )
    }

    cat(rep("-", w), sep = "", "\n\n")
  }

  if (progress) {
    cat("\n")
    if (!details) {
      cat("Variables Removed:", "\n\n")
    }
  }

  while (!end) {
    minc <- which(aics == min(aics))

    if (aics[minc] < aic_f) {
      rpred <- c(rpred, preds[minc])
      preds <- preds[-minc]
      ilp   <- length(preds)
      step  <- step + 1
      aic_f <- aics[minc]

      mi <- glm(
        paste(response, "~", paste(preds, collapse = " + ")),
        data = l, family = binomial(link = "logit")
      )

      laic <- c(laic, aic_f)
      lbic <- c(lbic, model_bic(mi))
      ldev <- c(ldev, model_deviance(mi))
      aics <- c()
      bics <- c()
      devs <- c()

      if (progress) {
        cat(paste("x", rev(rpred)[1], "\n"))
      }

      for (i in seq_len(ilp)) {
        predictors <- preds[-i]
        m <- glm(
          paste(response, "~", paste(predictors, collapse = " + ")),
          data = l, family = binomial(link = "logit")
        )
        aics[i] <- model_aic(m)
        bics[i] <- model_bic(m)
        devs[i] <- model_deviance(m)
      }


      if (details) {
        cat("\n\n", " Step", step, ": AIC =", aic_f, "\n", paste(response, "~", paste(preds, collapse = " + "), "\n\n"))


        da <- data.frame(
          predictors = preds, aics = aics, bics = bics,
          devs = devs
        )
        # da2 <- arrange(da, aics)
        da2 <- da[order(da[['aics']]), ]
        w1  <- max(nchar("Predictor"), nchar(predictors))
        w2  <- 2
        w3  <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
        w4  <- max(nchar("BIC"), nchar(format(round(bics, 3), nsmall = 3)))
        w5  <- max(nchar("Deviance"), nchar(format(round(devs, 3), nsmall = 3)))
        w   <- sum(w1, w2, w3, w4, w5, 16)
        ln  <- length(aics)

        cat(rep("-", w), sep = "", "\n")
        cat(
          fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
          fc("BIC", w4), fs(), fc("Deviance", w5), "\n"
        )
        cat(rep("-", w), sep = "", "\n")

        for (i in seq_len(ln)) {
          cat(
            fl(da2[i, 1], w1), fs(), fc(1, w2), fs(),
            fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
            fg(format(round(da2[i, 3], 3), nsmall = 3), w4), fs(),
            fg(format(round(da2[i, 4], 3), nsmall = 3), w5), "\n"
          )
        }

        cat(rep("-", w), sep = "", "\n\n")
      }
    } else {
      end <- TRUE

      if (details) {
        cat("No more variables to be removed.")
      }
    }
  }

  if (details) {
    cat("\n\n")
    cat("Variables Removed:", "\n\n")
    for (i in seq_len(length(rpred))) {
      cat(paste("-", rpred[i]), "\n")
    }
  }

  if (progress) {
    cat("\n\n")
    cat("Final Model Output", "\n")
    cat(rep("-", 18), sep = "", "\n\n")

    fi <- blr_regress(
      paste(response, "~", paste(preds, collapse = " + ")),
      data = l
    )
    print(fi)
  }

  final_model <- glm(paste(response, "~", paste(preds, collapse = " + ")),
    data = l, family = binomial(link = 'logit'))

  out <- list(
    candidates = nam,
    steps      = step,
    predictors = rpred,
    aics       = laic,
    bics       = lbic,
    devs       = ldev,
    model      = final_model
  )

  class(out) <- "blr_step_aic_backward"

  return(out)
}


#' @export
#'
print.blr_step_aic_backward <- function(x, ...) {
  if (x$steps > 0) {
    print_backward_elimination(x)
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname blr_step_aic_backward
#' @export
#'
plot.blr_step_aic_backward <- function(x, text_size = 3, print_plot = TRUE, ...) {

  steps <- NULL
  aics  <- NULL
  tx    <- NULL
  a     <- NULL
  b     <- NULL

  y    <- c(0, seq_len(x$steps))
  xloc <- y - 0.1
  yloc <- x$aics - 0.2
  xmin <- min(y) - 0.4
  xmax <- max(y) + 1
  ymin <- min(x$aics) - 1
  ymax <- max(x$aics) + 1


  predictors <- c("Full Model", x$predictors)

  d2 <- data.frame(x = xloc, y = yloc, tx = predictors)
  d  <- data.frame(a = y, b = x$aics)

  p <-
    ggplot(d, aes(x = a, y = b)) + geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + xlab("Step") + ylab("AIC") +
    ggtitle("Stepwise AIC Backward Elimination") +
    geom_text(data = d2, aes(x = x, y = y, label = tx),
              size = text_size, hjust = 0, nudge_x = 0.1)

  if (print_plot) {
    print(p)
  }

  invisible(p)

}


#' Coefficient names
#'
#' Returns the names of the coefficients including
#'   interaction variables.
#'
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
coeff_names <- function(model) {
  colnames(attr(model$terms, which = "factors"))
}
