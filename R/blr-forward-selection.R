#' Stepwise AIC forward selection
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on chi square statistic, in a stepwise manner
#' until there is no variable left to enter any more.
#'
#' @param model An object of class \code{glm}.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param ... Other arguments.
#' @param x An object of class \code{blr_step_aic_forward}.
#' @param text_size size of the text in the plot.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @return \code{blr_step_aic_forward} returns an object of class
#' \code{"blr_step_aic_forward"}. An object of class
#' \code{"blr_step_aic_forward"} is a list containing the following components:
#'
#' \item{model}{model with the least AIC; an object of class \code{glm}}
#' \item{candidates}{candidate predictor variables}
#' \item{steps}{total number of steps}
#' \item{predictors}{variables entered into the model}
#' \item{aics}{akaike information criteria}
#' \item{bics}{bayesian information criteria}
#' \item{devs}{deviances}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @examples
#' \dontrun{
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' # selection summary
#' blr_step_aic_forward(model)
#'
#' # print details of each step
#' blr_step_aic_forward(model, details = TRUE)
#'
#' # plot
#' plot(blr_step_aic_forward(model))
#'
#' # final model
#' k <- blr_step_aic_forward(model)
#' k$model
#'
#' }
#'
#' @family variable selection procedures
#'
#' @export
#'
blr_step_aic_forward <- function(model, ...) UseMethod("blr_step_aic_forward")

#' @rdname blr_step_aic_forward
#' @export
#'
blr_step_aic_forward.default <- function(model, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- TRUE
  }

  blr_check_model(model)
  blr_check_logic(details)
  blr_check_npredictors(model, 3)

  response <- names(model$model)[1]
  l        <- model$model
  nam      <- coeff_names(model)
  all_pred <- nam
  mlen_p   <- length(all_pred)
  preds    <- c()
  step     <- 1
  aics     <- c()
  bics     <- c()
  devs     <- c()

  mo <- glm(
    paste(response, "~", 1), data = l,
    family = binomial(link = "logit")
  )
  aic1 <- model_aic(mo)

  if (progress) {
    cat(format("Forward Selection Method", justify = "left", width = 24), "\n")
    cat(rep("-", 24), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(nam))) {
      cat(paste(i, ".", nam[i]), "\n")
    }
    cat("\n")
  }

  if (details) {
    cat(" Step 0: AIC =", aic1, "\n", paste(response, "~", 1, "\n\n"))
  }

  for (i in seq_len(mlen_p)) {
    predictors <- all_pred[i]
    k <- glm(
      paste(response, "~", paste(predictors, collapse = " + ")),
      data = l, family = binomial(link = "logit")
    )
    aics[i] <- model_aic(k)
    bics[i] <- model_bic(k)
    devs[i] <- model_deviance(k)
  }

  da <- data.frame(
    predictors = all_pred, aics = aics, bics = bics,
    devs = devs
  )
  da2 <- da[order(da[['aics']]), ]

  if (details) {
    w1 <- max(nchar("Predictor"), nchar(as.character(da2$predictors)))
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
        fl(da2[i, 1], w1), fs(), fg(1, w2), fs(),
        fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
        fg(format(round(da2[i, 3], 3), nsmall = 3), w4), fs(),
        fg(format(round(da2[i, 4], 3), nsmall = 3), w5), "\n"
      )
    }

    cat(rep("-", w), sep = "", "\n\n")
  }

  minc     <- which(aics == min(aics))
  laic     <- aics[minc]
  lbic     <- bics[minc]
  ldev     <- devs[minc]
  preds    <- all_pred[minc]
  lpreds   <- length(preds)
  all_pred <- all_pred[-minc]
  len_p    <- length(all_pred)
  step     <- 1

  if (progress) {
    cat("\n")
    if (!details) {
      cat("Variables Entered:", "\n\n")
    }
  }

  if (progress) {
    cat(paste("+", rev(preds)[1]), "\n")
  }


  while (step < mlen_p) {
    aics <- c()
    bics <- c()
    devs <- c()
    mo <- glm(
      paste(response, "~", paste(preds, collapse = " + ")), data = l,
      family = binomial(link = "logit")
    )
    aic1 <- model_aic(mo)

    if (details) {
      cat("\n\n", "Step", step, ": AIC =", aic1, "\n", paste(response, "~", paste(preds, collapse = " + "), "\n\n"))
    }

    for (i in seq_len(len_p)) {
      predictors <- c(preds, all_pred[i])
      k <- glm(
        paste(response, "~", paste(predictors, collapse = " + ")),
        data = l, family = binomial(link = "logit")
      )
      aics[i] <- model_aic(k)
      bics[i] <- model_bic(k)
      devs[i] <- model_deviance(k)
    }

    if (details) {
      da <- data.frame(
        predictors = all_pred, aics = aics, bics = bics,
        devs = devs
      )

      da2 <- da[order(da[['aics']]), ]
      w1  <- max(nchar("Predictor"), nchar(as.character(da2$predictors)))
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
          fl(da2[i, 1], w1), fs(), fg(1, w2), fs(),
          fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
          fg(format(round(da2[i, 3], 3), nsmall = 3), w4), fs(),
          fg(format(round(da2[i, 4], 3), nsmall = 3), w5), "\n"
        )
      }

      cat(rep("-", w), sep = "", "\n\n")
    }

    minaic <- which(aics == min(aics))

    if (aics[minaic] < laic[lpreds]) {
      preds    <- c(preds, all_pred[minaic])
      minc     <- aics[minaic]
      laic     <- c(laic, minc)
      lbic     <- c(lbic, minc)
      ldev     <- c(ldev, minc)
      lpreds   <- length(preds)
      all_pred <- all_pred[-minaic]
      len_p    <- length(all_pred)
      step     <- step + 1


      if (progress) {
        cat(paste("+", rev(preds)[1]), "\n")
      }
    } else {
      if (progress) {
        cat("\n")
        cat("No more variables to be added.")
      }
      break
    }
  }

  if (details) {
    cat("\n\n")
    cat("Variables Entered:", "\n\n")
    for (i in seq_len(length(preds))) {
      cat(paste("+", preds[i]), "\n")
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
    predictors = preds,
    aics       = laic,
    bics       = lbic,
    devs       = ldev,
    model      = final_model
  )

  class(out) <- "blr_step_aic_forward"

  return(out)
}


#' @export
#'
print.blr_step_aic_forward <- function(x, ...) {
  if (x$steps > 0) {
    print_forward_selection(x)
  } else {
    print("No variables have been added to the model.")
  }
}


#' @importFrom ggplot2 xlim ylim
#' @rdname blr_step_aic_forward
#' @export
#'
plot.blr_step_aic_forward <- function(x, text_size = 3, print_plot = TRUE, ...) {

  aic <- NULL
  tx  <- NULL
  a   <- NULL
  b   <- NULL

  y    <- seq_len(x$steps)
  xloc <- y - 0.1
  yloc <- x$aics - 0.2
  xmin <- min(y) - 1
  xmax <- max(y) + 1
  ymin <- min(x$aic) -1
  ymax <- max(x$aic) + 1

  predictors <- x$predictors

  d2 <- data.frame(x = xloc, y = yloc, tx = predictors)
  d  <- data.frame(a = y, b = x$aics)

  p <-
    ggplot(d, aes(x = a, y = b)) + geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + xlab("Step") + ylab("AIC") +
    ggtitle("Stepwise AIC Forward Selection") +
    geom_text(data = d2, aes(x = x, y = y, label = tx),
      size = text_size, hjust = 0, nudge_x = 0.1)

  if (print_plot) {
    print(p)
  }

  invisible(p)
}
