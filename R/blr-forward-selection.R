#' Stepwise AIC forward selection
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on chi square statistic, in a stepwise manner
#' until there is no variable left to enter any more.
#'
#' @param model An object of class \code{glm}.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param ... Other arguments.
#' @param x An object of class \code{blr_step_aic_forward}.
#' @param text_size size of the text in the plot.
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
blr_step_aic_forward <- function(model, details = FALSE, ...)
  UseMethod("blr_step_aic_forward")

#' @rdname blr_step_aic_forward
#' @export
#'
blr_step_aic_forward.default <- function(model, details = FALSE, ...) {
  
  blr_check_model(model)
  blr_check_logic(details)
  blr_check_npredictors(model, 3)

  response <-
    model %>%
    magrittr::use_series(model) %>%
    names() %>%
    magrittr::extract(1)

  l        <- mod_sel_data(model)
  nam      <- coeff_names(model)
  all_pred <- nam
  mlen_p   <- length(all_pred)
  preds    <- c()
  step     <- 1
  aics     <- c()
  bics     <- c()
  devs     <- c()

  mo <- stats::glm(
    paste(response, "~", 1), data = l,
    family = stats::binomial(link = "logit")
  )
  aic1 <- model_aic(mo)

  cat(format("Forward Selection Method", justify = "left", width = 24), "\n")
  cat(rep("-", 24), sep = "", "\n\n")
  cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
  for (i in seq_len(length(nam))) {
    cat(paste(i, ".", nam[i]), "\n")
  }
  cat("\n")

  if (details == TRUE) {
    cat(" Step 0: AIC =", aic1, "\n", paste(response, "~", 1, "\n\n"))
  }

  for (i in seq_len(mlen_p)) {
    predictors <- all_pred[i]
    k <- stats::glm(
      paste(response, "~", paste(predictors, collapse = " + ")),
      data = l, family = stats::binomial(link = "logit")
    )
    aics[i] <- model_aic(k)
    bics[i] <- model_bic(k)
    devs[i] <- model_deviance(k)
  }

  da <- data.frame(
    predictors = all_pred, aics = aics, bics = bics,
    devs = devs
  )
  da2 <- dplyr::arrange(da, aics)

  if (details == TRUE) {
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

  cat("\n")
  if (!details) {
    cat("Variables Entered:", "\n\n")
  }

  if (shiny::isRunning()) {
    cat(paste("-", dplyr::last(preds)), "\n")
  } else if (interactive()) {
    cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
  } else {
    cat(paste("-", dplyr::last(preds)), "\n")
  }


  while (step < mlen_p) {
    aics <- c()
    bics <- c()
    devs <- c()
    mo <- stats::glm(
      paste(response, "~", paste(preds, collapse = " + ")), data = l,
      family = stats::binomial(link = "logit")
    )
    aic1 <- model_aic(mo)

    if (details == TRUE) {
      cat("\n\n", "Step", step, ": AIC =", aic1, "\n", paste(response, "~", paste(preds, collapse = " + "), "\n\n"))
    }

    for (i in seq_len(len_p)) {
      predictors <- c(preds, all_pred[i])
      k <- stats::glm(
        paste(response, "~", paste(predictors, collapse = " + ")),
        data = l, family = stats::binomial(link = "logit")
      )
      aics[i] <- model_aic(k)
      bics[i] <- model_bic(k)
      devs[i] <- model_deviance(k)
    }

    if (details == TRUE) {
      da <- data.frame(
        predictors = all_pred, aics = aics, bics = bics,
        devs = devs
      )

      da2 <- dplyr::arrange(da, aics)
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

     
      if (shiny::isRunning()) {
        cat(paste("-", dplyr::last(preds)), "\n")
      } else if (interactive()) {
        cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
      } else {
        cat(paste("-", dplyr::last(preds)), "\n")
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
      if (shiny::isRunning()) {
        cat(paste("-", preds[i]), "\n")
      } else if (interactive()) {
        cat(crayon::green(clisymbols::symbol$tick), crayon::bold(preds[i]), "\n")
      } else {
        cat(paste("-", preds[i]), "\n")
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
  }

  final_model <- stats::glm(paste(response, "~", paste(preds, collapse = " + ")), 
    data = l, family = stats::binomial(link = 'logit'))

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

#' @rdname blr_step_aic_forward
#' @export
#'
plot.blr_step_aic_forward <- function(x, text_size = 3,  ...) {

  aic <- NULL
  tx  <- NULL
  a   <- NULL
  b   <- NULL

  y    <- seq_len(x$steps)
  xloc <- y - 0.1
  yloc <- x$aics - 0.2
  xmin <- min(y) - 1
  xmax <- max(y) + 1

  ymin <-
    x %>%
    magrittr::use_series(aic) %>%
    min() %>%
    magrittr::subtract(1)

  ymax <-
    x %>%
    magrittr::use_series(aic) %>%
    max() %>%
    magrittr::add(1)

  predictors <- x$predictors

  d2 <- tibble::tibble(x = xloc, y = yloc, tx = predictors)
  d  <- tibble::tibble(a = y, b = x$aics)

  p <- 
    ggplot2::ggplot(d, ggplot2::aes(x = a, y = b)) + 
    ggplot2::geom_line(color = "blue") +
    ggplot2::geom_point(color = "blue", shape = 1, size = 2) + 
    ggplot2::xlim(c(xmin, xmax)) +
    ggplot2::ylim(c(ymin, ymax)) + 
    ggplot2::xlab("Step") + 
    ggplot2::ylab("AIC") +
    ggplot2::ggtitle("Stepwise AIC Forward Selection") +
    ggplot2::geom_text(data = d2, ggplot2::aes(x = x, y = y, label = tx), 
      size = text_size, hjust = 0, nudge_x = 0.1)

  print(p)
}
