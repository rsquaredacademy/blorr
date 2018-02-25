#' @title Backward Elimination
#' @description Build regression model from a set of candidate predictor variables by removing predictors based on
#' Akaike Information Criteria, in a stepwise manner until there is no variable left to remove any more.
#' @param model an object of class \code{glm}; the model should include all candidate predictor variables
#' @param details logical; if \code{TRUE}, will print the regression result at each step
#' @param ... other arguments
#' @param x An object of class \code{blr_backward_elimination}.
#' @return \code{blr_backward_elimination} returns an object of class
#' \code{"blr_backward_elimination"}. An object of class
#' \code{"blr_backward_elimination"} is a list containing the following components:
#'
#' \item{candidates}{candidate predictor variables}
#' \item{steps}{total number of steps}
#' \item{predictors}{variables removed from the model}
#' \item{aics}{akaike information criteria}
#' \item{bics}{bayesian information criteria}
#' \item{devs}{deviances}
#'
#' @references Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#' @examples
#' \dontrun{
#' model <- glm(honcomp ~ female + read + science + math + prog + socst,
#' data = blorr::hsb2, family = binomial(link = 'logit'))
#'
#' # elimination summary
#' blr_backward_elimination(model)
#'
#' # print details of each step
#' blr_backward_elimination(model, details = TRUE)
#'
#' # plot
#' plot(blr_backward_elimination(model))
#' }
#'
#' @export
#'
blr_backward_elimination <- function(model, details = FALSE, ...) UseMethod("blr_backward_elimination")


#' @export
#' @rdname blr_backward_elimination
#'
blr_backward_elimination.default <- function(model, details = FALSE, ...) {
  if (!any(class(model) == "glm")) {
    stop("Please specify a binary logistic regression model.", call. = FALSE)
  }

  if (!is.logical(details)) {
    stop("details must be either TRUE or FALSE", call. = FALSE)
  }

  if (length(model$coefficients) < 3) {
    stop("Please specify a model with at least 2 predictors.", call. = FALSE)
  }

  response <-
    model %>%
    use_series(model) %>%
    names() %>%
    extract(1)

  l     <- mod_sel_data(model)
  nam   <- coeff_names(model)
  preds <- nam
  aic_f <- model_aic(model)

    mi <- glm(
    paste(response, "~", paste(preds, collapse = " + ")),
    data = l, family = binomial(link = "logit")
  )

  laic <- aic_f
  lbic <- model_bic(mi)
  ldev <- model_deviance(mi)

  cat(format("Backward Elimination Method", justify = "left", width = 27), "\n")
  cat(rep("-", 27), sep = "", "\n\n")
  cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
  for (i in seq_len(length(nam))) {
    cat(paste(i, ".", nam[i]), "\n")
  }
  cat("\n")

  if (details == TRUE) {
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

  da <- data.frame(predictors = preds, aics = aics, bics = bics, devs = devs)
  da2 <- arrange(da, aics)

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

  cat("\n")
  if (!details) {
    cat("Variables Removed:", "\n\n")
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

      if (interactive()) {
        cat(crayon::red(clisymbols::symbol$cross), crayon::bold(dplyr::last(rpred)), "\n")
      } else {
        cat(paste("-", dplyr::last(rpred)), "\n")
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


      if (details == TRUE) {
        cat("\n\n", " Step", step, ": AIC =", aic_f, "\n", paste(response, "~", paste(preds, collapse = " + "), "\n\n"))


        da <- data.frame(
          predictors = preds, aics = aics, bics = bics,
          devs = devs
        )
        da2 <- arrange(da, aics)
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

      if (details == TRUE) {
        cat(crayon::bold$red("No more variables to be removed."))
      }
    }
  }

  if (details == TRUE) {
    cat("\n\n")
    cat("Variables Removed:", "\n\n")
    for (i in seq_len(length(rpred))) {
      if (interactive()) {
        cat(crayon::red(clisymbols::symbol$cross), crayon::bold(rpred[i]), "\n")
      } else {
        cat(paste("-", rpred[i]), "\n")
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

  out <- list(
    candidates = nam,
    steps      = step,
    predictors = rpred,
    aics       = laic,
    bics       = lbic,
    devs       = ldev
  )

  class(out) <- "blr_backward_elimination"

  return(out)
}


#' @export
#'
print.blr_backward_elimination <- function(x, ...) {
  if (x$steps > 0) {
    print_backward_elimination(x)
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname blr_backward_elimination
#' @export
#'
plot.blr_backward_elimination <- function(x, ...) {

  steps <- NULL
  aics  <- NULL
  tx    <- NULL
  a     <- NULL
  b     <- NULL

  y <-
    x %>%
    use_series(steps) %>%
    seq_len() %>%
    prepend(0)

  xloc <- y - 0.1

  yloc <-
    x %>%
    use_series(aics) %>%
    subtract(0.2)

  xmin <-
    y %>%
    min() %>%
    subtract(0.4)

  xmax <-
    y %>%
    max() %>%
    add(1)

  ymin <-
    x %>%
    use_series(aics) %>%
    min() %>%
    add(1)

  ymax <-
    x %>%
    use_series(aics) %>%
    max() %>%
    add(1)

  predictors <- c("Full Model", x$predictors)

  d2 <- tibble(x = xloc, y = yloc, tx = predictors)
  d  <- tibble(a = y, b = x$aics)

  p <- ggplot(d, aes(x = a, y = b)) + geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + xlab("Step") + ylab("AIC") +
    ggtitle("Stepwise AIC Backward Elimination") +
    geom_text(data = d2, aes(x = x, y = y, label = tx),
              hjust = 0, nudge_x = 0.1)

  print(p)

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

  model %>%
    use_series(terms) %>%
    attr(which = "factors") %>%
    colnames()

}
