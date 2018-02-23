#' @importFrom ggplot2 geom_hline
#' @importFrom stats residuals rstandard hatvalues
#' @title Residual vs Fitted Values Plot
#' @description Residual vs fitted values plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param line_color color of the horizontal line
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_residual_fitted(model)
#'
#' @export
#'
blr_plot_residual_fitted <- function(model, point_color = "blue", line_color = "red",
                                     title = "Standardized Pearson Residual vs Fitted Values",
                                     xaxis_title = "Fitted Values",
                                     yaxis_title = "Standardized Pearson Residual") {
  fit_val <- fitted(model)
  res_val <- rstandard(model, type = "pearson")

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title) +
    geom_hline(yintercept = 0, color = line_color)

}

#' @title Residual Values Plot
#' @description Standardised pearson residuals plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_pearson_residual(model)
#'
#' @export
#'
blr_plot_pearson_residual <- function(model, point_color = "blue",
                                      title = "Standardized Pearson Residuals",
                                      xaxis_title = "id",
                                      yaxis_title = "Standardized Pearson Residuals") {

  res_val <- rstandard(model, type = "pearson")
  id      <- plot_id(res_val)
  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' @title Deviance vs Fitted Values Plot
#' @description Deviance vs fitted values plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param line_color color of the horizontal line
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_deviance_fitted(model)
#'
#' @export
#'
blr_plot_deviance_fitted <- function(model, point_color = "blue", line_color = "red",
                                     title = "Deviance Residual vs Fitted Values",
                                     xaxis_title = "Fitted Values",
                                     yaxis_title = "Deviance Residual") {

  fit_val <- fitted(model)
  res_val <- rstandard(model)

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title) +
    geom_hline(yintercept = 0, color = line_color)
}

#' @title Deviance Residual Values
#' @description Deviance residuals plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_deviance_residual(model)
#'
#' @export
#'
blr_plot_deviance_residual <- function(model, point_color = "blue",
                                       title = "Deviance Residuals Plot",
                                       xaxis_title = "id",
                                       yaxis_title = "Deviance Residuals") {
  res_val <- rstandard(model)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' @title Leverage vs Fitted Values Plot
#' @description Leverage vs fitted values plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_leverage_fitted(model)
#'
#' @export
#'
blr_plot_leverage_fitted <- function(model, point_color = "blue",
                                     title = "Leverage vs Fitted Values",
                                     xaxis_title = "Fitted Values",
                                     yaxis_title = "Leverage") {

  fit_val <- fitted(model)
  res_val <- hatvalues(model)

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' @title Leverage Plot
#' @description Leverage plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_leverage(model)
#'
#' @export
#'
blr_plot_leverage <- function(model, point_color = "blue",
                              title = "Leverage Plot",
                              xaxis_title = "id",
                              yaxis_title = "Leverage") {

  res_val <- hatvalues(model)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' @title Residual Diagnostics
#' @description Diagnostics for confidence interval displacement and
#' detecting ill fitted observations
#' @param model an object of class \code{glm}
#' @return C, CBAR, DIFDEV and DIFCHISQ
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_residual_diagnostics(model)
#'
#' @export
#'
blr_residual_diagnostics <- function(model) {

  res_val <-
    model %>%
    residuals(type = "pearson") %>%
    raise_to_power(2)

  hat_val  <- hatvalues(model)
  num      <- res_val * hat_val
  den      <- 1 - hat_val
  c        <- num / (den ^ 2)
  cbar     <- num / den
  difchisq <- cbar / hat_val

  difdev <-
    model %>%
    rstandard() %>%
    raise_to_power(2) %>%
    add(cbar)

  tibble(c = c, cbar = cbar, difdev = difdev, difchisq = difchisq)

}


#' @title CI Displacement C Plot
#' @description Confidence interval displacement diagnostics C plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_diag_c(model)
#'
#' @export
#'
blr_plot_diag_c <- function(model, point_color = "blue",
                            title = "CI Displacement C Plot",
                            xaxis_title = "id",
                            yaxis_title = "CI Displacement C") {

  res_val <- extract_diag(model, c)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' @title CI Displacement CBAR Plot
#' @description Confidence interval displacement diagnostics CBAR plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_diag_cbar(model)
#'
#' @export
#'
blr_plot_diag_cbar <- function(model, point_color = "blue",
                               title = "CI Displacement CBAR Plot",
                               xaxis_title = "id",
                               yaxis_title = "CI Displacement CBAR") {

  res_val <- extract_diag(model, cbar)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' @title Delta Chisquare Plot
#' @description Diagnostics for detecting ill fitted observations
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_diag_difchisq(model)
#'
#' @export
#'
blr_plot_diag_difchisq <- function(model, point_color = "blue",
                                   title = "Delta Chisquare Plot",
                                   xaxis_title = "id",
                                   yaxis_title = "Delta Chisquare") {

  res_val <- extract_diag(model,difchisq)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' @title Delta Deviance Plot
#' @description Diagnostics for detecting ill fitted observations
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_diag_difdev(model)
#'
#' @export
#'
blr_plot_diag_difdev <- function(model, point_color = "blue",
                                 title = "Delta Deviance Plot",
                                 xaxis_title = "id",
                                 yaxis_title = "Delta Deviance") {

  res_val <- extract_diag(model, difdev)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' @importFrom stats dfbetas
#' @importFrom ggplot2 geom_linerange geom_text annotate
#' @importFrom gridExtra grid.arrange
#' @title DFBETAs Panel
#' @description Panel of plots to detect influential observations using DFBETAs.
#' @param model an object of class \code{glm}
#' @details DFBETA measures the difference in each parameter estimate with and without the
#' influential point. There is a DFBETA for each data point i.e if there are n observations
#' and k variables, there will be \eqn{n * k} DFBETAs. In general, large values of DFBETAS indicate
#' observations that are influential in estimating a given parameter. Belsley, Kuh, and Welsch recommend
#' 2 as a general cutoff value to indicate influential observations and \eqn{2/\sqrt(n)} as a size-adjusted cutoff.
#' @return list; \code{blr_dfbetas_panel} returns a list of tibbles (for intercept and each predictor)
#' with the observation number and DFBETA of observations that exceed the threshold for classifying
#' an observation as an outlier/influential observation.
#' @references Belsley, David A.; Kuh, Edwin; Welsh, Roy E. (1980). Regression
#' Diagnostics: Identifying Influential Data and Sources of Collinearity.
#' Wiley Series in Probability and Mathematical Statistics.
#' New York: John Wiley & Sons. pp. ISBN 0-471-05856-4.
#' @examples
#' \dontrun{
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_dfbetas_panel(model)
#' }
#' @export
#'
blr_plot_dfbetas_panel <- function(model) {

  if (!any(class(model) == "glm")) {
    stop("Please specify a binary logistic regression model.", call. = FALSE)
  }

  dfb       <- dfbetas(model)
  n         <- nrow(dfb)
  np        <- ncol(dfb)
  threshold <- 2 / sqrt(n)
  myplots   <- list()
  outliers  <- list()

  for (i in seq_len(np)) {

    d <- dfbetas_data_prep(dfb, n, threshold, i)
    f <- dfbetas_outlier_data(d)
    p <- eval(substitute(dfbetas_plot(d, threshold, dfb, i),list(i = i)))

    myplots[[i]]  <- p
    outliers[[i]] <- f

  }

  suppressWarnings(do.call(grid.arrange, c(myplots, list(ncol = 2))))
  names(outliers) <- model_coeff_names(model)
  result <- list(outliers = outliers, plots = myplots)
  invisible(result)

}

#' @title CI Displacement C vs Fitted Values Plot
#' @description Confidence interval displacement diagnostics C vs fitted values plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_c_fitted(model)
#'
#' @export
#'
blr_plot_c_fitted <- function(model, point_color = "blue",
                              title = "CI Displacement C vs Fitted Values Plot",
                              xaxis_title = "Fitted Values",
                              yaxis_title = "CI Displacement C") {

  res_val <- extract_diag(model, c)
  fit_val <- fitted(model)

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' @title Delta Chi Square vs Fitted Values Plot
#' @description Delta Chi Square vs fitted values plot for detecting ill fitted observations
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_difchisq_fitted(model)
#'
#' @export
#'
blr_plot_difchisq_fitted <- function(model, point_color = "blue",
                                     title = "Delta Chi Square vs Fitted Values Plot",
                                     xaxis_title = "Fitted Values",
                                     yaxis_title = "Delta Chi Square") {

  res_val <- extract_diag(model, difchisq)
  fit_val <- fitted(model)

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' @title Delta Deviance vs Fitted Values Plot
#' @description Delta deviance vs fitted values plot for detecting ill fitted observations
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_difdev_fitted(model)
#'
#' @export
#'
blr_plot_difdev_fitted <- function(model, point_color = "blue",
                                   title = "Delta Deviance vs Fitted Values Plot",
                                   xaxis_title = "Fitted Values",
                                   yaxis_title = "Delta Deviance") {

  res_val <- extract_diag(model, difdev)
  fit_val <- fitted(model)

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' @title Delta Deviance vs Leverage Plot
#' @description Delta deviance vs leverage plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_difdev_leverage(model)
#'
#' @export
#'
blr_plot_difdev_leverage <- function(model, point_color = "blue",
                                     title = "Delta Deviance vs Leverage Plot",
                                     xaxis_title = "Leverage",
                                     yaxis_title = "Delta Deviance") {

  res_val <- extract_diag(model, difdev)
  hat_val <- hatvalues(model)

  create_plot(hat_val, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' @title Delta Chi Square vs Leverage Plot
#' @description Delta chi square vs leverage plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_difchisq_leverage(model)
#'
#' @export
#'
blr_plot_difchisq_leverage <- function(model, point_color = "blue",
                                       title = "Delta Chi Square vs Leverage Plot",
                                       xaxis_title = "Leverage",
                                       yaxis_title = "Delta Chi Square") {

  res_val <- extract_diag(model, difchisq)
  hat_val <- hatvalues(model)

  create_plot(hat_val, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' @title CI Displacement C vs Leverage Plot
#' @description Confidence interval displacement diagnostics C vs leverage plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_c_leverage(model)
#'
#' @export
#'
blr_plot_c_leverage <- function(model, point_color = "blue",
                                title = "CI Displacement C vs Leverage Plot",
                                xaxis_title = "Leverage",
                                yaxis_title = "CI Displacement C") {

  res_val <- extract_diag(model, c)
  hat_val <- hatvalues(model)

  create_plot(hat_val, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' @title Fitted Values vs Leverage Plot
#' @description Fitted values vs leverage plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_fitted_leverage(model)
#'
#' @export
#'
blr_plot_fitted_leverage <- function(model, point_color = "blue",
                                     title = "Fitted Values vs Leverage Plot",
                                     xaxis_title = "Leverage",
                                     yaxis_title = "Fitted Values") {

  fit_val <- fitted(model)
  hat_val <- hatvalues(model)

  create_plot(hat_val, fit_val, point_color, title, xaxis_title, yaxis_title)


}

plot_id <- function(res_val) {

  res_val %>%
    length() %>%
    seq_len()

}


extract_diag <- function(model, value) {

  vals <- enquo(value)

  model %>%
    blr_residual_diagnostics() %>%
    pull(!! vals)

}


dfbetas_data_prep <- function(dfb, n, threshold, i) {

  dbetas <- dfb[, i]
  tibble(obs = seq_len(n), dbetas = dbetas) %>%
    mutate(
      color = ifelse(((dbetas >= threshold) | (dbetas <= -threshold)),
                      c("outlier"), c("normal")),
      fct_color = color %>%
        factor() %>%
        ordered(levels = c("normal", "outlier")),
      txt = ifelse(color == "outlier", obs, NA)
    )

}

dfbetas_plot <- function(d, threshold, dfb, i) {

  ggplot(d, aes(x = obs, y = dbetas, label = txt, ymin = 0, ymax = dbetas)) +
    geom_linerange(colour = "blue") +
    geom_hline(yintercept = c(0, threshold, -threshold), colour = "red") +
    geom_point(colour = "blue", shape = 1) +
    xlab("Observation") + ylab("DFBETAS") +
    ggtitle(paste("Influence Diagnostics for", colnames(dfb)[i])) +
    geom_text(hjust = -0.2, nudge_x = 0.15, size = 2, family = "serif",
              fontface = "italic", colour = "darkred", na.rm = TRUE) +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.5, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = paste("Threshold:", round(threshold, 2))
    )
}


dfbetas_outlier_data <- function(d) {

  d %>%
    filter(color == "outlier") %>%
    select(obs, dbetas)
}


model_coeff_names <- function(model) {

  model %>%
    coefficients() %>%
    names()
}

create_plot <- function(x, y, point_color, title, xaxis_title, yaxis_title) {

  tibble(x = x, y = y) %>%
    ggplot() +
    geom_point(aes(x = x, y = y), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)

}


