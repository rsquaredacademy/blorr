#' Residual vs fitted values plot
#'
#' Residual vs fitted values plot.
#'
#' @inheritParams blr_plot_pearson_residual
#' @param line_color Color of the horizontal line.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_residual_fitted(model)
#'
#' @importFrom ggplot2 geom_hline
#' @importFrom stats residuals rstandard hatvalues
#'
#' @export
#'
blr_plot_residual_fitted <- function(model, point_color = "blue", line_color = "red",
                                     title = "Standardized Pearson Residual vs Fitted Values",
                                     xaxis_title = "Fitted Values",
                                     yaxis_title = "Standardized Pearson Residual") {

  blr_check_model(model)
  fit_val <- fitted(model)
  res_val <- rstandard(model, type = "pearson")

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title) +
    geom_hline(yintercept = 0, color = line_color)

}

#' Residual values plot
#'
#' Standardised pearson residuals plot.
#'
#' @param model An object of class \code{glm}.
#' @param point_color Color of the points.
#' @param title Title of the plot.
#' @param xaxis_title X axis label.
#' @param yaxis_title Y axis label.
#'
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

  blr_check_model(model)
  res_val <- rstandard(model, type = "pearson")
  id      <- plot_id(res_val)
  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' Deviance vs fitted values plot
#'
#' Deviance vs fitted values plot.
#'
#' @inheritParams blr_plot_pearson_residual
#' @param line_color Color of the horizontal line.
#'
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

  blr_check_model(model)
  fit_val <- fitted(model)
  res_val <- rstandard(model)

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title) +
    geom_hline(yintercept = 0, color = line_color)
}

#' Deviance residual values
#'
#' Deviance residuals plot.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- rstandard(model)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' Leverage vs fitted values plot
#'
#' Leverage vs fitted values plot
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  fit_val <- fitted(model)
  res_val <- hatvalues(model)

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' Leverage plot
#'
#' Leverage plot.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- hatvalues(model)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' Residual diagnostics
#'
#' @description
#' Diagnostics for confidence interval displacement and detecting ill fitted
#' observations.
#'
#' @param model An object of class \code{glm}.
#'
#' @return C, CBAR, DIFDEV and DIFCHISQ.
#'
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_residual_diagnostics(model)
#'
#' @export
#'
blr_residual_diagnostics <- function(model) {

  blr_check_model(model)

  res_val  <- residuals(model, type = "pearson") ^ 2
  hat_val  <- hatvalues(model)
  num      <- res_val * hat_val
  den      <- 1 - hat_val
  c        <- num / (den ^ 2)
  cbar     <- num / den
  difchisq <- cbar / hat_val
  difdev   <- (rstandard(model) ^ 2) + cbar
    
  data.frame(c = c, cbar = cbar, difdev = difdev, difchisq = difchisq)

}


#' CI Displacement C plot
#'
#' Confidence interval displacement diagnostics C plot.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- extract_diag(model, c)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' CI Displacement CBAR plot
#'
#' Confidence interval displacement diagnostics CBAR plot.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- extract_diag(model, cbar)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' Delta chisquare plot
#'
#' Diagnostics for detecting ill fitted observations.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- extract_diag(model,difchisq)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' Delta deviance plot
#'
#' Diagnostics for detecting ill fitted observations.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- extract_diag(model, difdev)
  id      <- plot_id(res_val)

  create_plot(id, res_val, point_color, title, xaxis_title, yaxis_title)

}



#' DFBETAs panel
#'
#' Panel of plots to detect influential observations using DFBETAs.
#'
#' @param model An object of class \code{glm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details
#' DFBETA measures the difference in each parameter estimate with and without
#' the influential point. There is a DFBETA for each data point i.e if there
#' are n observations and k variables, there will be \eqn{n * k} DFBETAs. In
#' general, large values of DFBETAS indicate observations that are influential
#' in estimating a given parameter. Belsley, Kuh, and Welsch recommend 2 as a
#' general cutoff value to indicate influential observations and
#' \eqn{2/\sqrt(n)} as a size-adjusted cutoff.
#'
#' @return list; \code{blr_dfbetas_panel} returns a list of tibbles (for
#' intercept and each predictor) with the observation number and DFBETA of
#' observations that exceed the threshold for classifying an observation as an
#' outlier/influential observation.
#'
#' @references
#' Belsley, David A.; Kuh, Edwin; Welsh, Roy E. (1980). Regression
#' Diagnostics: Identifying Influential Data and Sources of Collinearity.
#' Wiley Series in Probability and Mathematical Statistics.
#' New York: John Wiley & Sons. pp. ISBN 0-471-05856-4.
#'
#' @examples
#' \dontrun{
#' model <- glm(honcomp ~ female + read + science, data = hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_plot_dfbetas_panel(model)
#' }
#'
#' @importFrom stats dfbetas
#' @importFrom ggplot2 geom_linerange geom_text annotate
#'
#' @export
#'
blr_plot_dfbetas_panel <- function(model, print_plot = TRUE) {

  blr_check_model(model)

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

  if (print_plot) {
    suppressWarnings(do.call(grid.arrange, c(myplots, list(ncol = 2))))
  }
  
  names(outliers) <- model_coeff_names(model)
  result <- list(outliers = outliers, plots = myplots)
  invisible(result)

}

#' CI Displacement C vs fitted values plot
#'
#' Confidence interval displacement diagnostics C vs fitted values plot.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- extract_diag(model, c)
  fit_val <- fitted(model)

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' Delta chi square vs fitted values plot
#'
#' Delta Chi Square vs fitted values plot for detecting ill fitted observations.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- extract_diag(model, difchisq)
  fit_val <- fitted(model)

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' Delta deviance vs fitted values plot
#'
#' Delta deviance vs fitted values plot for detecting ill fitted observations.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- extract_diag(model, difdev)
  fit_val <- fitted(model)

  create_plot(fit_val, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' Delta deviance vs leverage plot
#'
#' Delta deviance vs leverage plot.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- extract_diag(model, difdev)
  hat_val <- hatvalues(model)

  create_plot(hat_val, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' Delta chi square vs leverage plot
#'
#' Delta chi square vs leverage plot.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- extract_diag(model, difchisq)
  hat_val <- hatvalues(model)

  create_plot(hat_val, res_val, point_color, title, xaxis_title, yaxis_title)

}

#' CI Displacement C vs leverage plot
#'
#' Confidence interval displacement diagnostics C vs leverage plot.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  res_val <- extract_diag(model, c)
  hat_val <- hatvalues(model)

  create_plot(hat_val, res_val, point_color, title, xaxis_title, yaxis_title)

}


#' Fitted values vs leverage plot
#'
#' Fitted values vs leverage plot.
#'
#' @inheritParams blr_plot_pearson_residual
#'
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

  blr_check_model(model)
  fit_val <- fitted(model)
  hat_val <- hatvalues(model)

  create_plot(hat_val, fit_val, point_color, title, xaxis_title, yaxis_title)


}

plot_id <- function(res_val) {
  seq_len(length(res_val))
}


extract_diag <- function(model, value) {
  vals <- deparse(substitute(value))
  blr_residual_diagnostics(model)[[vals]] 
}


dfbetas_data_prep <- function(dfb, n, threshold, i) {

  dbetas  <- dfb[, i]
  d       <- data.frame(obs = seq_len(n), dbetas = dbetas)

  d$color <- ifelse(((d$dbetas >= threshold) | (d$dbetas <= -threshold)), 
    c("outlier"), c("normal"))
  d$fct_color <- ordered(factor(color), levels = c("normal", "outlier"))
  d$txt   <- ifelse(d$color == "outlier", obs, NA)

  # tibble(obs = seq_len(n), dbetas = dbetas) %>%
  #   mutate(
  #     color = ifelse(((dbetas >= threshold) | (dbetas <= -threshold)),
  #                     c("outlier"), c("normal")),
  #     fct_color = color %>%
  #       factor() %>%
  #       ordered(levels = c("normal", "outlier")),
  #     txt = ifelse(color == "outlier", obs, NA)
  #   )

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
  d[d$color == "outlier", c('obs', 'betas')]
}


model_coeff_names <- function(model) {
  names(coefficients(model))
}

create_plot <- function(x, y, point_color, title, xaxis_title, yaxis_title) {

  ggplot(data.frame(x = x, y = y)) +
    geom_point(aes(x = x, y = y), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)

}


