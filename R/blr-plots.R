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
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  fit_val <- model %>%
    fitted()

  res_val <- model %>%
    rstandard(type = "pearson")

  tibble(fit = fit_val, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = fit, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title) +
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
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    rstandard(type = "pearson")

  id <- res_val %>%
    length() %>%
    seq_len()

  tibble(id = id, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = id, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
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
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  fit_val <- model %>%
    fitted.values()

  res_val <- model %>%
    rstandard()

  tibble(fit = fit_val, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = fit, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title) +
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
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    rstandard()

  id <- res_val %>%
    length() %>%
    seq_len()

  tibble(id = id, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = id, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}


#' @title Leverage vs Fitted Values Plot
#' @description Leverage vs fitted values plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  fit_val <- model %>%
    fitted.values()

  res_val <- model %>%
    hatvalues()

  tibble(fit = fit_val, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = fit, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}


#' @title Leverage Plot
#' @description Leverage plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    hatvalues()

  id <- res_val %>%
    length() %>%
    seq_len()

  tibble(id = id, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = id, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}


#' @title Residual Diagnostics
#' @description Diagnostics for confidence interval displacement and
#' detecting ill fitted observations
#' @param model an object of class \code{glm}
#' @return C, CBAR, DIFDEV and DIFCHISQ
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_residual_diagnostics(model)
#'
#' @export
#'
blr_residual_diagnostics <- function(model) {
  hat_val <- model %>%
    hatvalues()

  res_val <- model %>%
    residuals(type = "pearson") %>%
    raise_to_power(2)

  num <- res_val * hat_val

  den <- 1 %>%
    subtract(hat_val)

  c <- num %>%
    divide_by(den %>%
      raise_to_power(2))

  cbar <- num %>%
    divide_by(den)

  difdev <- model %>%
    rstandard() %>%
    raise_to_power(2) %>%
    add(cbar)

  difchisq <- cbar %>%
    divide_by(hat_val)

  result <- tibble(c = c, cbar = cbar, difdev = difdev, difchisq = difchisq)
  return(result)
}


#' @title CI Displacement C Plot
#' @description Confidence interval displacement diagnostics C plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    blr_residual_diagnostics() %>%
    pull(c)

  id <- res_val %>%
    length() %>%
    seq_len()

  tibble(id = id, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = id, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}


#' @title CI Displacement CBAR Plot
#' @description Confidence interval displacement diagnostics CBAR plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    blr_residual_diagnostics() %>%
    pull(cbar)

  id <- res_val %>%
    length() %>%
    seq_len()

  tibble(id = id, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = id, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}


#' @title Delta Chisquare Plot
#' @description Diagnostics for detecting ill fitted observations
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    blr_residual_diagnostics() %>%
    pull(difchisq)

  id <- res_val %>%
    length() %>%
    seq_len()

  tibble(id = id, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = id, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}

#' @title Delta Deviance Plot
#' @description Diagnostics for detecting ill fitted observations
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    blr_residual_diagnostics() %>%
    pull(difdev)

  id <- res_val %>%
    length() %>%
    seq_len()

  tibble(id = id, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = id, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
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
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
#' family = binomial(link = 'logit'))
#'
#' blr_dfbetas_panel(model)
#' }
#' @export
#'
blr_plot_dfbetas_panel <- function(model) {
  if (!any(class(model) == "glm")) {
    stop("Please specify a binary logistic regression model.", call. = FALSE)
  }

  dfb <- dfbetas(model)
  n <- nrow(dfb)
  np <- ncol(dfb)
  threshold <- 2 / sqrt(n)
  obs <- NULL
  txt <- NULL
  Observation <- NULL
  myplots <- list()
  outliers <- list()
  for (i in seq_len(np)) {
    dbetas <- dfb[, i]

    d <- tibble(obs = seq_len(n), dbetas = dbetas)
    d$color <- ifelse(((d$dbetas >= threshold) | (d$dbetas <= -threshold)), c("outlier"), c("normal"))
    d$color1 <- factor(d$color)
    d$Observation <- ordered(d$color1, levels = c("normal", "outlier"))
    d <- d %>% mutate(txt = ifelse(Observation == "outlier", obs, NA))
    f <- d %>%
      filter(., Observation == "outlier") %>%
      select(obs, dbetas)
    p <- eval(substitute(
      ggplot(d, aes(x = obs, y = dbetas, label = txt, ymin = 0, ymax = dbetas)) +
        geom_linerange(colour = "blue") +
        geom_hline(yintercept = c(0, threshold, -threshold), colour = "red") +
        geom_point(colour = "blue", shape = 1) +
        xlab("Observation") + ylab("DFBETAS") +
        ggtitle(paste("Influence Diagnostics for", colnames(dfb)[i])) +
        geom_text(hjust = -0.2, nudge_x = 0.15, size = 2, family = "serif", fontface = "italic", colour = "darkred", na.rm = TRUE) +
        annotate(
          "text", x = Inf, y = Inf, hjust = 1.5, vjust = 2,
          family = "serif", fontface = "italic", colour = "darkred",
          label = paste("Threshold:", round(threshold, 2))
        ),
      list(i = i)
    ))
    # print(p)
    myplots[[i]] <- p
    outliers[[i]] <- f
  }

  suppressWarnings(do.call(grid.arrange, c(myplots, list(ncol = 2))))

  names(outliers) <- model %>%
    coefficients() %>%
    names()

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
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    blr_residual_diagnostics() %>%
    pull(c)

  fit_val <- model %>%
    fitted.values()

  tibble(fit = fit_val, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = fit, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}

#' @title Delta Chi Square vs Fitted Values Plot
#' @description Delta Chi Square vs fitted values plot for detecting ill fitted observations
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    blr_residual_diagnostics() %>%
    pull(difchisq)

  fit_val <- model %>%
    fitted.values()

  tibble(fit = fit_val, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = fit, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}

#' @title Delta Deviance vs Fitted Values Plot
#' @description Delta deviance vs fitted values plot for detecting ill fitted observations
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    blr_residual_diagnostics() %>%
    pull(difdev)

  fit_val <- model %>%
    fitted.values()

  tibble(fit = fit_val, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = fit, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}

#' @title Delta Deviance vs Leverage Plot
#' @description Delta deviance vs leverage plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    blr_residual_diagnostics() %>%
    pull(difdev)

  hat_val <- model %>%
    hatvalues()

  tibble(hat = hat_val, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = hat, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}

#' @title Delta Chi Square vs Leverage Plot
#' @description Delta chi square vs leverage plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    blr_residual_diagnostics() %>%
    pull(difchisq)

  hat_val <- model %>%
    hatvalues()

  tibble(hat = hat_val, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = hat, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}

#' @title CI Displacement C vs Leverage Plot
#' @description Confidence interval displacement diagnostics C vs leverage plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  res_val <- model %>%
    blr_residual_diagnostics() %>%
    pull(c)

  hat_val <- model %>%
    hatvalues()

  tibble(hat = hat_val, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = hat, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}


#' @title Fitted Values vs Leverage Plot
#' @description Fitted values vs leverage plot
#' @param model an object of class \code{glm}
#' @param point_color color of the points
#' @param title title of the plot
#' @param xaxis_title x axis label
#' @param yaxis_title y axis label
#' @examples
#' model <- glm(honcomp ~ female + read + science, data = blorr::hsb2,
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
  fit_val <- model %>%
    fitted.values()

  hat_val <- model %>%
    hatvalues()

  tibble(hat = hat_val, fit = fit_val) %>%
    ggplot() +
    geom_point(aes(x = hat, y = fit), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)
}
