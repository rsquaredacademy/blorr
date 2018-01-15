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
blr_plot_residual_fitted <- function(model, point_color = 'blue', line_color = 'red',
                                     title = 'Standardized Pearson Residual vs Fitted Values',
                                     xaxis_title = 'Fitted Values',
                                     yaxis_title = 'Standardized Pearson Residual') {

  fit_val <- model %>%
    fitted

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
blr_plot_pearson_residual <- function(model, point_color = 'blue',
                                      title = 'Standardized Pearson Residuals',
                                      xaxis_title = 'id',
                                      yaxis_title = 'Standardized Pearson Residuals') {

  res_val <- model %>%
    rstandard(type = 'pearson')

  id <- res_val %>%
    length %>%
    seq_len

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
blr_plot_deviance_fitted <- function(model, point_color = 'blue', line_color = 'red',
                                     title = 'Deviance Residual vs Fitted Values',
                                     xaxis_title = 'Fitted Values',
                                     yaxis_title = 'Deviance Residual') {

  fit_val <- model %>%
    fitted.values

  res_val <- model %>%
    rstandard

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
blr_plot_deviance_residual <- function(model, point_color = 'blue',
                                      title = 'Deviance Residuals Plot',
                                      xaxis_title = 'id',
                                      yaxis_title = 'Deviance Residuals') {

  res_val <- model %>%
    rstandard()

  id <- res_val %>%
    length %>%
    seq_len

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
blr_plot_leverage_fitted <- function(model, point_color = 'blue',
                                     title = 'Leverage vs Fitted Values',
                                     xaxis_title = 'Fitted Values',
                                     yaxis_title = 'Leverage') {

  fit_val <- model %>%
    fitted.values

  res_val <- model %>%
    hatvalues

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
blr_plot_leverage <- function(model, point_color = 'blue',
                                       title = 'Leverage Plot',
                                       xaxis_title = 'id',
                                       yaxis_title = 'Leverage') {

  res_val <- model %>%
    hatvalues

  id <- res_val %>%
    length %>%
    seq_len

  tibble(id = id, resid = res_val) %>%
    ggplot() +
    geom_point(aes(x = id, y = resid), color = point_color) +
    ggtitle(title) + xlab(xaxis_title) + ylab(yaxis_title)

}
