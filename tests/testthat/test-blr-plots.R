context("test-blr-plots.R")

model <- glm(
  honcomp ~ female + read + science, data = hsb2,
  family = binomial(link = "logit")
)

test_that("event segmentation distribution plot is as expected", {
  skip_on_cran()

  p <-
    blr_segment_dist(hsb2, honcomp, prog) %>%
    plot()

  vdiffr::expect_doppelganger("segment distribution plot", p)
})

test_that("woe plot is as expected", {
  skip_on_cran()

  p <-
    blr_woe_iv(hsb2, prog, honcomp) %>%
    plot()

  vdiffr::expect_doppelganger("woe plot", p)
})

test_that("lift chart is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_gains_table() %>%
    plot()

  vdiffr::expect_doppelganger("lift chart", p)
})

test_that("ks chart is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_gains_table() %>%
    blr_ks_chart()

  vdiffr::expect_doppelganger("ks chart", p)
})

test_that("lorenz curve is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_lorenz_curve()

  vdiffr::expect_doppelganger("lorenz curve", p)
})

test_that("roc curve is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_gains_table() %>%
    blr_roc_curve()

  vdiffr::expect_doppelganger("roc curve", p)
})

test_that("residual vs fitted plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_residual_fitted()

  vdiffr::expect_doppelganger("residual fitted plot", p)
})

test_that("pearson residual plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_pearson_residual()

  vdiffr::expect_doppelganger("pearson residual plot", p)
})

test_that("deviance vs fitted plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_deviance_fitted()

  vdiffr::expect_doppelganger("deviance fitted plot", p)
})

test_that("deviance residual plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_deviance_residual()

  vdiffr::expect_doppelganger("deviance residual plot", p)
})


test_that("leverage vs fitted plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_leverage_fitted()

  vdiffr::expect_doppelganger("leverage fitted plot", p)
})

test_that("leverage plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_leverage()

  vdiffr::expect_doppelganger("leverage plot", p)
})

test_that("c plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_diag_c()

  vdiffr::expect_doppelganger("c plot", p)
})

test_that("cbar plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_diag_cbar()

  vdiffr::expect_doppelganger("cbar plot", p)
})

test_that("difchisq plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_diag_difchisq()

  vdiffr::expect_doppelganger("difchisq plot", p)
})

test_that("difdev plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_diag_difdev()

  vdiffr::expect_doppelganger("difdev plot", p)
})


test_that("c vs fitted plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_c_fitted()

  vdiffr::expect_doppelganger("c fitted plot", p)
})

test_that("difchisq vs fitted plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_difchisq_fitted()

  vdiffr::expect_doppelganger("difchisq fitted plot", p)
})

test_that("difdev vs fitted plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_difdev_fitted()

  vdiffr::expect_doppelganger("difdev fitted plot", p)
})

test_that("difdev vs leverage plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_difdev_leverage()

  vdiffr::expect_doppelganger("difdev leverage plot", p)
})

test_that("difchisq vs leverage plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_difchisq_leverage()

  vdiffr::expect_doppelganger("difchisq leverage plot", p)
})

test_that("c vs leverage plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_c_leverage()

  vdiffr::expect_doppelganger("c leverage plot", p)
})

test_that("fitted vs leverage plot is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_plot_fitted_leverage()

  vdiffr::expect_doppelganger("fitted leverage plot", p)
})


test_that("decile lift chart is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_gains_table() %>%
    blr_decile_lift_chart()

  vdiffr::expect_doppelganger("decile lift chart", p$plot)
})

test_that("decile wise capture rate chart is as expected", {
  skip_on_cran()

  p <-
    model %>%
    blr_gains_table() %>%
    blr_decile_capture_rate()

  vdiffr::expect_doppelganger("decile capture rate chart", p$plot)
})
