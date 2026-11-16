test_that("output from blr_pairs is as expected", {
  model <- glm(
    honcomp ~ female + read + science, data = blorr::hsb2,
    family = binomial(link = "logit")
  )

  actual   <- round(blr_pairs(model)[['gamma']], 2)
  expected <- 0.71

  expect_equal(actual, expected)
})

test_that("Somers' D and gamma use the correct denominators", {
  model <- glm(
    honcomp ~ female, data = blorr::hsb2,
    family = binomial(link = "logit")
  )

  k    <- blr_pairs(model)
  resp <- model$y
  fit  <- fitted(model)
  n    <- length(resp)

  concordant <- discordant <- tied <- 0

  for (i in seq_len(n - 1)) {
    for (j in seq.int(i + 1, n)) {
      if (resp[i] == resp[j]) {
        next
      }

      hi <- if (resp[i] == 1) fit[i] else fit[j]
      lo <- if (resp[i] == 1) fit[j] else fit[i]

      if (hi > lo) {
        concordant <- concordant + 1
      } else if (hi < lo) {
        discordant <- discordant + 1
      } else {
        tied <- tied + 1
      }
    }
  }

  expect_equal(concordant + discordant + tied, k$pairs)
  expect_equal(k$gamma, (concordant - discordant) / (concordant + discordant))
  expect_equal(k$somers_d,
               (concordant - discordant) / (concordant + discordant + tied))
})

test_that("pair counts do not overflow 32-bit integers on large fits", {
  skip_on_cran()

  set.seed(2)
  n <- 100000
  d <- data.frame(
    y  = rbinom(n, 1, 0.5),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  model <- glm(y ~ x1 + x2, data = d, family = binomial)

  n1 <- sum(model$y == 1)
  n0 <- sum(model$y == 0)

  k <- blr_pairs(model)

  expect_gt(as.double(n1) * n0, 2 ^ 31 - 1)
  expect_equal(k$pairs, as.double(n1) * n0)
  expect_true(all(c(k$concordance, k$discordance, k$tied) >= 0))
  expect_equal(k$concordance * k$pairs + k$discordance * k$pairs +
                 k$tied * k$pairs, k$pairs, tolerance = 1e-8)
  expect_true(is.finite(k$tau))
  expect_equal(k$c, (blr_gini_index(model) + 1) / 2, tolerance = 1e-10)
})


test_that("blr_pairs works for na.exclude fits (bfx6)", {
  d <- mtcars
  d$hp[c(2, 9, 30)] <- NA
  m_ex   <- glm(am ~ wt + hp, d, family = binomial, na.action = na.exclude)
  m_omit <- glm(am ~ wt + hp, d, family = binomial, na.action = na.omit)
  expect_equal(blr_pairs(m_ex), blr_pairs(m_omit))
  expect_equal(blr_pairs(m_ex)[["pairs"]], 198)
})
