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
