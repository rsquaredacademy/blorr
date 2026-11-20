data_column_response_apis <- list(
  blr_woe_iv = function(d) blr_woe_iv(d, g, y),
  blr_woe_iv_stats = function(d) blr_woe_iv_stats(d, y, g),
  blr_segment = function(d) blr_segment(d, y, g),
  blr_segment_dist = function(d) blr_segment_dist(d, y, g),
  blr_segment_twoway = function(d) blr_segment_twoway(d, y, g, h)
)

for (api in names(data_column_response_apis)) {
  test_that(paste(api, "rejects responses with three observed levels"), {
    call_api <- data_column_response_apis[[api]]
    d <- data.frame(g = rep(c("a", "b"), each = 3),
                    h = rep(c("x", "y"), 3))
    for (y in list(factor(rep(1:3, 2)), rep(c("no", "maybe", "yes"), 2),
                   rep(0:2, 2))) {
      d$y <- y
      expect_error(
        call_api(d),
        "The response `y` has 3 levels; blorr supports a binary response with exactly two levels.",
        fixed = TRUE
      )
    }
  })

  test_that(paste(api, "preserves ordinary binary response encodings"), {
    call_api <- data_column_response_apis[[api]]
    d <- data.frame(y = c(0, 1, 0, 1, 0, 1),
                    g = rep(c("a", "b"), each = 3),
                    h = rep(c("x", "y"), 3))
    expected_output <- capture.output(expected <- call_api(d))

    # Compare both returned results and printed multi-variable WoE tables.
    for (y in list(d$y, factor(ifelse(d$y == 1, "yes", "no")),
                   d$y == 1, ifelse(d$y == 1, "yes", "no"),
                   factor(d$y, levels = c(9, 0, 1)))) {
      d$y <- y
      output <- capture.output(result <- call_api(d))
      expect_equal(result, expected)
      expect_equal(output, expected_output)
    }
  })
}
