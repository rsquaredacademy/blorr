# pkg-blorr-c25-aud: audit of the pkg-blorr-c25 build (blorr #130)

2026-11-18. Audited `git diff origin/develop..HEAD` on `task/blorr-c25`
(base `origin/develop` = `bc76c35`, head `1bebeb2` "Check for a binary response
in response profile and regression print (#130)"). This was a read-and-verify
audit: no package code was changed.

Changed files: `NEWS.md`, `R/blr-output.R`, `R/blr-utils.R`,
`R/blr-gains-table.R` (roxygen tag only), `tests/testthat/test-binary-response.R`
(new), `docs-dev/c25-report.md` (builder report).

Domain used for judging: the package is documented as "Tools for Developing
Binary Logistic Regression Models", and every model entry point's `@param model`
says that only binary logistic regression models are supported. So a response
with three or more levels is outside the documented domain. The fix turns that
silent misuse into an error.

## Findings by severity

### High

None.

### Medium

**M1. The shared model gate has no test, so removing it leaves the suite green.**
The fix that matters most for users is the new line in `blr_check_supported()`
(`R/blr-utils.R:54`):
`blr_check_binary_response(resp, names(model$model)[1], exactly = FALSE)`.
Without it, `blr_model_fit_stats()`, `blr_rsq_adj_count()` and the stepwise
functions return numbers for a three-level response without any warning. The
new tests reach this line only through `blr_regress()`, and `resp_profile()`
then raises the same message anyway, so no test depends on the gate.

Mutation check: I extracted HEAD to `%TEMP%/pkg-blorr-c25-aud-mut`, deleted only
that line, and ran `devtools::test()`. Result:
`files = 26 tests = 245 failed = 0 error = 0 ... passed expectations = 1998`,
the same as the unmutated head.

What the gate catches (base on the left, head on the right). The 3-level model is
`m <- glm(ses3 ~ female + read, d, family = binomial)` with `d$ses3 <- hsb2$ses`:

| Call | origin/develop (expected: error) | HEAD |
| --- | --- | --- |
| `blr_model_fit_stats(m)` | prints a full fit table (McFadden R2 0.063, Adj Count R2 0.064, ...) | error, "The response `ses3` has 3 levels; ..." |
| `blr_rsq_adj_count(m)` | `0.06382979` | same error |
| `blr_step_aic_forward(glm(ses3 ~ female + read + math, d))` | prints a selection summary | same error |

The brief says "Every changed behaviour gets a testthat test that fails before
the fix". This changed behaviour has no such test. The suggested follow-up is to
add `expect_error(blr_model_fit_stats(m), "has 3 levels")` (and one for a
stepwise function or an R2 function). Behaviour is correct today, so this is a
test gap, not a wrong result.

### Low

**L1. The NEWS bullet does not fully match the behaviour.**
- It says the response profile (`resp_profile()`) "showed the frequencies of
  only the first two levels". In fact `resp_profile()` returned all three
  levels on base (`1 2 3 / 47 95 58`); only the printed summary
  (`print_blr_reg()`) dropped the third.
- It names `resp_profile()` and `print_blr_reg()`, which are internal and not
  exported, so users cannot call them.
- It leaves out the wider user-facing change: every model entry point now
  rejects a response with 3+ levels (`blr_model_fit_stats()`, the `blr_rsq_*()`
  functions, the stepwise functions, `blr_gains_table()`,
  `blr_confusion_matrix()`, `blr_test_hosmer_lemeshow()`). Some of these
  returned numbers on base (see M1); others already failed with "response
  contains values not present in the fitted model: 3".
- It also leaves out that `blr_regress()` on a single-outcome response (for
  example `zero ~ read` with every `zero == 0`) now fails at fit time with "The
  response `zero` has 1 level; ...". On base the object was built and printing
  then failed with "subscript out of bounds". That input is degenerate (the fit
  does not converge), so this is a note, not a regression.

The first sentence ("A response with more than two levels is now rejected with
an error naming the response and the number of levels found") is accurate.

**L2. The data-column APIs still silently treat level 2 as the event when the
response has 3+ levels.** This is pre-existing and was not changed by this
build. The builder listed it in the grep audit and as follow-up 1.

Reprex (same on base and head):

```r
d <- hsb2; d$ses3 <- d$ses
blr_woe_iv(d, female, ses3)   # count_0s 44/61, count_1s 47/48 -> levels 1 and 3 pooled as "0s"
blr_segment(d, ses3, female)  # 1s% 52.00 / 44.00 = share of level 2 only
```

Expected: an error, or a documented "level 2 vs rest" rule. Actual: WoE/IV and
event rates for "level 2 vs everything else", with no message. These functions'
`@param response` says only "Response variable; column in data". The package
scope (binary logistic regression) and the concepts themselves (WoE, "1s")
imply a binary response, so a 3-level column is outside the domain and not an
ordinary input. The brief's rule was "fix only where an ordinary input could
give a wrong result silently", so the builder's choice to defer is defensible.
Recommendation: do follow-up 1 (reuse `blr_check_binary_response()` on the
response column) and say "binary" in those `@param response` docs.

**L3. Process: the builder report has the wrong filename.** The brief asked for
`docs-dev/<id>-report.md`, i.e. `docs-dev/pkg-blorr-c25-report.md`. The commit
adds `docs-dev/c25-report.md` instead. The content is complete.

### Info

- **I1.** New test 3 (`print_blr_reg` with a hand-built list) does fail on the
  old code, but with an unrelated error (`round(x$est, 4)`: non-numeric
  argument) rather than an assertion mismatch. That is acceptable: the
  expectation still requires the new message.
- **I2.** The `R/blr-gains-table.R` roxygen change (`@importFrom ggplot2 ...
  element_text ... theme`) has no effect on `NAMESPACE`: `importFrom(ggplot2,
  element_text)` and `importFrom(ggplot2, theme)` were already there
  (`NAMESPACE:133,146`). It is harmless and future-proofs `document()`.
- **I3.** `blr_check_binary_response()` counts `nlevels()` for factor responses.
  A valid binary factor that carries an unused third level could in principle be
  wrongly rejected, but `glm()` builds its model frame with
  `drop.unused.levels = TRUE`, so `model$model[[1]]` never carries one. I tested
  this explicitly (rows `unused`, `ses13`, `sub` in the grid below): accepted,
  identical output.

## Passing checks

### (1) Each brief issue is fixed: reprexes re-run

Script `reprex.R` was run with `pkgload::load_all()` on the base extraction and
on the head worktree.

| Reprex | origin/develop | HEAD |
| --- | --- | --- |
| `resp_profile(m)` (3-level `ses3`) | `1 2 3 / 47 95 58` | Error: The response `ses3` has 3 levels; blorr supports a binary response with exactly two levels. |
| `blr_regress(ses3 ~ female + read, data = d)` | Response Summary shows `1 47` and `2 95` only; level 3 (58) dropped silently | same error |
| `blr_regress(m)` (glm method) | same silent truncation | same error |
| `print_blr_reg(list(resp_name = "ses3", resp_prof = table(d$ses3)))` | prints `1 47 / 2 95`, then errors on `round(x$est)` | same error, raised before any output |
| `blr_regress(prog4 ~ read)` (6 levels) | prints `1 0 21 / 1 1 24`, the other 4 levels dropped | Error: The response `prog4` has 6 levels; ... |
| `blr_gains_table(m)`, `blr_confusion_matrix(m)`, `blr_test_hosmer_lemeshow(m)` | error "response contains values not present in the fitted model: 3" | new clear error |
| `blr_model_fit_stats(m)`, `blr_rsq_adj_count(m)`, `blr_step_aic_forward(...)` | silent numbers (see M1) | new clear error |
| character 3-level response | `glm()` itself errors (`y values must be 0 <= y <= 1`) | unchanged |
| `blr_confusion_matrix()` on an all-zero response | reported (existing tested behaviour) | unchanged, identical output |

The error message names the response and the number of levels found, as the
brief requires.

The builder's grep audit of other two-level assumptions is present and
matches the code. The model-side helpers (`model_response_levels()`'s
`lvls[1:2]`, `model_event_level()`'s `lvls[2]`, `blr_confusion_matrix()`,
`blr_rsq_adj_count()`, `blr_pairs()`) are all reached only behind
`blr_check_model()`, which now rejects 3+ levels. The data-column APIs remain
(L2).

### (2) Ordinary inputs give the same numbers as origin/develop

`grid.R` was run on both trees in separate Rscript processes. Each call stores
the normalised returned value (ggplot objects reduced to `$data`/labels, glm
objects to coef/deviance) plus the captured printed output. `compare.R` then
checks the error message, the print output (`identical`) and the value
(`all.equal`, tolerance 0).

```text
calls: 258  identical: 258  differing: 0
errors on base: 0  errors on head: 0
```

Grid contents:

- **13 models:**
  - numeric 0/1 (`honcomp`)
  - two-level labelled factor
  - reversed-level factor
  - ordered factor
  - logical
  - factor with an unused third level in the data
  - response with NAs
  - `I(math > 60)`
  - `ses` restricted to levels 1/3 with level 2 left unused
  - `subset = ses != "2"`
  - mtcars `am ~ wt + hp`
  - `am == 1 ~ wt`
  - `bank_marketing` with factor predictors
- **18 calls per model:**
  - `blr_regress()`, with and without `odd_conf_limit`
  - `resp_profile()`
  - `blr_gains_table()`
  - `blr_confusion_matrix()` at cutoffs 0.5 and 0.3
  - `blr_model_fit_stats()`
  - `blr_test_hosmer_lemeshow()`
  - `blr_pairs()`
  - `blr_gini_index()`
  - `blr_prep_lorenz_data()`
  - all eight `blr_rsq_*()` functions (one call)
  - `blr_residual_diagnostics()`
  - `blr_linktest()`
  - `blr_coll_diag()`
  - ROC and KS prep data
  - `blr_plot_residual_fitted()` data
- **Other calls:**
  - held-out `data =` for the gains table, confusion matrix and HL test
  - the formula interface (including the unused-level and 1/3-subset factors)
  - `blr_test_lr()` and `blr_multi_model_fit_stats()`
  - all six stepwise procedures
  - `blr_segment()`, `blr_segment_dist()`, `blr_segment_twoway()`,
    `blr_bivariate_analysis()`, `blr_woe_iv()`, `blr_woe_iv_stats()` on
    mtcars and bank_marketing

### (3) New errors don't reject valid inputs

None of the 258 ordinary calls errors on head. That covers binary factors with
unused levels, reversed or ordered levels, logical and expression responses,
NA responses, and `subset =`. The `exactly = FALSE` setting in the shared gate
keeps the existing single-outcome support in `blr_confusion_matrix()`
(`test-model-validation.R`), which I confirmed. The only newly rejected inputs
are responses with 3+ levels (outside the domain) and `blr_regress()` /
`resp_profile()` on a single-outcome response (degenerate; this already failed
on base, at print time).

### (4) Tests are meaningful

I copied the new test file into the base extraction and ran
`devtools::test(base, filter = "binary-response")`:

```text
TOTALS: files = 1  tests = 4  failed = 2  error = 1  skipped = 0  passed expectations = 8
Failure test-binary-response.R:16  resp_profile(m) did not throw
Failure test-binary-response.R:25  blr_regress(ses3 ~ ...) did not throw
Error   test-binary-response.R:35  round(x$est, 4): non-numeric argument
```

The fourth test ("binary responses unchanged") passes on base by design: it is
a regression pin, with values matching the base outputs. The gap is the
untested shared gate (M1).

### (5) Docs/NEWS match behaviour

NEWS has the bullet under `# blorr (development version)` / `## Bug fixes`. Its
headline sentence is accurate; the detail has the issues in L1. No `.Rd`
changes were needed or made: the code changes are internal, and the existing
`@param model` text already limits support to binary logistic regression. The
version is still `0.3.2.9000` (no bump). The builder report's claims match
what I observed (reprex output, the 1998-expectation total, before-fix
failures).

### (6) `devtools::test()` passes

```text
TOTALS: files = 26  tests = 245  failed = 0  error = 0  skipped = 0  warning = 0  passed expectations = 1998
```

The test run rewrote snapshot line endings and `tests/testthat/Rplots.pdf`.
`git diff --ignore-cr-at-eol` was empty for the snapshots, and these generated
artifacts were restored with `git checkout`. The compiled `src/*.o` and
`src/blorr.dll` files are git-ignored.

## Commands run

Scratch scripts live in
`C:/Users/DELL/.gemini/antigravity-cli/brain/2c612999-9fbd-4696-aeff-5deb386ccbd1/scratch/`
(`reprex.R`, `grid.R`, `compare.R`, `runtests.R`).

```text
Get-Content F:/pm/notes/2026-11-15-day/briefs/pkg-blorr-c25-brief.md
git status; git log --oneline origin/develop..HEAD; git diff --stat origin/develop..HEAD
git diff origin/develop..HEAD -- NEWS.md R tests
# base extraction (a direct `git archive | tar` pipe corrupts binary data in PowerShell 5.1, so went via a tar file)
git archive --format=tar -o $env:TEMP/pkg-blorr-c25-aud-base.tar origin/develop
tar --force-local -x -f $env:TEMP/pkg-blorr-c25-aud-base.tar -C $env:TEMP/pkg-blorr-c25-aud-base
Rscript reprex.R $env:TEMP/pkg-blorr-c25-aud-base   > reprex-base.txt
Rscript reprex.R .                                  > reprex-head.txt
Rscript grid.R $env:TEMP/pkg-blorr-c25-aud-base grid-base.rds
Rscript grid.R . grid-head.rds
Rscript compare.R grid-base.rds grid-head.rds
Rscript runtests.R .                                               # devtools::test() on head
Copy-Item tests/testthat/test-binary-response.R $env:TEMP/pkg-blorr-c25-aud-base/tests/testthat/
Rscript runtests.R $env:TEMP/pkg-blorr-c25-aud-base binary-response  # new tests vs old code
git archive --format=tar -o $env:TEMP/pkg-blorr-c25-aud-head.tar HEAD   # mutant copy, gate line removed
tar --force-local -x -f $env:TEMP/pkg-blorr-c25-aud-head.tar -C $env:TEMP/pkg-blorr-c25-aud-mut
Rscript runtests.R $env:TEMP/pkg-blorr-c25-aud-mut                 # 0 failures -> M1
git diff --ignore-cr-at-eol --stat -- tests/testthat/_snaps; git checkout -- tests/testthat/Rplots.pdf tests/testthat/_snaps
```

All R runs used `pkgload::load_all()` (`devtools::test()` for the test runs).
Nothing was installed, no worktrees were created, and nothing was pushed.

Any wrong result left inside the domain: no
