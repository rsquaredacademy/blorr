# c25: binary-response check for response profile and regression print (#130)

2026-11-18. Branch `task/blorr-c25` from `origin/develop` at `bc76c35`
(blorr 0.3.2.9000). One commit:

- `Check for a binary response in response profile and regression print (#130)`

## What was wrong

`glm()` happily fits a binomial model to a factor response with three or more
levels by recoding it as "first level" versus "everything else". blorr then
assumed exactly two response levels:

- `resp_profile()` returned the full `table()` of the response, but
- `print_blr_reg()` indexed only `resp_prof[[1]]` and `resp_prof[[2]]`, and
  printed `outcomes[1]`/`outcomes[2]`,

so the printed response summary silently dropped every level after the second.
For `ses3` (a copy of hsb2's three-level `ses`) with 200 observations, the
summary reported 47 and 95 and never mentioned the third level (58).

### Exported name

Issue #130 calls the function `blr_resp_profile()`; `NAMESPACE` exports no such
name. The actual function is the internal `resp_profile()` (`R/blr-utils.R`),
used by `blr_reg_compute()` to build `resp_prof` and by `print_blr_reg()` to
print it. The guard was added to the real names; no new export was introduced.

## Fix

- New shared helper `blr_check_binary_response(resp, name, exactly = TRUE)` in
  `R/blr-utils.R:74`. It counts `nlevels()` for factors, unique non-missing
  values for numeric/logical/character responses, and `length()` for a response
  profile table, then stops with:

  ```text
  The response `ses3` has 3 levels; blorr supports a binary response with
  exactly two levels.
  ```

  The message names the response and the number of levels found (singular
  "level" when there is one).
- `resp_profile()` (`R/blr-utils.R:281`) checks the fitted model's response
  before tabulating.
- `print_blr_reg()` (`R/blr-output.R:1`) checks `x$resp_prof`; this protects a
  printed object even if it was built without going through the model gate.
- `blr_check_supported()` (`R/blr-utils.R:54`), the guard reached by
  `blr_check_model()` in every model entry point (`blr_regress()`,
  `blr_gains_table()`, `blr_confusion_matrix()`, the R2 functions, the plots,
  the stepwise procedures, ...), rejects responses with more than two levels.
  It uses `exactly = FALSE` so degenerate single-outcome fits keep their
  previous behaviour: `blr_confusion_matrix()` has an existing test that
  reports an all-zero-response model (`tests/testthat/test-model-validation.R:42`)
  rather than refusing it, and that behaviour is unchanged. The response
  profile and its printer do require exactly two outcomes, so `blr_regress()`
  on such a degenerate fit now fails early with the new clear message instead
  of failing later inside the printer.
- `R/blr-gains-table.R:139`: added the missing `element_text` and `theme`
  roxygen imports. They were used but never tagged, and the installed
  roxygen2 8.1.1 drops untagged imports when regenerating `NAMESPACE`
  (`devtools::document()` removed them and every plot then failed with
  `could not find function "theme"`). With the tags, `document()` keeps them.

No values in returned objects changed for binary responses: the pre/post
comparison below is byte identical for `resp_profile()`, the printed
`blr_regress()` output, `blr_gains_table()`, `blr_confusion_matrix()` and
`blr_model_fit_stats()`.

## Other R/ code that assumes two levels (grep audit)

`levels(...)[1:2]`, `levels(...)[2]`, `table(...)[2]`, `[[2]]`, `max(table())`
and `event_indicator()` uses, as of this branch:

| Location | Assumption | Status |
| --- | --- | --- |
| `R/blr-output.R:32,43-44` (`print_blr_reg`) | `resp_prof[[1]]`, `resp_prof[[2]]`, `outcomes[1:2]` | **Fixed**: guard errors first. |
| `R/blr-utils.R:281` (`resp_profile`) | `table()` has two entries | **Fixed**: exactly-two check. |
| `R/blr-utils.R:216,233` (`model_response_levels`) | returns `lvls[1:2]` | Reachable only through `blr_check_model()`; now rejects >2 levels. The truncation itself was left as is. |
| `R/blr-utils.R:172,189` (`model_event_level`) | event is `lvls[2]` | Same: gated by `blr_check_model()` for model input. |
| `R/blr-utils.R:119-155` (`event_indicator`) | event is the second level; a third level becomes a non-event | Used by data-based bivariate functions (below); for held-out data of a fitted model, `model_event_indicator()` errors clearly with "response contains values not present in the fitted model". |
| `R/blr-model-validation.R:62-66,72` (`blr_confusion_matrix`) | `table(Prediction, Reference)` is 2x2, cells `[2,2]...[1,1]`, `max(table(ref))` | Model input gated by `blr_check_model()`; single-outcome fits deliberately still accepted. |
| `R/blr-model-fit-stats.R:362` (`blr_rsq_adj_count`) | `max(table(resp))` over a two-level response | Gated by `blr_check_model()`. |
| `R/blr-pairs.R:28-39` (`blr_pairs`) | pairs `response == 1` against `response == 0` | Uses `model$y`, already 0/1; gated by `blr_check_model()`. |
| `R/blr-gains-table.R:291-302`, `R/blr-lorenz-curve.R:52-78`, `R/blr-hosmer-lemeshow-test.R:45,64-68` | response/event via `model_event_indicator()` | Gated by `blr_check_model()`; held-out data with unknown labels errors. |
| `R/blr-bivariate-analysis.R:39-50` (`blr_bivariate_analysis`), `:134-159` (`blr_segment`), `:188-215` (`blr_segment_twoway`), `:255-307` (`blr_segment_dist`), `R/blr-woe-iv.R:55-84` (`blr_woe_iv`), `:140-150` (`blr_woe_iv_stats`) | take a response **column**, call `event_indicator()`/`glm()` directly | **Not fixed**: a 3+ level factor response is treated as level 2 = event, silently producing wrong rates/WoE/IV. These are data-column APIs, not model APIs, so the model gate cannot cover them. Left as a follow-up (below); an ordinary binary column is unaffected. |

## Before / after reprex

Data and call:

```r
library(blorr)
d <- hsb2
d$ses3 <- d$ses          # factor with three levels: 1, 2, 3
m <- glm(ses3 ~ female + read, data = d, family = binomial(link = "logit"))
resp_profile(m)
blr_regress(ses3 ~ female + read, data = d)
```

Before (exported-API output, current `origin/develop` code):

```text
> resp_profile(m)

 1  2  3
47 95 58

> blr_regress(ses3 ~ female + read, data = d)
                             Model Overview
------------------------------------------------------------------------
Data Set    Resp Var    Obs.    Df. Model    Df. Residual    Convergence
------------------------------------------------------------------------
  data        ses3      200         2            197            TRUE
------------------------------------------------------------------------

                    Response Summary
--------------------------------------------------------
Outcome        Frequency        Outcome        Frequency
--------------------------------------------------------
   1              47               2              95
--------------------------------------------------------
... (third level, 58 observations, silently omitted)
```

After (this branch):

```text
## resp_profile(m)
Error: The response `ses3` has 3 levels; blorr supports a binary response with exactly two levels.

## blr_regress(ses3 ~ female + read, data = d)
Error: The response `ses3` has 3 levels; blorr supports a binary response with exactly two levels.

## print_blr_reg() with a three-outcome profile
Error: The response `ses3` has 3 levels; blorr supports a binary response with exactly two levels.

## ordinary binary response: resp_profile()
 0  1
19 13

## ordinary binary response: response summary
                    Response Summary
--------------------------------------------------------
Outcome        Frequency        Outcome        Frequency
--------------------------------------------------------
   0              19               1              13
--------------------------------------------------------
```

`glm()` itself still fits the three-level response (that is outside blorr);
blorr now refuses to summarise it.

## Unchanged binary outputs (values captured before the change)

A script saved the outputs of binary fits from the unmodified code (working
tree at `bc76c35`), then re-ran them with the fix loaded through
`pkgload::load_all()`:

```text
num_out    identical: TRUE     # print(blr_regress(honcomp ~ female + read + science, hsb2))
fact_out   identical: TRUE     # print(blr_regress(hc_lab ~ female + read)) two-level labelled factor
num_prof   identical: TRUE     # resp_prof
fact_prof  identical: TRUE     # resp_prof, labels "no"/"yes"
gains      identical: TRUE     # blr_gains_table()$decile_table
gains_out  identical: TRUE     # print(blr_gains_table())
cm         identical: TRUE     # blr_confusion_matrix() statistics
cm_out     identical: TRUE     # print(blr_confusion_matrix())
fs         identical: TRUE     # blr_model_fit_stats()$fit_stats
pf_num     identical: TRUE     # resp_profile() numeric 0/1
pf_fact    identical: TRUE     # resp_profile() labelled factor
full mtcars binary print identical: TRUE
```

## Tests

New file `tests/testthat/test-binary-response.R` (4 tests, 8 assertions):

- `resp_profile()` errors for a three-level factor response, message names
  `ses3` and `3 levels`.
- `blr_regress()` errors with the same message.
- `print_blr_reg()` errors when handed a three-outcome profile.
- binary responses unchanged: numeric 0/1 (147/53), labelled two-level factor
  (`no` 147, `yes` 53), logical (19/13), and the exact six-line response
  summary block for `am ~ wt + hp` on mtcars, captured before the change.

Before-fix check (fix temporarily reverted, new test file kept):

```text
[ FAIL 3 | WARN 0 | SKIP 0 | PASS 8 ]
── Failure ('test-binary-response.R:16:3'): resp_profile errors for a three-level factor response (#130)
Expected `resp_profile(m)` to throw a error.
── Failure ('test-binary-response.R:25:3'): blr_regress errors for a three-level factor response (#130)
Expected `blr_regress(ses3 ~ female + read, data = d)` to throw a error.
── Error ('test-binary-response.R:35:3'): print_blr_reg errors when the stored profile is not binary (#130)
Error in `round(x$est, 4)`: non-numeric argument to mathematical function
```

After the fix, the full suite:

```text
TOTALS: files = 26  test = 1998  failed = 0  error = 0  warning = 0  skipped = 0
```

## Check results

`R CMD build` then `R CMD check --as-cran --no-manual` on the tarball with
`_R_CHECK_FORCE_SUGGESTS_=false`:

```text
Status: 2 NOTEs   (0 errors, 0 warnings)
```

NOTEs, both environmental and not caused by this change:

1. `checking CRAN incoming feasibility ... NOTE` — maintainer line, "Version
   contains large components (0.3.2.9000)", "Days since last update: 2". The
   version is the dev version already in `DESCRIPTION`; no bump was made (this
   is a patch-round build, CRAN after 2027-01-13).
2. `checking for hidden files and directories ... NOTE` — `Found the following
   hidden files and directories: .git`. A git worktree stores `.git` as a file
   and `R CMD build` includes it; not present when building from a clean
   archive.

## Commands run

```text
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/c25-document.R     # devtools::document()
Rscript C:/Users/DELL/AppData/Local/Temp/opencode/c25-test.R         # devtools::test(), totals above
R CMD build .
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran --no-manual blorr_0.3.2.9000.tar.gz
Rscript .../c25-reprex-before.R                                      # pre-fix reprex
Rscript .../c25-reprex-final.R                                       # post-fix reprex
Rscript .../c25-before-values.R                                      # capture binary outputs
Rscript .../c25-compare.R                                            # before/after identical check
Rscript .../c25-t-new.R                                              # new tests against pre-fix code (3 failures)
```

`devtools::document()` was run. The installed roxygen2 is 8.1.1 while
`DESCRIPTION` records `RoxygenNote: 7.3.2`; 8.1.1 rewrites `NAMESPACE` (one
`importFrom` per line) and adds an "Authors" section to `man/blorr.Rd`. Those
generated files were restored to `HEAD` to keep this patch focused, after
confirming that with the new tags `document()` keeps `theme` and
`element_text` in `NAMESPACE`. Test runs regenerate `tests/testthat/Rplots.pdf`
and touch snapshot line endings; those artifacts were also restored.

## Decisions

- Guard the actual internal names (`resp_profile`, `print_blr_reg`) plus the
  shared model gate, not a new `blr_resp_profile()` export; the issue's name
  does not exist in `NAMESPACE`.
- `blr_check_supported()` rejects only >2 levels, preserving the existing,
  tested all-zero-response support in `blr_confusion_matrix()`. The profile
  and printer require exactly two because they print two outcomes.
- Data-based bivariate functions are left alone (listed above); they are a
  separate API with a separate test story.

## Follow-ups

1. Add the binary-response guard to the data-column bivariate entry points:
   `blr_bivariate_analysis()`, `blr_segment()`, `blr_segment_dist()`,
   `blr_segment_twoway()`, `blr_woe_iv()` and `blr_woe_iv_stats()`. They call
   `event_indicator()` without a model, so today a three-level factor response
   is read as "level 2 = event" and yields silently wrong event rates, WoE and
   IV. Reuse `blr_check_binary_response()` on the named response column and add
   tests mirroring `test-binary-response.R`.
2. Regenerate and commit documentation with roxygen2 >= 8 (or pin
   `RoxygenNote` to the version in use) in a separate housekeeping commit.
   roxygen2 8.1.1 reformats `NAMESPACE` and rewrites all `man/*.Rd` even
   though no documentation text changed; the untagged `theme`/`element_text`
   imports were fixed here, but the generated files were restored to keep this
   diff reviewable.
3. Mention "the response must have exactly two levels" in the shared
   `@param model` documentation text when the docs are next regenerated.
4. Add `^\.git$` to `.Rbuildignore` (or build from a clean archive) so
   `R CMD check` does not NOTE the `.git` file when the source is a git
   worktree.
