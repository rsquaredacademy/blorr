# bfx13: blorr 0.3.2 fix round 13

2026-11-14. Branch `task/blorr-fix-032`; base `da3c0c8`.
Implementation commit: `3adf604` (BFXX12-1 and BFXX12-2 are a tight group:
the shared model contract and its public documentation).

Applied Aravind's 2026-11-14 decision: version 0.3.2 supports binary logistic
regression only, with binomial family and logit link. Other links are rejected;
support for them is not planned for this version. The existing restrictions on
weights, grouped responses and stored model components remain in effect.
This is an intentional API restriction required by the fix: unsupported
family/link inputs now stop. Function signatures and supported logit results
are unchanged.

| Finding | Test added / before evidence | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| BFXX12-1 (High) | `tests/testthat/test-bfx13.R:17` reproduces both audited hsb2 cases. Before the fix, all eight expected errors failed: R2, single/multi-model fit statistics and the odds table accepted probit/cloglog. | `R/blr-utils.R:21` checks both family and link; `R/blr-error-messages.R:26` always invokes that shared guard for a GLM, replacing the former non-binomial advice without a stop. | Exact error checks across all 51 namespace-enumerated public GLM entry points; selection rejects before refit preparation; independent logit value assertions, existing snapshots and base-to-fixed example/vignette comparison. | Resolved by the approved rejection policy. |
| BFXX12-2 (Low) | Before: NEWS promised grouped zero-trial selection, weighted LR/R2/regression and grouped statistics matching base R; the main regression argument documented neither the restrictions nor storage/refit requirement. | `NEWS.md:30` adds the logit-only contract; superseded support claims are removed. `R/blr-regress.R:5` and `man/blr_regress.Rd:13` document the supported family/link, weights/grouped restriction and explicit storage/refit requirement. Shared restriction text is updated in the other 19 roxygen source files and 48 Rd files. | Reviewed the complete 0.3.2 NEWS section, matched the argument text in source/Rd, searched README and vignettes, executed examples and vignette, and ran the clean archive check. | Fixed. |

## Before and after: the audit reproducer

The reproducer was executed against a private installation of an unmodified
`git archive da3c0c8` and again against the fixed archived sources, using R 4.6.1
ucrt on Windows. Logs are `.bfx13-tmp/baseline-probe.log` and
`.bfx13-tmp/current-probe.log`. No dependencies were installed.

```r
library(blorr)
m <- glm(honcomp ~ female + read + science, hsb2,
         family = binomial(link = "probit"))
eta <- m$linear.predictors
v <- mean((eta - mean(eta))^2)
blr_rsq_mckelvey_zavoina(m)
blr_model_fit_stats(m)$mckelvey
blr_regress(m, odd_conf_limit = TRUE)
```

Before, probit returned `0.2588369786434366` from both R2 paths, whereas the
normal-latent reference `v / (v + 1)` is `0.5346506968796372`. The female
exponentiated coefficient was `2.277618407264136`; odds ratios calculated from
predictions on hsb2 rows 1 and 10, setting female to each level, were
`4.386081606109337` and `4.187369942593873`.

The nearby cloglog case returned `0.3812202481158881` from both R2 paths versus
`0.5520050095354564` using residual variance `pi^2 / 6`. Its female exponentiated
coefficient was `3.293405554188865` versus probability-derived odds ratios
`3.680829389134466` and `3.786087899946078`. The cauchit case also accepted the
fit before the fix (R2 `0.6330435110734393`, exponentiated female coefficient
`8.055463765390543`).

After, all three public calls above stop, without returning those statistics:

```text
blorr supports binary logistic regression only (binomial family, logit link). This model uses the 'binomial' family and the 'probit' link.
```

Cloglog and cauchit produce the identical template with their actual link name.
Other families report both the actual family and actual link. In particular,
quasibinomial with a logit link is rejected rather than receiving advice and
continuing as it did before this fix.

The logit control retains R2 `0.5177763741943727`, matching the independent
population-variance calculation with residual variance `pi^2 / 3`, and female
odds ratio `4.403930926218576`. All three predictor odds estimates and the
profile limits remain covered by the existing regression tests/snapshot.

## Tests and documentation verification

The new file has four blocks and 412 passing expectations:

- Eight rejection assertions reproduce the audit's probit/cloglog single and
  multi-model R2 and odds paths.
- Independently enumerating exports by first formal argument (`model`,
  `full_model`, `object`) finds exactly 51 public GLM entry points. Six fitted
  fixtures across all 51 give 306 exact `conditionMessage()` comparisons:
  probit, cloglog, cauchit, quasibinomial/logit, Gaussian/identity and Poisson/log.
  There are another 24 assertions for both LR positions and later multi-model
  positions. Fixtures are fitted without suppressing warnings.
- All six selection procedures reject each of the three non-logit binomial
  fixtures before `selection_data()` is reached; that preparation function is
  mocked to stop with a distinct error if reached (18 assertions).
- Independent logit R2 and odds values are asserted, and all 51 ordinary logit
  entry-point calls complete successfully.

The first full run exposed a problem in the newly added Gaussian fixture:
hsb2's response is a factor. That produced three warnings and an error while
constructing the fixture. Converting the matrix-test response to numeric 0/1
fixed the test setup; the subsequent full run is the reported clean result.
No warnings were suppressed to obtain the clean result.

Final `devtools::test()` with `NOT_CRAN=true`:

```text
213 test blocks
     nb  failed   error warning skipped  passed
   1870       0       0       0       0    1870
```

The test runner rewrote snapshot line endings and deleted its existing unused
`forward-p.md` snapshot. Those runner changes were restored from Git. There is
no committed snapshot change and no change to the existing example models.
The initial red run's generated `tests/testthat/Rplots.pdf` was also restored;
subsequent graphics were directed into `.bfx13-tmp/`.

README.md, README.Rmd and `vignettes/introduction.Rmd` contain only logit GLM
examples, with no probit/cloglog or weighted/grouped-binomial usage. The
vignette's reference to grouping scores into deciles is unrelated to grouped
binomial models. No example text needed replacement. NEWS retains the valid
transformed-response, raw-newdata, offsets, contrasts, omission-padding and
exact-restoration notes, without claiming other-link or weighted/grouped support.

## Baseline comparison

Compared private installations from archived `da3c0c8` and `3adf604`:

```text
Example scripts: 64
Runnable example scripts: 63
Example expressions: 170
Vignette expressions: 27
Differences: 0
```

The one non-executable example is retained as an empty/commented script in the
comparison; dontrun code is not executed. Equality is exact (`identical()`, with
`all.equal(..., tolerance = 0)` available to diagnose a difference). There were
zero execution errors. Printed output, returned values and plot data are equal.
No numerical field or overview correction was excluded. Logs and RDS results:
`.bfx13-tmp/baseline-output.*`, `.bfx13-tmp/current-output.*` and
`.bfx13-tmp/comparison.log`.

## Clean archive build and check

An unmodified `git archive` of implementation commit `3adf604` was built with
`R CMD build --no-manual src` (including vignette generation), followed by
`R CMD check --as-cran --no-manual blorr_0.3.2.tar.gz` with
`_R_CHECK_FORCE_SUGGESTS_=false`. Result: **0 errors, 0 warnings, 1 NOTE**.

```text
* checking examples ... [22s] OK
* checking examples with --run-donttest ... [50s] OK
* checking tests ...
  Running 'testthat.R' [54s]
 [54s] OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE
```

The installed check-mode test suite reports:

```text
[ FAIL 0 | WARN 0 | SKIP 49 | PASS 1813 ]
```

The 49 skips are the existing 47 on-CRAN skips and two tests requiring source
Rd files. The sole NOTE is incoming feasibility's expected maintainer change:

```text
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

There are no other NOTE, WARNING or ERROR entries. DESCRIPTION and
`cran-comments.md` need no change for this result. The tarball contains 237
entries and no scratch, audit, docs-dev, revdep or Rcheck paths.

Implementation-run evidence is retained in `.bfx13-tmp/validation/build.log`,
`check.log`, `blorr.Rcheck/00check.log` and
`blorr.Rcheck/tests/testthat.Rout`. After committing this report, the same
build/check sequence is repeated on a fresh `git archive HEAD` in
`.bfx13-tmp/final/`, so the required final check occurs after the last commit.
That run's logs are retained separately and its outcome is stated in the
final response. The report-only commit changes no package input.

## Commands and retained evidence

All scratch, private libraries, R temporary files, archives, builds and logs
are inside `.bfx13-tmp/`. The wrapper used for substantive R commands was:

```sh
export LC_ALL=C LANG=C NOT_CRAN=true
export TMPDIR="$PWD/.bfx13-tmp" TMP="$PWD/.bfx13-tmp" TEMP="$PWD/.bfx13-tmp"
bash ~/.claude/bin/heavy-slot.sh pkg-bfx13 <command> <arguments>
```

The heavy-slot wrapper reports that this task already holds the queue job's
slot (`pkg-bfx13`). Explicit C locale settings avoid inherited Windows locale
startup warnings; package and test warnings remain visible.

Commands run that created or changed files, in addition to the documented
source/test/report edits:

```text
New-Item -ItemType Directory -Path .bfx13-tmp -Force
Rscript .bfx13-tmp/red.R
Rscript .bfx13-tmp/test.R
python .bfx13-tmp/update-docs.py
python .bfx13-tmp/archive.py da3c0c8 baseline
R CMD INSTALL --library=.bfx13-tmp/baseline-lib .bfx13-tmp/baseline/src
Rscript .bfx13-tmp/probe.R .bfx13-tmp/baseline-lib
Rscript .bfx13-tmp/capture.R .bfx13-tmp/baseline-lib .bfx13-tmp/baseline/src .bfx13-tmp/baseline-output.rds
git restore -- tests/testthat/Rplots.pdf
git restore -- tests/testthat/_snaps
git add NEWS.md R man tests/testthat/test-bfx13.R
git commit -m "fix: reject unsupported links and align docs for BFXX12-1/2 (bfx13)"
python .bfx13-tmp/archive.py HEAD validation
R CMD INSTALL --library=.bfx13-tmp/current-lib .bfx13-tmp/validation/src
Rscript .bfx13-tmp/probe.R .bfx13-tmp/current-lib
Rscript .bfx13-tmp/capture.R .bfx13-tmp/current-lib .bfx13-tmp/validation/src .bfx13-tmp/current-output.rds
Rscript .bfx13-tmp/compare.R
bash .bfx13-tmp/run-heavy.sh bash .bfx13-tmp/build-check.sh validation
git add docs-dev/bfx13-report.md
git commit -m "docs: record BFXX12-1/2 validation results (bfx13)"
python .bfx13-tmp/archive.py HEAD final
bash .bfx13-tmp/run-heavy.sh bash .bfx13-tmp/build-check.sh final
```

The archive helper runs `git archive --format=zip <ref>` and extracts it to
`.bfx13-tmp/<label>/src`. The documentation helper replaces only the existing
shared restriction sentence, preserving each file's line endings. Documentation
was updated in source and Rd together; `devtools::document()` was not run.

For each build/check label, the build helper changes to
`.bfx13-tmp/<label>/`, sets `_R_CHECK_FORCE_SUGGESTS_=false` and `NOT_CRAN=false`,
and runs these commands sequentially (inside the heavy-slot wrapper):

```sh
R CMD build --no-manual src > build.log 2>&1
R CMD check --as-cran --no-manual blorr_0.3.2.tar.gz > check.log 2>&1
```

The capture helper uses `tools::Rd2ex()` with donttest examples enabled and
dontrun examples commented, then evaluates the parsed expressions with a fixed
seed and options in a fresh environment for each script. It also uses
`knitr::purl()` to extract the executable vignette code. It records printed
output, visibility, returned data and plot data/layer data/labels; environments
and function closures are replaced by stable markers. It does not compare
raster pixels or process-specific environments. The two versions run in
separate R processes using their respective private package installations.

## Deferred and follow-ups

None of the two findings is deferred. There are no required follow-ups from
this fix round. Support for other links and for weighted/grouped models is
outside the approved 0.3.2 contract. No dependency, version, maintainer or native
code change was made. No push, GitHub action, branch switch, or change to other
worktrees was performed. The pre-existing untracked `.bfxx*` folders, audit
reports and `build.log` were left alone.

Reverse-dependency checks were not requested or repeated in this round. The
historical two-package reverse-dependency statement in `cran-comments.md` was
not independently recertified here.

## Files changed

74 files, including this report:

- `NEWS.md`
- `R/blr-backward-elimination.R`
- `R/blr-collinearity-diagnostics.R`
- `R/blr-error-messages.R`
- `R/blr-forward-selection.R`
- `R/blr-gains-table.R`
- `R/blr-hosmer-lemeshow-test.R`
- `R/blr-linktest.R`
- `R/blr-lorenz-curve.R`
- `R/blr-lrtest.R`
- `R/blr-model-fit-stats.R`
- `R/blr-model-validation.R`
- `R/blr-multi-model-fit-stats.R`
- `R/blr-pairs.R`
- `R/blr-plots-data.R`
- `R/blr-plots.R`
- `R/blr-regress.R`
- `R/blr-residual-diagnostics.R`
- `R/blr-stepwise-backward-regression.R`
- `R/blr-stepwise-forward-regression.R`
- `R/blr-stepwise-regression.R`
- `R/blr-stepwise-selection.R`
- `R/blr-utils.R`
- `man/blr_coll_diag.Rd`
- `man/blr_confusion_matrix.Rd`
- `man/blr_gains_table.Rd`
- `man/blr_gini_index.Rd`
- `man/blr_linktest.Rd`
- `man/blr_lorenz_curve.Rd`
- `man/blr_model_fit_stats.Rd`
- `man/blr_multi_model_fit_stats.Rd`
- `man/blr_pairs.Rd`
- `man/blr_plot_c_fitted.Rd`
- `man/blr_plot_c_leverage.Rd`
- `man/blr_plot_deviance_fitted.Rd`
- `man/blr_plot_deviance_residual.Rd`
- `man/blr_plot_dfbetas_panel.Rd`
- `man/blr_plot_diag_c.Rd`
- `man/blr_plot_diag_cbar.Rd`
- `man/blr_plot_diag_difchisq.Rd`
- `man/blr_plot_diag_difdev.Rd`
- `man/blr_plot_diag_fit.Rd`
- `man/blr_plot_diag_influence.Rd`
- `man/blr_plot_diag_leverage.Rd`
- `man/blr_plot_difchisq_fitted.Rd`
- `man/blr_plot_difchisq_leverage.Rd`
- `man/blr_plot_difdev_fitted.Rd`
- `man/blr_plot_difdev_leverage.Rd`
- `man/blr_plot_fitted_leverage.Rd`
- `man/blr_plot_leverage.Rd`
- `man/blr_plot_leverage_fitted.Rd`
- `man/blr_plot_pearson_residual.Rd`
- `man/blr_plot_residual_fitted.Rd`
- `man/blr_prep_lorenz_data.Rd`
- `man/blr_regress.Rd`
- `man/blr_residual_diagnostics.Rd`
- `man/blr_rsq_adj_count.Rd`
- `man/blr_rsq_count.Rd`
- `man/blr_rsq_cox_snell.Rd`
- `man/blr_rsq_effron.Rd`
- `man/blr_rsq_mcfadden.Rd`
- `man/blr_rsq_mcfadden_adj.Rd`
- `man/blr_rsq_mckelvey_zavoina.Rd`
- `man/blr_rsq_nagelkerke.Rd`
- `man/blr_step_aic_backward.Rd`
- `man/blr_step_aic_both.Rd`
- `man/blr_step_aic_forward.Rd`
- `man/blr_step_p_backward.Rd`
- `man/blr_step_p_both.Rd`
- `man/blr_step_p_forward.Rd`
- `man/blr_test_hosmer_lemeshow.Rd`
- `man/blr_test_lr.Rd`
- `tests/testthat/test-bfx13.R`
- `docs-dev/bfx13-report.md`
