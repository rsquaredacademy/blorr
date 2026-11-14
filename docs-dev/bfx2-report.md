# blorr 0.3.2 re-audit fix report (`bfx2`)

Worktree: `F:/R/Packages/statistics/blorr-wt/fix-032` (`task/blorr-fix-032`,
base `0c83913`, round-1 HEAD `8ea0d5a`). No push, branch change, GitHub access
or other worktree was used. All eight remaining findings in
`F:/pm/notes/2026-11-01-pkg-audits/bfxx-reaudit.md` (BF1-BF8) are fixed, each
with a value-asserting test that reproduced the re-audit case before the fix.
No API change and no new dependency.

## Findings and fixes

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| BF1 (default Gini on labelled/logical responses) | `test-gini-index.R:40` | `R/blr-lorenz-curve.R:60` uses the original fitted response (`model$model[[1]]`) instead of the encoded `model$y` before label mapping | Labelled-factor default and explicit calls both give `0.967611336032`; logical fit gives `0.967611336032` and equals the numeric fit | Fixed |
| BF2 (weighted forward p selection and printed fit) | `test-forward-selection.R:25` (exact weighted case), `:43`, `:60`, `:77` | `R/blr-stepwise-forward-regression.R:170` uses `selection_model(..., fit = model)` for every candidate; detail prints at `:154`, `:202` and the final print at `:229`-`:231` use the fitted objects. Same for backward (`R/blr-stepwise-backward-regression.R:127`, `:156`-`:158`), both-direction (`R/blr-stepwise-regression.R:158`, `:238`, `:283`, `:303`-`:305`) and the AIC procedures (`R/blr-forward-selection.R:268`-`:275`, `R/blr-backward-elimination.R:238`-`:245`, `R/blr-stepwise-selection.R:272`-`:279`) | Weighted seed-1 `am ~ wt + hp + noise` case selects `c("wt", "hp")`; `coef(k$model)` is `22.55392245, -9.13507042, 0.03626057`, equal to an independent weighted `glm(am ~ wt + hp)`; every printed estimate equals the returned model for forward, backward, both and AIC selection | Fixed |
| BF3 (grouped-binomial trial weights applied twice) | `test-model-fit-stats.R:197`, `:209` | `R/blr-utils.R:275` (`selection_weights`) divides the trial counts out of `prior.weights` for grouped binomial fits; `:297` (`selection_model`) and `:345` (`i_model`) use it; `:261` (`selection_formula`) quotes the stored response column so a `cbind()` response is taken from the model frame | Null log-likelihood is `-73.348689513827`, equal to `update(m, . ~ 1)`; Cox-Snell is `0.787563611002`; a grouped fit with user weights has the same coefficients and log-likelihood as an independent `glm(..., weights = w)` | Fixed |
| BF4 (LR nesting checked by term names) | `test-blr-lrtest.R:92`, `:102`, `:113` | `R/blr-lrtest.R:105`-`:140` checks the fitted design matrices: reduced columns must be present in the full design with identical values (intercept included) and at least one parameter must differ | Both re-audit cases now error (`am ~ 0 + wt + hp` vs `am ~ wt`; reversed `wt` column); a valid nested pair still gives `lr_ratio 9.116974`, equal to `anova()`; identical models error | Fixed |
| BF4 follow-up (intercept-only fit statistics) | `test-model-fit-stats.R:259` | `R/blr-model-fit-stats.R:371`-`:379` returns the degenerate 0/0/1 LR result for an intercept-only model instead of calling the stricter `blr_test_lr()` | Intercept-only `blr_model_fit_stats()` has `lr_ratio 0`, `lr_df 0`, `dev_df` equal to `df.residual()`; backward/both selection with all predictors removed runs again | Fixed |
| BF5 (unknown held-out labels become non-events) | `test-gini-index.R:159` | `R/blr-utils.R:39`-`:77` validates every non-missing response label against both fitted classes before encoding; `R/blr-utils.R:142` passes the fitted response levels | Numeric `9`, observed factor `"unknown"` and a logical model with a third numeric value all error with `response contains values not present in the fitted model`; declared but unused levels are still accepted | Fixed |
| BF6 (clamped eigenvalues and zero columns) | `test-coll-diag.R:78`, `:99` | `R/blr-collinearity-diagnostics.R:114`-`:129` computes the diagnostics once and passes one SVD-based rank to both consumers; `:168` (`evalue`) keeps zero-norm columns as zeros; `:195` (`cindx`) retains positive eigenvalues; `:218` (`pveindex`) reuses the same rank | Near-collinear full-rank case has a positive eigenvalue and finite condition index `89785.8298103`, equal to the base eigen calculation; a zero column returns an infinite index and `NA` variance proportions without an error; the constant-predictor test still gives `Inf` | Fixed |
| BF7 (deciles assigned before dropping incomplete rows) | `test-blr-gains-table.R:30`, `:51` | `R/blr-gains-table.R:66`-`:68` prepares/filters the response-prediction pairs before `gains_decile_count()`; `R/blr-plots-data.R:53`-`:55` does the same for Lorenz data | Missing outcome: total `31`, events `12`, deciles 1-10 unique and cumulative endpoints `100`; Lorenz data has 12 rows with endpoints `(0, 0)` and `(1, 1)`; missing predictor gives the same row/event counts | Fixed |
| BF8 (Lorenz example correction absent) | `test-gini-index.R:81` (Rd check; skipped when installed docs are unavailable) | `R/blr-plots-data.R:38` and `man/blr_prep_lorenz_data.Rd:22` call `blr_prep_lorenz_data(model)` instead of using `model$data` | Source and Rd examples contain no `model$data`; the documented example runs and returns 12 rows with endpoints `(0, 1)`; `R CMD check` examples are OK | Fixed |

The round-1 report's M3 row said the example had been corrected; it had not.
That claim is corrected here and the example is now actually changed; the
round-1 report is left as a historical record.

## Deferred

- H7's tied-score test still derives its p-value from the function's own
  statistic, and its labelled-response test only asserts non-missing values.
  No wrong value was found in the re-run, so the assertions were not
  strengthened in this round.
- Remote checks (win-builder devel/release, R-hub, macOS) and a fresh reverse
  dependency run were not performed; only the local Windows / R 4.6.1 check is
  reported.
- `tests/testthat/_snaps/forward-p.md` is an unused snapshot that testthat
  deletes during a run. It was restored after each run so this job does not
  carry an unrelated deletion; removing it can be a separate cleanup.
- The interactive `blr_launch_app()` example remains `\dontrun` as intended.

## Checks

Full local suite (`NOT_CRAN=true`, `devtools::test()`):
**19 files, 182 tests, 395 passed expectations, 0 failed, 0 errors,
0 warnings, 0 skipped** (the large 100,000-row pair test ran).

Clean-archive check, from `git archive HEAD` in a temporary directory outside
the worktree:

```text
R CMD build --no-manual src          -> blorr_0.3.2.tar.gz (vignettes OK)
R CMD check --as-cran --no-manual    -> Status: 1 NOTE
```

The only NOTE is CRAN incoming feasibility reporting the intended maintainer
address change:

```text
New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

Examples, examples with `--run-donttest`, tests and vignette rebuilding are
all OK. With `_R_CHECK_CRAN_INCOMING_REMOTE_=false` the same check reports
`Status: OK`; the NOTE is solely the maintainer change. The built tarball
contains no `docs-dev`, `revdep`, `.bfxx` or check directory.

## Commands run that changed something

```powershell
# after each finding: run the new test, see it fail, fix, run it again
$env:NOT_CRAN = "true"
Rscript -e "devtools::test(reporter = 'summary', stop_on_failure = FALSE)"

# clean-archive build and check
git archive --format=tar --output=<tmp>/src.tar HEAD
tar --force-local -xf <tmp>/src.tar -C <tmp>/src
R CMD build --no-manual src
R CMD check --as-cran --no-manual blorr_0.3.2.tar.gz

# remove untracked audit artifacts from the worktree
rm -rf .bfxx
rm -f tests/testthat/Rplots.pdf docs-dev/bfxx-reaudit.md
```

`git checkout -- tests/testthat/_snaps` was used after test runs to discard
line-ending-only rewrites and the unused-snapshot deletion; no snapshot content
was changed.

## Not done

- No remote or reverse-dependency checks were run (see Deferred).
- The H7 assertion-strengthening items were not changed (no wrong result).
- No API, export, dependency or version change was made.
