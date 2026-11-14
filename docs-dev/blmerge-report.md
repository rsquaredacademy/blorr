# blorr 0.3.2 merge report (`pkg-blmerge`)

Worktree: `F:/R/Packages/statistics/blorr-wt/fix-032`
(`task/blorr-fix-032`). Audited candidate before the merge: `5fb7d63`.
Merged `origin/develop` (`73408ec`) into it with a real merge commit,
`e1455bc` (parents `5fb7d63` and `73408ec`); no rebase, squash or branch
change. Nothing was pushed and GitHub/`gh` was not used.

The fix branch is the source of truth. Its shared guard
`blr_check_supported()` (`R/blr-utils.R:21`, called from
`R/blr-error-messages.R:26`) rejects weighted, grouped and non-logit models;
fits stored without model frame/response must be refit. Develop's weighted
code paths are therefore dropped, while its compatible non-weighted changes
are ported.

## Develop commits

| Develop commit | Disposition | Evidence / reason |
| --- | --- | --- |
| `868b04d` #138/#129/#139: HL df from group count; null model keeps weights/offset/family; no network at attach | **Ported + already covered** | HL df already computed from the actual groups in the audited branch (`R/blr-hosmer-lemeshow-test.R:66-76`, `df = groups - 2`; `< 3` groups gives `NA`). The null model already keeps the fit's family and offset (`R/blr-utils.R:703-715`). Network access at attach is removed by keeping develop's deletion of `R/zzz.R` and its `tests/testthat/test-attach.R`. |
| `8e9a359` #140: `blr_regress()` keeps weights/offset; non-logit and unsupported weights error; HL print < 10 groups | **Covered / ported / superseded** | `blr_regress.glm()` summarises the supplied fit directly (`R/blr-regress.R:84`), so its own offset is used; weighted fits are rejected by the guard (`R/blr-utils.R:21-51`). Non-logit and grouped/weighted rejection is the shared guard, not a second implementation. HL printing uses `seq_len(nrow(j))` (`R/blr-output.R:297`), kept. |
| `ea7c15d` #141 round 1: formula offsets/environments; weighted HL errors; weighted fit stats | **Ported (offset/environment) / superseded by rejection (weighted)** | Formula environment and formula offsets are preserved because the glm method no longer refits (`R/blr-regress.R:81-84`); the formula method keeps the environment in `R/blr-regress-compute.R:14-21`. Weighted HL and weighted fit statistics are unreachable: every entry point calls `blr_check_model()`/`blr_check_supported()` first (`R/blr-model-fit-stats.R` per-function guard), so develop's weighted values/`NA` paths were removed. |
| `04ac433` #141 round 2: `blr_regress()` model weights/offset vs same-named data columns | **Covered / ported for offsets; weights rejected** | Same direct-summary path (`R/blr-regress.R:84`); for the formula method the supplied vectors are bound under non-clashing names `.blr_weights`/`.blr_offset` (`R/blr-regress-compute.R:11-21`). A weighted formula fit is rejected by the added guard at `R/blr-regress-compute.R:55`; an offset fit is still supported. |
| `56af923` #141 round 2: null model uses model weights/offset with same-named data columns; weighted R2 errors | **Covered for offset / superseded for weights** | `i_model()` passes the model frame plus `selection_weights(model)` and `model$offset` explicitly (`R/blr-utils.R:703-715`), so data columns named `weights`/`offset` are not read as the model inputs. Weighted R2 functions now stop at the shared guard (`R/blr-utils.R:21-51`); develop's separate "not available for prior weights" errors were dropped. |
| `1c2af9c` merge PR #141 | **Merge only** | No independent change; its content is covered by its two parents above. |
| `73408ec` #142: issue forms / CONTRIBUTING | **Ported unchanged** | `.github/ISSUE_TEMPLATE/{bug_report,feature_request,config}.yml` and `.github/CONTRIBUTING.md` kept as merged. |

## Auto-merged hunks reviewed against `5fb7d63`

| File / hunk | Kept or dropped | Reason |
| --- | --- | --- |
| `R/blr-backward-elimination.R`, `R/blr-forward-selection.R`, `R/blr-linktest.R`, `R/blr-stepwise-selection.R`: added `blr_check_weights_offset(model)` | **Dropped** | That helper was part of develop's weighted/offset rejection and no longer exists after the fix branch's guard was kept. Weighted models are already rejected by `blr_check_model()`; offsets remain supported by the audited branch. |
| `R/blr-lrtest.R`: `lr <- deviance(reduced) - deviance(full)` replacing the log-likelihood difference | **Dropped** | Develop's change was for weighted fits, which are now rejected. It also introduced ~1e-13 numeric drift in bivariate LR values and the vignette (saturated-deviance cancellation); restoring the audited `mll()` difference made examples/vignette identical to `5fb7d63`. |
| `R/blr-model-fit-stats.R`: `weighted <- has_prior_weights()`, `NA` values for count/Efron/McKelvey, `effective_n()`, `blr_check_prior_weights()` calls | **Dropped** | Weighted models never reach these functions (shared guard). Keeping the hunks referenced helpers removed with develop's error messages and contradicted the 0.3.2 contract. |
| `R/blr-output.R`: prior-weight `NA` footer in `print_model_fit_stats()` | **Dropped** | No weighted fit statistic can produce those `NA`s now; the audited print ending is restored. |
| `R/blr-regress-compute.R`: weights/offset arguments and formula-environment binding (`.blr_weights`, `.blr_offset`) | **Kept, guarded** | Compatible with the fix branch: offsets are supported and formula environments are preserved. Added `blr_check_model(model)` at `:55` so a weighted/grouped formula fit is rejected by the same guard as every other entry point. |
| `R/blr-regress.R`: `weights`/`offset` arguments on the default method | **Kept, guarded** | Develop feature that does not conflict with the decisions; weighted input is rejected, offset input works. The glm method keeps the fix branch's direct summary (`:81-84`) rather than develop's refit (`refit_formula()` is gone). |
| `R/zzz.R` deleted; `tests/testthat/test-attach.R` added | **Kept** | No network access at attach, as required by #138/#129/#139. |
| `tests/testthat/test-linktest.R`, `test-forward-selection.R`: weighted-model error tests | **Kept, expectation converted** | They now expect the shared guard's "unweighted 0/1" error instead of develop's removed "Weights are not supported". |
| `tests/testthat/test-blr-lrtest.R`, `test-blr-regress.R`, `test-model-fit-stats.R`, `test-hosmer-lemeshow.R`: develop tests | **Kept, weighted-result tests converted** | Offset and null-model/family tests are kept unchanged where compatible. Tests that expected weighted results now expect the guard error; the offset-only parts stay. See the test table below. |
| `tests/testthat/test-model-fit-stats.R`: `null model uses the model when the data has a weights column` | **Kept** | The model is unweighted; a predictor is named `weights`. It protects the same-named-column behaviour without implying weighted support. |
| `tests/testthat/_snaps/forward-p.md` deleted | **Kept deleted** | Develop removed the snapshot test; no `expect_snapshot()` in the current suite refers to the forward-p snapshot. No other test or snapshot was deleted. |
| `man/*.Rd` auto-merged from develop | **Adjusted** | `man/blr_regress.Rd` documents the kept arguments and direct-summary details; `man/blr_model_fit_stats.Rd` no longer advertises weighted `NA` behaviour; `man/blr_test_hosmer_lemeshow.Rd` describes the audited df/`< 3` group behaviour. |
| `NEWS.md` auto-merged top bullets | **Consolidated** | One 0.3.2 section; develop's weighted-support bullets removed, compatible attachment/offset/null-model bullets kept, duplicates merged. No weighted/grouped/non-logit support is claimed. |

## Tests

All develop tests that still apply were kept. Converted tests and their new
expectations:

| Test | Change |
| --- | --- |
| `test-blr-lrtest.R`: "null model retains weights, offset, and family" | Weighted fit now expects `unweighted 0/1`; offset and probit parts kept. |
| `test-blr-regress.R`: "keeps weights and offset of the fitted model" | Weighted fit now expects `unweighted 0/1`; offset estimates still compared with `summary()`. |
| `test-blr-regress.R`: formula/argument offset test | Offset cases kept; weighted formula case now expects the guard error. |
| `test-blr-regress.R`: weights/offset same-named data columns | Rewritten as unweighted offset models so the column-name protection remains tested; weighted variants assert the guard error. |
| `test-blr-regress.R`, `test-linktest.R`, `test-forward-selection.R`: weighted-model tests | Expect the shared guard message ("unweighted 0/1"). |
| `test-model-fit-stats.R`: weighted fit statistics and weighted R2 functions | Now assert that `blr_model_fit_stats()` and each R2 function reject the weighted model with the shared guard. |
| `test-hosmer-lemeshow.R`: weighted model | Expects the shared guard instead of develop's HL-specific message. |
| `test-hosmer-lemeshow.R`: df adapts / print < 10 groups | Kept unchanged; both already pass with the audited implementation. |

Deleted test artifact: only `tests/testthat/_snaps/forward-p.md`, for the reason
above. The test runner's regenerated `tests/testthat/Rplots.pdf` and
line-ending-only snapshot rewrites were restored from Git and are not
committed.

## Examples and vignette comparison

Private installations of clean `git archive 5fb7d63` and of the merged sources
were compared with the same driver used for the previous audits: every Rd
example (including `--run-donttest` code) plus
`vignettes/introduction.Rmd`, comparing values, printed output, conditions and
plot data exactly.

```text
Example scripts: 64
Runnable example scripts: 63
Example expressions: 170
Vignette expressions: 27
Differences: 0
```

An intermediate state differed in two bivariate LR values and the vignette by
relative ~1e-13 because the auto-merged `deviance()` LR formula was used; that
hunk was dropped, and the final comparison is exactly identical to `5fb7d63`.

## Checks

Local full suite (`NOT_CRAN=true`, `devtools::test()`):

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1952 ]
```

Clean `git archive HEAD` -> `R CMD build --no-manual src` ->
`R CMD check --as-cran --no-manual blorr_0.3.2.tar.gz`:

```text
* checking examples ... [20s] OK
* checking examples with --run-donttest ... [52s] OK
* checking tests ... Running 'testthat.R' [73s] OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [39s] OK
Status: 1 NOTE
```

Check-mode suite: `[ FAIL 0 | WARN 0 | SKIP 49 | PASS 1895 ]`. The only NOTE
is the expected CRAN incoming feasibility maintainer change:

```text
New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

`.Rbuildignore` now also excludes `^\.hz-` (line 21); `docs-dev` is already
excluded.

## Commands run that changed something

```sh
git merge origin/develop            # real merge commit e1455bc
git checkout --ours -- <audited R files>   # keep the fix branch's guard/behaviour
# conflict resolution: NEWS.md, error messages, HL, plots data, regress,
# stepwise guards, utils (i_model), and the five conflicted test files
git checkout --ours -- R/blr-model-fit-stats.R hunks  # drop weighted paths
git checkout 5fb7d63 -- NAMESPACE DESCRIPTION          # avoid roxygen-version churn
Rscript -e "devtools::document()"                     # then kept only the 3 changed Rd files
NOT_CRAN=true bash ~/.claude/bin/heavy-slot.sh pkg-blmerge \
  Rscript -e "devtools::test(reporter='silent')"
git archive 5fb7d63 | tar -x -C .blmerge-tmp/baseline-src
bash ~/.claude/bin/heavy-slot.sh pkg-blmerge R CMD INSTALL --no-docs --no-help \
  -l .blmerge-tmp/baseline-lib .blmerge-tmp/baseline-src
bash ~/.claude/bin/heavy-slot.sh pkg-blmerge R CMD INSTALL --no-docs --no-help \
  -l .blmerge-tmp/current-lib .
Rscript .blmerge-tmp/capture.R ...   # baseline and merged outputs
Rscript .blmerge-tmp/compare.R
git archive HEAD | tar -x -C .blmerge-tmp/finalcheck/src
bash ~/.claude/bin/heavy-slot.sh pkg-blmerge bash -c \
  'cd .blmerge-tmp/finalcheck && R CMD build --no-manual src && R CMD check --as-cran --no-manual blorr_0.3.2.tar.gz'
```

Scratch output is confined to `.blmerge-tmp/`; untracked `docs-dev/*reaudit.md`,
`.bf*`/`.blrep*` folders and `build.log` were left untouched.
