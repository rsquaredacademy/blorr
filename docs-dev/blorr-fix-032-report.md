# blorr 0.3.2 fix report (`blr-fix-032`)

Worktree: `F:/R/Packages/statistics/blorr-wt/fix-032` (`task/blorr-fix-032` from
`release/v0.3.2`, base `0c83913`). No push, branch change, GitHub access or
other worktree was used.

## Findings and fixes

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| H1 | `test-forward-aic.R:38` (inferior first predictor), `:53` (improving predictor still enters) | `R/blr-forward-selection.R:138` returns the intercept-only model when the best first candidate does not improve AIC; null/selection fits now use `selection_model()` | Audit seed-11 case returns `y ~ 1`, steps 0, AIC/BIC/deviance equal to `update(m, . ~ 1)`; a strong-predictor case still enters `x1` | Fixed |
| H2 | `test-model-fit-stats.R:176`, `:197` | `R/blr-utils.R:250` (`selection_model`), `:298` (`i_model`), `:327` (`model_d_f`); `R/blr-regress.R:50`, `R/blr-regress-compute.R:9` summarise the supplied fit instead of refitting | Weighted `am ~ wt + hp`: LR ratio/df/p equal `anova()`, Cox-Snell/Nagelkerke equal logLik formulas, `blr_regress()` estimates equal `coef(model)` | Fixed |
| H3 | `test-blr-lrtest.R:68`, `:80`; `test-bivariate-analysis.R:214` | `R/blr-lrtest.R:58`, `:82` validate response/observations, weights, offset and nesting; `R/blr-bivariate-analysis.R:83` fits the null on `model.frame(model)` | Missing-gear bivariate LR/df/p equal `anova()`; mismatched and non-nested models error | Fixed |
| H4 | `test-model-fit-stats.R:209` | `R/blr-model-fit-stats.R:260`, `:297`, `:329`, `:363` use aligned fitted values/linear predictors and `model$y` | `na.exclude` and `na.omit` fits give identical Efron/count/adjusted-count/McKelvey-Zavoina values, all non-missing | Fixed |
| H5 | `test-bivariate-analysis.R:228`, `:242` | `R/blr-utils.R:37` keeps missing responses as `NA`; segment/confusion/gains paths filter or exclude missing responses (`R/blr-bivariate-analysis.R:142`, `:294`, `R/blr-model-validation.R:48`, `R/blr-gains-table.R:302`) | Audit example gives segment rates 1 and 0.5; confusion matrix total 31 and accuracy 0.9354839 | Fixed |
| H6 | `test-gini-index.R:123`; `test-bivariate-analysis.R:255` | `R/blr-utils.R:37`, `:91`, `:126`, `:131` encode events from the fitted model and drop unused standalone levels; used by Gini, gains, confusion matrix and WoE/segment | Unused level 9 no longer flips Gini (-0.9676 to 0.9676) or gains event count (13), and confusion matrix is 2x2 | Fixed |
| H7 | `test-hosmer-lemeshow.R:23`, `:39`, `:51` | `R/blr-hosmer-lemeshow-test.R` consistent event mapping, explicit data and complete rows, dynamic df = groups - 2, unavailable if groups < 3; `R/blr-output.R` prints the observed partitions | Explicit `data`, numeric and labelled-factor responses match; tied example has 3 groups, df 1, p 0.4280238; intercept-only has df/p `NA` | Fixed |
| H8 | `test-bivariate-analysis.R:267` | `R/blr-woe-iv.R:177` computes distributions, logs and IV at full precision and rounds only returned columns | Imbalanced audit example is finite (`woe` 9.2104, `iv` 4.6047); low `digits` still finite | Fixed |
| H9 | `test-blr-pairs.R:51` (`skip_on_cran`) | `src/blr-pairs-cpp.cpp:14` wide double counters; `R/blr-pairs.R:49` double tau denominator | 100,000-row balanced fit: `pairs` 2,499,930,304, non-negative components summing to total, `c = (Gini + 1) / 2`, finite tau | Fixed |
| H10 | `test-model-fit-stats.R:229` | `R/blr-model-fit-stats.R:375` uses `df.residual()`; `R/blr-utils.R:327` uses fitted rank | Aliased `am ~ wt + I(2 * wt)`: `dev_df` 30 equals `df.residual()` | Fixed |
| H11 | `test-coll-diag.R:55` | `R/blr-collinearity-diagnostics.R:151` detects zero-variance predictors (tolerance 0, VIF `Inf`); `:183`, `:195` clamp numerical eigenvalues and mark undefined variance proportions `NA` | Constant `z`: VIF `Inf`, `wt` VIF 1; eigen condition indices contain no `NaN`, zero eigenvalue gives `Inf`/`NA` proportions | Fixed |
| M1 | Metadata verified via build/check | `DESCRIPTION:5`, `man/blorr.Rd:25` use `aravindhebbali@rsquaredacademy.com` | `R CMD check` reports the intended new-maintainer note; no old address in package metadata | Fixed |
| M2 | `test-coll-diag.R:70` | `R/blr-collinearity-diagnostics.R:144` keeps matrix dimensions with `drop = FALSE` | Single-predictor VIF/tolerance both 1, no error | Fixed |
| M3 | `test-gini-index.R:136` | `R/blr-plots-data.R:43` defaults to `model$model`, uses fitted rows when `test_data = FALSE`, and corrects the example | Default helper and omitted-row training data return 12 Lorenz rows with endpoints 0 and 1 | Fixed |
| M4 | `test-blr-regress.R:68` | `R/blr-regress.R:50`, `R/blr-regress-compute.R:9` summarise the supplied fit directly | `glm(I(am == 1) ~ wt)` returns estimates equal to `coef(model)` without formula re-evaluation | Fixed |
| L1 | Rendered README checked | `README.md:127` Somers' D 0.7136 and gamma 0.7147 | Values match `blr_pairs()` for the documented hsb2 model | Fixed |
| L2 | `R CMD check` examples, including `--run-donttest` | Six selection examples and the dfbetas panel now run; expensive both-direction examples use `\donttest`; stepwise examples use `family = binomial`; selection docs say class `glm` | `R CMD check` examples OK and examples with `--run-donttest` OK | Fixed |
| L3 | Check output recorded | `cran-comments.md` lists only the local environment actually checked and the current check result | Local `R CMD check --as-cran --no-manual`: 0 errors / 0 warnings / 1 explained note | Fixed |
| L4 | Build from clean archive | `.Rbuildignore:20` excludes `^docs-dev$` | Clean `git archive` build contains no `docs-dev` | Fixed |

## Deferred

- Remote checks (win-builder devel/release, R-hub, macOS) were not run; only the
  local Windows 11 / R 4.6.1 check is reported.
- Reverse dependencies were not rechecked in this job; `cran-comments.md`
  retains the existing midoc/xplorerr claim without independent re-certification.
- The interactive `blr_launch_app()` example remains `\dontrun` as intended.

## Checks

Clean `git archive` copy, `R CMD build --no-manual src`, then
`R CMD check --as-cran --no-manual blorr_0.3.2.tar.gz`:

```
Status: 1 NOTE
```

The only note is CRAN incoming feasibility reporting the intended maintainer
address change:

```
New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

Everything else was OK, including examples, `--run-donttest`, tests and
vignette rebuilding. Check-mode suite:
`[ FAIL 0 | WARN 0 | SKIP 48 | PASS 266 ]`.

Local full suite (`NOT_CRAN=true`, `devtools::test()`): all tests passed, with
the large pair-count test exercised (not skipped locally).

## Commands run that changed something

```powershell
# tests / snapshots
$env:NOT_CRAN = "true"
Rscript -e "devtools::test(reporter = 'summary', stop_on_failure = FALSE)"
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::snapshot_accept(c('bivariate-analysis', 'blr-regress'))"
# the vdiffr woe plot was accepted from its regenerated .new.svg

# clean-archive build and check
git archive --format=tar --output=<tmp>/head.tar HEAD
tar --force-local -xf <tmp>/head.tar -C <tmp>/src
R CMD build --no-manual src
R CMD check --as-cran --no-manual blorr_0.3.2.tar.gz
```

`cran-comments.md` and this report are excluded from the package build.
