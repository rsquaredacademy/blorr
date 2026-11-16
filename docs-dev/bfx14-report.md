# bfx14: blorr 0.3.2 fix round 14

2026-11-14. Branch `task/blorr-fix-032`; base `bf2e27e`.
Implementation commit: `294316c` (fix, tests and NEWS in one commit; this report
is a later documentation-only commit).

## Finding

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| EX1 (wrong selection history) | `tests/testthat/test-bfx14.R:14` p-both history/step/AIC consistency on `stepwise`; `:40` p-forward on the same data; `:60` constructed AIC ties for forward and backward selection | `R/blr-utils.R:675` new `selection_p_index()`; used at `R/blr-stepwise-forward-regression.R:121,182`, `R/blr-stepwise-regression.R:124,176,215,260`, `R/blr-stepwise-backward-regression.R:105`. AIC selectors use `which.min()` at `R/blr-forward-selection.R:163,242`, `R/blr-backward-elimination.R:157`, `R/blr-stepwise-selection.R:155,239` | The EX1 reproduction, independent `AIC(glm(...))` for every history row, a fresh GLM comparison for the final model, and constructed exact-tie cases. Full test suite and clean archive check. | Fixed. |

### What changed

Every selection step now moves exactly one term.

- P-value selectors: among candidates tied at `min(p)` (entry) or `max(p)`
  (removal), the index is chosen by the Wald chi-square statistic from the same
  candidate fits: largest for entry, smallest for removal. A remaining tie keeps
  candidate order. This covers p values that underflow to 0 (and ties at 1).
- AIC selectors: `which.min()` picks the first minimum, so a tie keeps candidate
  order.
- The AIC stepwise (both directions) selector had the same tie bug and the same
  `condition has length > 1` failure on an exact tie, so its two picks were
  fixed with the same `which.min()` rule even though the brief only listed the
  one-direction AIC selectors.

No API change and no new dependency; `selection_p_index()` is internal.

## Before and after: EX1

Before (audit and reproduced on base `bf2e27e`), `blr_step_p_both()` on the
20,000-row `stepwise` data returned six names but three steps:

```r
orders = c("x1", "x2", "x3", "x6", "x6", "x5")
method = c("addition", "removal", "addition")
steps  = 3
aic    = c(16642.373527545, 16640.6533561194, 16639.2187650009)
```

The printed first row said `x1` was added with AIC `16642.374`, while an
independent `glm(y ~ x1)` has AIC `23205.16`; `16642.374` belongs to
`glm(y ~ x1 + x2 + x3 + x6)`. `blr_step_p_forward()` likewise returned five
predictors with only two AIC values (`length(predictors) = 5`,
`length(aic) = 2`).

After:

```r
orders = c("x6", "x1", "x3", "x2", "x6", "x5")
method = c("addition", "addition", "addition", "addition", "removal", "addition")
steps  = 6
predictors = c("x1", "x3", "x2", "x5")
aic = c(18869.627, 18571.376, 18016.724, 16642.374, 16640.653, 16639.219)
```

Each of the six steps now has one name, one method and one AIC, and each AIC is
the independent `AIC(glm(...))` of the terms accumulated by that row. The
initial tie among `x1`, `x2`, `x3` and `x6` (all p = 0) is resolved by the Wald
statistics `3313.31`, `3205.54`, `3352.81` and `4876.36`: `x6` enters first.
The final model has the same terms (`x1`, `x3`, `x2`, `x5`) and the same
coefficients as a fresh GLM; only the formula term order follows the corrected
addition order.

For `blr_step_p_forward()` the result is now `steps = 5`,
`length(predictors) = 5`, `length(aic) = 5`; every AIC row matches an
independent GLM of `predictors[1:i]`, and the final coefficients match a fresh
GLM of `x6 + x1 + x3 + x2 + x5`.

The constructed backward AIC tie (two predictors swapped across duplicated
rows, so removing either has exactly the same AIC) previously stopped with
`the condition has length > 1` inside `if (aics[minc] < aic_f)`; it now removes
`x_a` first by candidate order and records one removal per step.

## Tests

New file `tests/testthat/test-bfx14.R` (three test blocks). It asserts:

- p-both: `steps == length(orders) == length(method) == length(aic) ==
  length(bic) == length(dev)`; every history AIC equals an independent
  `AIC(glm(...))` of that row's accumulated terms; final coefficients equal a
  fresh GLM of the selected terms.
- p-forward on the same data: `steps == length(predictors) == length(aic)`;
  every AIC equals an independent GLM of `predictors[1:i]`; final coefficients
  equal a fresh GLM of the selected terms.
- AIC ties: forward picks the first candidate in candidate order; backward
  removes the first of two exactly tied candidates and moves one term per step.

The red run before the fix failed on the history lengths and wrong AIC values
(`actual 16640.7 / expected 23205.2`, and p-forward `steps 2 / predictors 5`),
and the backward AIC tie errored as described above.

Full `devtools::test()` with `NOT_CRAN=true`:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1898 ]
```

No snapshot content changed. The test runner rewrote snapshot line endings and
deleted its pre-existing unused `forward-p.md` snapshot; both were restored from
Git and are not committed. The generated `tests/testthat/Rplots.pdf` was also
restored.

One existing non-snapshot assertion changed:
`tests/testthat/test-stepwise-selection.R:14` hard-coded the incorrect EX1
history `c("x1", "x2", "x3", "x6", "x6", "x5")`. It now expects the corrected
`c("x6", "x1", "x3", "x2", "x6", "x5")`. No other existing test or snapshot was
modified.

## Examples and vignette comparison

Private installations of clean `git archive bf2e27e` and of the fixed sources
were compared with the same driver used by bfx13 (`capture.R`: every Rd example,
`--run-donttest` code included, plus `vignettes/introduction.Rmd`). All values,
printed output, conditions and plot data were compared exactly.

```text
Example scripts: 64
Runnable example scripts: 63
Example expressions: 170
Vignette expressions: 27
Differences: 1
```

The only difference is `man/blr_step_p_both.Rd`, the EX1 selector history. Its
printed `Variables Entered/Removed` lines now list `x6, x1, x3, x2, x6, x5`
instead of `x6, x6, x5`, its summary has six correctly labelled rows instead of
three, and the final model formula lists the same selected terms in the
corrected addition order (`y ~ x1 + x3 + x2 + x5`; identical coefficients and
AIC). No vignette expression and no other example changed.

## Clean archive check

From a clean `git archive 294316c` in `.bfx14-tmp/finalcheck/src`:

```text
R CMD build --no-manual src
R CMD check --as-cran --no-manual blorr_0.3.2.tar.gz
```

Result: **0 errors, 0 warnings, 1 NOTE**.

```text
* checking examples ... [15s] OK
* checking examples with --run-donttest ... [46s] OK
* checking tests ...
  Running 'testthat.R' [64s]
 [65s] OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [35s] OK
Status: 1 NOTE
```

Installed check-mode test summary: `[ FAIL 0 | WARN 0 | SKIP 49 | PASS 1841 ]`
(the 49 skips are the existing on-CRAN and source-Rd skips). The sole NOTE is
the expected CRAN incoming feasibility maintainer change:

```text
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
```

No other NOTE, WARNING or ERROR appeared. After this report-only commit the
same build/check sequence is repeated on a fresh `git archive HEAD`; its result
is stated in the final response. This report commit changes no package input.

## Commands run that changed something

```sh
# before-fix reproduction and candidate Wald statistics
Rscript .bfx14-tmp/repro.R

# red run, then after the fix (NOT_CRAN=true)
Rscript -e 'devtools::test(filter = "bfx14")'

# full suite (green): [ FAIL 0 | WARN 0 | SKIP 0 | PASS 1898 ]
NOT_CRAN=true bash ~/.claude/bin/heavy-slot.sh pkg-bfx14 Rscript -e 'devtools::test()'

# clean installations for the base/fixed example and vignette comparison
git archive bf2e27e | tar -x -C .bfx14-tmp/baseline-src
git archive HEAD   | tar -x -C .bfx14-tmp/current-src
R CMD INSTALL --no-docs --no-help -l .bfx14-tmp/baseline-lib .bfx14-tmp/baseline-src
R CMD INSTALL --no-docs --no-help -l .bfx14-tmp/current-lib .bfx14-tmp/current-src
Rscript .bfx14-tmp/capture.R .bfx14-tmp/baseline-lib .bfx14-tmp/baseline-src .bfx14-tmp/baseline-output.rds
Rscript .bfx14-tmp/capture.R .bfx14-tmp/current-lib  .bfx14-tmp/current-src  .bfx14-tmp/current-output.rds
Rscript .bfx14-tmp/compare.R

# clean archive build and check
git archive HEAD | tar -x -C .bfx14-tmp/finalcheck/src
bash ~/.claude/bin/heavy-slot.sh pkg-bfx14 bash -c 'cd .bfx14-tmp/finalcheck && R CMD build --no-manual src && R CMD check --as-cran --no-manual blorr_0.3.2.tar.gz'

# commits
git commit -m "fix: add or remove one term per selection step on ties (EX1) (bfx14)"
```

All scratch, logs, private libraries and the check directory are inside
`.bfx14-tmp/`. The untracked `docs-dev/*reaudit.md`, `.bfx*`, `.blrep*` and
`build.log` paths were left untouched.

## Follow-ups

None. EX1 is fully fixed for the p-value selectors (forward, backward, both) and
the AIC selectors (forward, backward, both). There are no deferred findings
from this task and no known remaining tie-driven history mismatch.
