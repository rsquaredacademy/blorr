# bfx5: blorr fix round 5 (BFXX4-1, BFXX4-2, BFXX4-3)

Date: 2026-11-02. Worktree `F:/R/Packages/statistics/blorr-wt/fix-032`,
branch `task/blorr-fix-032`, base `0123aa9`. Audit fixed:
`docs-dev/bfxx4-reaudit.md` (Codex round-4 re-audit). Environment: Windows,
R 4.6.1, testthat 3.3.2.

## Files changed

- `R/blr-utils.R`
  - `column_normalize()`: each design column is divided by its largest
    absolute value before its 2-norm is taken, so the norm cannot overflow
    (values ~1e155) or underflow to zero (values ~1e-200). Genuinely zero
    columns stay zero; a non-finite design is an explicit error.
  - `selection_term_exprs()` / `selection_formula(fit, predictors)`: candidate
    formulas are built as language objects from the stored model-frame
    columns each term uses. A variable is a symbol named by its model-frame
    column (`log(wt)`, `body weight`), and an interaction joins its variables
    with `:` (`x:z` -> `` `x`:`z` ``). No string pasting, so no double quoting
    and interactions stay grouped.
  - `selection_restore()`: the stored-frame refit keeps its coefficients, but
    its terms (with the original `predvars`/`dataClasses`), formula, call,
    model frame column names and coefficient names are replaced by the
    original-expression versions, after checking that the original terms
    rebuild exactly the same design matrix from the stored frame. Formula
    offsets are kept. If that check cannot be made, the stored-frame refit is
    returned unchanged (same estimates).
  - `selection_model()` uses both, so every model the six procedures return
    (including intercept-only ones) is restored.
- `tests/testthat/test-blr-lrtest.R`: one new block (BFXX4-1).
- `tests/testthat/test-forward-selection.R`: four new blocks (BFXX4-2/-3),
  each run over all six procedures.
- `NEWS.md`: two existing 0.3.2 bullets extended (selection; LR nesting).

## Reproduction before / after (devtools::load_all)

BFXX4-1, `full = glm(am ~ wt + hp)`, `reduced = glm(am ~ x)`, `x = rev(wt) * s`:

| s | before (HEAD 0123aa9) | after |
| --- | --- | --- |
| 1 | error "must be nested" | error "must be nested" |
| 1e155 | LR 32.83258, df 1, p 1.00447e-08 (wrong) | error "must be nested" |
| 1e-200 | LR 32.83258, df 1, p 1.00447e-08 (wrong) | error "must be nested" |

Nested control (`x = wt * s`, `am ~ x + hp` vs `am ~ x`): LR 9.116974, df 1,
p 0.002532485 at all three scales, before and after (equals `anova()`).

BFXX4-2, seed 1, `glm(am ~ log(wt) + hp + noise)`, `blr_step_p_forward`:

- before: formula `` am ~ `log(wt)` + hp ``; `predict(k$model, mtcars)` ->
  `Error: object 'log(wt)' not found`.
- after: formula `am ~ log(wt) + hp`; coefficients `(Intercept) 19.76650040,
  log(wt) -22.66901764, hp 0.03310056`; raw-data prediction works
  (Mazda RX4 0.828260192, Mazda RX4 Wag 0.370023887, ...), equal to an
  independent `glm(am ~ log(wt) + hp)` with error 0.

BFXX4-3:

- seed 103 `glm(y ~ x*z + noise)`: before `Error: object 'x:z' not found`
  in all six; after p-forward returns `y ~ x + z + noise + x:z`, logLik
  -155.495848540222 (matches the round-3 value in the audit); p-both returns
  `y ~ x + z`.
- `` am ~ `body weight` + hp + noise ``: before `attempt to use zero-length
  variable name`; after `` am ~ `body weight` + hp ``.

Wider check (scratch script, all six procedures, each returned model compared
with an independent `glm()` on its own formula): `log(wt)`, `I(wt^2)`, `x*z`,
`` `body weight` ``, `poly(x, 2) + factor`, `factor * x`, formula `offset(o)`,
source data mutated after fit, local function `lt(wt)`. Every case: coefficient
error 0, identical coefficient names, logLik error 0, raw-newdata prediction
error 0. Also `summary()`, `vcov()`, `confint()` equal the reference and
`update(k$model, . ~ . - hp)` works.

## Tests

- New tests fail before the fix (R/blr-utils.R stashed): 9 failures/errors in
  the 5 new blocks; pass after.
- Full suite (`NOT_CRAN=true`, `devtools::test()`): 19 files, 198 test
  blocks, 606 expectations passed, 0 failed, 0 errors, 0 warnings, 0 skipped.
  (testthat deleted the unused snapshot `_snaps/forward-p.md` and rewrote
  snapshot line endings; both reverted with `git checkout`, not committed.)

## R CMD check

`R CMD check --as-cran blorr_0.3.2.tar.gz` (tarball from `R CMD build
--no-manual` on a `git archive` copy of the working tree; the check itself
built the PDF manual with TinyTeX: "checking PDF version of manual ... OK").
Run with `_R_CHECK_FORCE_SUGGESTS_=false` because V8 is not installed.
**Status: 1 NOTE, 0 warnings, 0 errors.** The NOTE is the expected CRAN
incoming maintainer change (hebbali.aravind@gmail.com ->
aravindhebbali@rsquaredacademy.com).

## Commands run

- `Rscript repro.R` (audit cases, before and after), `Rscript verify.R`
  (all-six reference comparison), `git stash push R/blr-utils.R` + filtered
  `devtools::test()` (fail-before), full `devtools::test()`.
- `git archive $(git stash create) | tar -x`, `R CMD build --no-manual src`,
  `_R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran blorr_0.3.2.tar.gz`
  (in the session scratchpad, outside the worktree).

## Not fixed / judgment calls

- Selection still always fits candidates with an intercept, even when the
  input model has none (pre-existing behaviour, unchanged here).
- Term hierarchy is not enforced: a procedure may keep `x:z` without `x` or
  `z` (pre-existing; the audit also calls this out as not a claim).
- If the original-expression terms cannot reproduce the stored design exactly,
  the stored-frame refit is returned (correct estimates, literal column-name
  formula). No case found that triggers this.

## Follow-ups

1. Decide whether selection on a no-intercept model should keep the model
   without an intercept (statistical/API choice for Aravind; currently every
   candidate has an intercept).
2. Decide whether selection should enforce marginality (keep `x` and `z`
   whenever `x:z` is kept).
3. Codex re-audit of this round (bfxx5) before any CRAN submission.
