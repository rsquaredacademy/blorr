# bfx8: blorr fix round 8 (BFXX7-1, BFXX7-2, BFXX7-3)

Date: 2026-11-02. Worktree `F:/R/Packages/statistics/blorr-wt/fix-032`,
branch `task/blorr-fix-032`, base `ea9055b`. Audit fixed:
`docs-dev/bfxx7-reaudit.md` (left untracked with `.bfxx7/`, untouched).
Environment: Windows, R 4.6.1, testthat 3.3.2.

## Files changed

- `R/blr-utils.R`
  - BFXX7-1: `design_contains()` now compares column **spaces**. Each
    design (estimable columns on the informative rows) is
    column-normalized, and an orthonormal basis of each space is taken
    from its SVD. Each reduced basis vector is projected onto the full
    basis; the residual 2-norm is the sine of its angle to the full
    space. The tolerance is
    `8 * eps * sqrt(n) * (kappa_full + kappa_reduced)`, the rounding error
    such a residual can carry, where kappa are the condition numbers of
    the normalized designs. A space does not change when a column is
    translated by another column (for example a constant with an
    intercept) or rescaled. Only the conditioning, and so the tolerance,
    changes. The old absolute `sqrt(eps) * sqrt(n)` tolerance is gone; it
    was what let a non-nested direction that is small relative to a large
    constant pass. If the tolerance is not small (>= 1e-4, a severely
    ill-conditioned design), the result is marked unreliable and the LR
    test rejects with a clear message.
  - BFXX7-2: `predictor_mine()` (SE, z, p) matches `summary.glm` rows to
    coefficient names, with NA for aliased coefficients.
    `predictor_df()` gives 1 per estimated coefficient and 0 per aliased
    one. All summary vectors now have one entry per coefficient name.
  - BFXX7-3: `selection_restore_impl()` rebuilds each selected term from the
    original `variables` language objects and the factor incidence. It no
    longer uses `str2lang()` on display labels, which rounded constants.
    The design comparison is now `design_equal()`: column-wise, within
    64 * eps of each column's scale, with matching NA patterns. A real
    mismatch still fails, and selection then stops with the bfx7 error.
- `R/blr-lrtest.R`: the reduced design is also cut to its estimable columns
  (`fitted_design()`). The unreliable case gets its own error message.
- `R/blr-output.R`: the odds estimate column width ignores NA, so aliased
  odds tables print. The width is unchanged otherwise, so snapshots are
  unchanged.
- Tests: `test-blr-lrtest.R` (1 block), `test-blr-regress.R` (1 block),
  `test-forward-selection.R` (2 blocks; one of them covers the intentional
  restore-error branch).
- `NEWS.md`: two new bullets (aliased summaries by name; exact constants in
  selection). The nesting bullet is extended (translation).

## Reproduction before (ea9055b) / after

BFXX7-1: full `am ~ wt + hp`, reduced `am ~ z`, `z = rev(wt) + shift`.

| shift | before | after |
| --- | --- | --- |
| 0 | error | error |
| 1e6 | error | error |
| 1e7, 1e8, 1e9 | LR 32.83258, df 1, p 1.00e-08 (wrong) | error "must be nested" |

Valid controls are accepted before and after at every shift, with LR
9.116974 (= `anova()`): reduced `am ~ I(wt + shift)`, and the shifted column
placed in the full model instead. No-intercept null check
(`am ~ 0 + I(wt + shift) + hp`): before, LR 2.002166 at 1e7/1e8/1e9 (wrong);
after, unavailable with the "not nested" warning at every shift.

BFXX7-2 (`am ~ I(0 * wt) + wt + hp`), `blr_regress()` rows after:

| parameter | df | est | se | z | p |
| --- | --- | --- | --- | --- | --- |
| (Intercept) | 1 | 18.8662987 | 7.44355806 | 2.534581 | 0.011258199 |
| I(0 * wt) | 0 | NA | NA | NA | NA |
| wt | 1 | -8.0834752 | 3.06867511 | -2.634191 | 0.008433813 |
| hp | 1 | 0.0362556 | 0.01773415 | 2.044394 | 0.040914646 |

Before, the SE, z and p values were shifted up one row: `wt` printed
.0177 / 2.0444 / .0409, and the parameter and df vectors had different
lengths. Aliased odds tables (`am ~ wt + I(2*wt)`, with and without an
intercept) failed before with "invalid 'width' argument"; they now print.

BFXX7-3: `I(wt/3.141592653589793)`, `I(wt^0.3333333333333333)` and
`I(wt + 0.12345678901234567)` (+ hp + noise) failed before in all six
procedures with "could not rebuild the selected model". After, all six
complete. Against an independent glm with the exactly written formula,
coefficient, logLik, raw-data prediction and profile-CI errors are all 0.
The selected terms keep the exact constant: `identical()` variables,
3.1415926535897931. For pi, the coefficients are 18.86629871720412,
-25.39498624864346, .03625559608221653.

## Regression re-runs (scratch scripts)

- bfx6/bfx7 offset, weights, grouped, zero-trial and `na.exclude` grid, all
  six procedures: 52 of 54 rows have error 0. The remaining 2 are the known
  poly-basis reference artifact: logLik equal, prediction 2e-16.
- LR near-collinear grid: eps 1e-4/1e-6/1e-8 at scales 1, 1e155 and 1e-200
  are all accepted with LR = `anova()`. The bfx6 boundary case
  (eps 1e-8 at 1e-200), previously rejected, is now accepted. The invalid
  reversed-predictor controls still error.
- BFXX6-2 weighted and zero-trial nesting, and BFXX6-3 intercept-only odds:
  unchanged and correct.

## Tests

- Fail before: the new test files were run against a `git archive` copy of
  ea9055b (no `git stash`). The result was 22 failures or errors:
  translation 9, aliased summaries 12, constants 1. The restore-error
  branch test passes on both versions, as intended; it exercises the
  bfx7 error path, which had no test.
- Full suite (`NOT_CRAN=true`, `devtools::test()`): 19 files, 209 test
  blocks, 1050 expectations passed, 0 failed, 0 errors, 0 warnings, 0
  skipped. testthat's deletion of `_snaps/forward-p.md` and its snapshot
  line-ending rewrites were reverted with `git checkout`.

## R CMD check

`R CMD check --as-cran blorr_0.3.2.tar.gz`. The tarball was built with
`R CMD build --no-manual` from a `git archive HEAD` copy with the changed
files overlaid; `diff -rq --strip-trailing-cr` found no differences from the
worktree. The check built the PDF manual ("checking PDF version of manual
... OK"). `_R_CHECK_FORCE_SUGGESTS_=false` because V8 is not installed.
**Status: 1 NOTE, 0 warnings, 0 errors.** The NOTE is the expected
maintainer change.

## Not fixed / judgment calls

- The nesting tolerance now grows with conditioning. A direction that
  deviates from the full space by less than about
  `8 * eps * sqrt(n) * kappa` counts as nested. Severely ill-conditioned
  comparisons (tolerance >= 1e-4) are rejected with a clear error.
- When glm itself aliases a column (for example a shift so large that the
  varying part is below glm's tolerance), the nesting test uses glm's
  estimable columns. That is consistent with the fitted models.
- Carried over: selection always fits an intercept; term hierarchy is not
  enforced.

## Follow-ups

1. Codex re-audit of this round (bfxx8) before any CRAN submission,
   focusing on the condition-number tolerance and summaries of aliased
   models in the other printed tables.
2. Aravind: no-intercept selection policy (open from bfx5).
3. Aravind: marginality for interaction terms (open from bfx5).
