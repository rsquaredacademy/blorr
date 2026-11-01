# bfx3 fix report — blorr round 3

Job: `bfx3`. Worktree `F:/R/Packages/statistics/blorr-wt/fix-032`, branch
`task/blorr-fix-032`. Base for this round: `4c64e79` (round-2 HEAD). Source
of findings: `F:/pm/notes/2026-11-01-pkg-audits/bfxx2-reaudit.md`; process
rules: `F:/pm/notes/2026-11-01-pkg-audits/common-pkgfix.md`.

Environment: Windows 11 x64, R 4.6.1 (2026-06-24 ucrt), GCC 14.3.0,
testthat 3.3.2. All R runs used `Rscript` from this worktree. No package
installation, dependency change, branch change, push or remote access was
performed. Temporary build/check/source copies were kept outside the worktree
under `C:/Users/DELL/AppData/Local/Temp/opencode`.

## Findings fixed

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| BFXX2-1 rank-one no-intercept model got a fabricated 0/0/1 LR test | `tests/testthat/test-model-fit-stats.R:269` | `R/blr-model-fit-stats.R:371` (`fit_stat`), `:410` (`null_model_nested`); NEWS | Fitted log-likelihoods are retained (`-21.61487` vs `-19.85828`, difference 3.513167); `lr_ratio`, `lr_df`, `lr_pval` are now `NA` with a warning instead of `0/0/1`; the intercept-only 0/0/1 test still passes; a two-predictor no-intercept model is also reported unavailable | Fixed |
| BFXX2-2 full-rank condition indices could be infinite or inaccurate | `tests/testthat/test-coll-diag.R:118` (plus SVD reference in the existing test at `:78`) | `R/blr-collinearity-diagnostics.R:176` (`evalue`), `:214` (`cindx`), `:238` (`pveindex`) | Seed-2 `sd = 1e-8`: eigenvalue `5.568402e-18`, index `727132689.55556` equals `max(svd(z)$d)/min(svd(z)$d)`; seed-1 `sd = 1e-8`: index `897855518.49466` equals the SVD ratio; zero/constant columns still give `Inf`/`NA`; variance proportions still sum to one | Fixed |
| BFXX2-3 p-value selection failed on transformed predictors | `tests/testthat/test-forward-selection.R:77` | `R/blr-utils.R:275` (`selection_weights`), `:300` (`selection_data`), `:349` (`selection_model`); `l <- selection_data(model)` in all six selection procedures (`R/blr-forward-selection.R:69`, `R/blr-backward-elimination.R:71`, `R/blr-stepwise-selection.R:64`, `R/blr-stepwise-forward-regression.R:71`, `R/blr-stepwise-backward-regression.R:66`, `R/blr-stepwise-regression.R:70`) | `am ~ log(wt) + hp + noise` selects `log(wt), hp` in forward, backward and both-direction p procedures; coefficients equal an independent `glm(am ~ log(wt) + hp)` (`19.766500`, `-22.669018`, `0.0331006`); weighted, grouped-trial, offset, probit and sum-contrast fits still match independent fits exactly | Fixed |
| BFXX2-4 LR rejected a valid nested model with different contrasts | `tests/testthat/test-blr-lrtest.R:113` and `:142` | `R/blr-lrtest.R:120` (projection of the reduced design onto the full design) | Treatment-vs-sum contrast pair gives LR `6.7312378514618`, df 1, p `0.0094738831697857`, identical to `anova()`; the reverse contrast pair also matches; `I(wt)` nested in `wt` also matches `anova()`; the missing-intercept, reversed-predictor, different-observations and identical-model rejection tests still pass | Fixed |
| BFXX2-5 VIF dropped a predictor from a no-intercept fit | `tests/testthat/test-coll-diag.R:55` | `R/blr-collinearity-diagnostics.R:143` (`viftol`) | `am ~ 0 + wt + hp` reports both `wt` and `hp` with tolerance `0.5660512` and VIF `1.7666245792683`, matching `1/(1 - R2(lm(hp ~ wt)))`; the single-predictor no-intercept fit reports `wt`, tolerance 1, VIF 1; intercept models are unchanged | Fixed |

## Verification of the audit's exact reproductions

Reproduced against the installed working tree before each fix and rechecked
after it:

- BFXX2-1: before, `lr_ratio 0`, `lr_df 0`, `lr_pval 1` while
  `-2*(logLik(update(m, . ~ 1)) - logLik(m))` was `3.5131665386962`. After,
  the likelihoods are still reported and the test fields are `NA`.
- BFXX2-2: before (seed 2), eigenvalue `-1.218839e-20`, index `Inf` against
  the finite SVD ratio `727132689.55556`. After, the eigenvalue is positive
  and the index is the finite SVD ratio. The tighter seed-1 case now reports
  the SVD ratio `897855518.49466` rather than the cross-product value
  `230297435.75080`. For the original BF6 `sd = 1e-4` case the SVD-based index
  is `89785.8584036323` (the previous cross-product value was `89785.829810331`;
  the relative change is about `3e-7` and the new value is the reference
  asserted in the test).
- BFXX2-3: before, all three p procedures stopped with
  `Error: object 'wt' not found`. After, they return the fit whose
  coefficients match an independent `glm`.
- BFXX2-4: before, `Error: reduced_model must be nested within full_model.`;
  after, the result equals `anova(reduced, full, test = "Chisq")`.
- BFXX2-5: before, `blr_vif_tol(am ~ 0 + wt + hp)` returned only `hp` with
  VIF 1; after, both predictors are reported and `hp` has VIF
  `1.7666245792683`.

Nearby cases were checked as well: intercept-only fit statistics keep
0/0/1; a no-intercept two-predictor model reports the LR test as unavailable;
constant/zero columns still return `Inf`/`NA` diagnostics; and selection with
dropped rows plus case weights still aligns to an independent weighted fit on
the fitted rows.

## Tests

Full local suite (`NOT_CRAN=true devtools::test()`, summary reporter):
19 files, all green, no failures/errors/warnings; the large native pair-count
test ran. New warnings only occur in the newly tested no-intercept case.
Testthat removed the unused `forward-p.md` snapshot during the disposable
full-suite run; that deletion and incidental line-ending churn were restored
and are not part of any commit.

## Check

Clean source archive (`git archive HEAD`), then:

```
R CMD build --no-manual src
R CMD check --as-cran --no-manual blorr_0.3.2.tar.gz
```

Result: **0 errors / 0 warnings / 1 NOTE**. Examples, run-donttest examples,
tests and vignette rebuild are all OK. Check-mode suite:
`FAIL 0 | WARN 0 | SKIP 49 | PASS 379`. The only NOTE is the intended CRAN
incoming-feasibility maintainer change:

```
New maintainer: Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s): Aravind Hebbali <hebbali.aravind@gmail.com>
```

No other NOTE or WARNING was emitted. The check was rerun from a fresh
`git archive` after the final LR-terminology commit (`102ac04`) and produced
the same result.

## Commits

- `f292940` fix: only use the intercept-only null when it is nested (BFXX2-1) (bfx3)
- `804ed6b` fix: derive condition indices from scaled-design singular values (BFXX2-2) (bfx3)
- `7071649` fix: keep every direction for wide-design condition indices (BFXX2-2) (bfx3)
- `d70093a` fix: keep every predictor in VIF for no-intercept fits (BFXX2-5) (bfx3)
- `6a9a8db` fix: test LR nesting by design column space (BFXX2-4) (bfx3)
- `102ac04` fix: accept any design-space nested model in LR test (BFXX2-4) (bfx3)
- `f7f1f19` fix: refit selection candidates on original fitted rows (BFXX2-3) (bfx3)

Each user-visible fix has one bullet in `NEWS.md` under 0.3.2.

## Deferred

- Variable selection on a model fitted without a `data` argument and using a
  transformed predictor still cannot refit candidates; the original
  variables are not recoverable from the stored model frame, so the procedure
  errors rather than returning a wrong value. All models whose data is
  recorded in the call are handled by `selection_data()`.
- No other re-audit finding remains open.

## Notes

- No API change and no new dependency. `blr_model_fit_stats()` now warns and
  returns `NA` for the LR test when the intercept-only model is not nested in
  the fitted design (a no-intercept model); this is the documented
  "unavailable" behaviour requested by BFXX2-1.
- `blr_vif_tol()` now applies its usual centred auxiliary-regression rule to
  every predictor of a no-intercept design; it no longer assumes the first
  design column is the intercept.
- `.bfxx2/` (1.2 GB of audit copies, logs, libraries and tarballs) was
  deleted from the worktree. The auditor's untracked
  `docs-dev/bfxx2-reaudit.md` was left in place because it is a report, not a
  temporary folder; it is not part of any commit.
