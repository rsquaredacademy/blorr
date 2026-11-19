pkg-blorr-c25-rx: independent C25 follow-up re-check (Codex)

2026-11-19. Audited HEAD `7a17c2b51b3a1bb529c32df6f93f67b73bc327ac`
on `task/blorr-c25`; follow-up parent `19d1604`, original C25 build
`1bebeb2`, pre-C25 develop baseline `bc76c35`. Read both audit reports and
`F:/pm/notes/package-audit-stop-rule.md`; independently reproduced the claims.
Only this report is changed in the worktree. All R scripts, extracted trees,
mutations, compiled files, test artifacts and check output are outside the repo.

The supported model domain is unweighted Bernoulli binomial/logit models with
the stored model frame and response, as stated in the installed help, including
`man/blr_model_fit_stats.Rd`. Responses with three observed factor levels are
outside the binary domain. Ordinary binary inputs, unused factor levels,
missing responses and supported single-outcome confusion matrices must retain
their behavior. No floating-point extremes were added to this audit.

The follow-up changes exactly `NEWS.md`, `tests/testthat/test-binary-response.R`
and its report. `git diff --exit-code HEAD^ HEAD -- R src man NAMESPACE
DESCRIPTION` is empty. Its claim that no package code behavior changed is true.

| Original item | Re-check | Evidence and implication |
| --- | --- | --- |
| M1, shared-gate test gap | **FIXED** | The three new assertions pass on HEAD and fail when only the shared gate is removed in scratch. |
| L1, NEWS accuracy | **STILL OPEN, partially fixed** | All four original corrections were made, but the revised bullet still falsely says `glm()` silently recodes a character response with 3+ levels. Direct output below contradicts that statement. This remains Low. |
| L2, data-column APIs silently choose level 2 | **STILL OPEN, explicitly deferred** | Five APIs still return results without a warning or error for the three-level response. The stop rule's out-of-domain honesty exception applies; this is a merge blocker under that rule. |
| L3, builder report filename | **STILL OPEN, not claimed fixed** | `docs-dev/c25-report.md` exists; `docs-dev/pkg-blorr-c25-report.md` does not. Nonblocking process/documentation issue. |

M1 was verified without touching the real worktree's source. Extracted HEAD to
`mutant/` and removed exactly this line from `R/blr-utils.R:54`:

```r
blr_check_binary_response(resp, names(model$model)[1], exactly = FALSE)
```

`git diff --no-index head/R/blr-utils.R mutant/R/blr-utils.R` showed that single
deletion and no other source difference. The HEAD test file was identical in
both copies. Each test run was a separate Rscript process using a script file
that called `devtools::test(path, filter = "binary-response", reporter =
"summary", stop_on_failure = FALSE)`.

```text
HEAD:
TOTALS: files = 1 tests = 5 failed = 0 error = 0 skipped = 0 warning = 0 passed expectations = 14

HEAD with only the shared gate removed:
Failure test-binary-response.R:46:3
  Expected `blr_model_fit_stats(m)` to throw a error.
Failure test-binary-response.R:50:3
  Expected `blr_rsq_adj_count(m)` to throw a error.
Failure test-binary-response.R:57:3
  Expected `blr_step_aic_forward(ms)` to throw a error.
TOTALS: files = 1 tests = 5 failed = 3 error = 0 skipped = 0 warning = 0 passed expectations = 11
```

The mutant returns actual numbers: `blr_model_fit_stats(m)` prints McFadden R2
`0.063` and adjusted count R2 `0.064`; `blr_rsq_adj_count(m)` returns
`0.06382979`; forward AIC selection retains `read`, then `female`, with AICs
`212.528`, `210.262`. On HEAD all three calls stop with:

```text
The response `ses3` has 3 levels; blorr supports a binary response with exactly two levels.
```

Here `d <- hsb2; d$ses3 <- d$ses`,
`m <- glm(ses3 ~ female + read, data = d, family = binomial)` and
`ms <- glm(ses3 ~ female + read + math, data = d, family = binomial)`.
This proves that the new assertions depend on the shared gate, rather than
being rescued by the separate profile check.

For completeness, copied the entire HEAD binary-response test file to the
pre-C25 scratch tree and reran it against that older code:

```text
TOTALS: files = 1 tests = 5 failed = 5 error = 1 skipped = 0 warning = 0 passed expectations = 8
```

The failures are the two original response/profile expectations and the three
new shared-gate expectations. The error is the previously documented
`round(x$est, 4): non-numeric argument to mathematical function` in the
hand-built printer test. The ordinary-response regression block passes on
both versions by design. No pre-fix failures are attributed to setup problems.

L1's four original points were checked individually:

- The rewritten example correctly attributes the truncated frequencies to the
  printed `blr_regress()` summary. On the pre-C25 tree, `resp_profile(m)` returns
  all three counts `47 95 58`; the printed summary shows only `47` and `95`.
- The revised bullet no longer presents `resp_profile()` or `print_blr_reg()`
  as user-facing functions; neither is exported.
- The listed model APIs reject the factor with the response name and level
  count. Probed both regression interfaces, fit statistics, all eight R2
  helpers, all six stepwise procedures, gains, confusion matrix and HL test.
- With `d$zero <- 0`, HEAD's `blr_regress(zero ~ read, data = d)` errors with
  ``The response `zero` has 1 level; ...``. The old tree creates the object and
  printing fails with `subscript out of bounds`. Both emit the existing
  non-convergence warning for this degenerate fit.

However, the retained words "factor or character response" in `NEWS.md:7`
are inaccurate.
These are direct probe outputs on both HEAD and the old tree:

```r
d <- hsb2
d$char3 <- as.character(d$ses)
glm(char3 ~ female + read, data = d, family = binomial)
# ERROR: y values must be 0 <= y <= 1

mf <- glm(ses ~ female + read, data = d, family = binomial)
table(mf$y)
#   0   1
#  47 153
```

The silent recoding claim applies to the factor response. Dropping "or
character" would make this part accurate. No NEWS behavior test was added by
the follow-up; a prose-only change has no runtime mutation test. The report's
claim that every NEWS statement was rechecked is not supported by this probe.

L2 was reproduced on HEAD and on `bc76c35`, using the same three-level `ses3`.
The relevant public paths are in `R/blr-woe-iv.R:51` and
`R/blr-bivariate-analysis.R:134`, without the shared model gate:

```text
table(d$ses3, d$female)
     0  1
  1 15 32
  2 47 48
  3 29 29

blr_woe_iv(d, female, ses3)
levels  count_0s  count_1s
0       44        47
1       61        48
Information Value: 0.0232

blr_segment(d, ses3, female)
pred   1s%
0      52.00
1      44.00

blr_segment_dist(d, ses3, female)
female   n    1s    n%      1s%
0        91   47    46.00   52.00
1       109   48    54.00   44.00
```

None of these calls signals a warning/error or identifies the undocumented
level-2-versus-rest interpretation. `blr_woe_iv_stats(d, ses3, female)` prints
the same WoE table; `blr_segment_twoway(d, ses3, female, prog)` silently returns
the level-2 shares as well. These five APIs count 95 events, whereas the fitted
factor-response glm counts 153. This is categorical misuse with silently
misleading output, not a numerical precision edge case.

One correction to the follow-up report: `blr_bivariate_analysis(d, ses3,
female)` already errors on HEAD with ``The response `ses3` has 3 levels; ...``
through its model-side LR calculation. It is not a sixth silently accepting
API on this tree. On the old tree it returned IV `0.02`, LR chi-square `4.6749`.

The stop rule explicitly says that an out-of-domain finding is backlog
"unless the package returns a wrong result without the bound or the error
(that is a blocker, because the honesty part failed)." The follow-up's
criterion "provided no ordinary input is affected" omits this exception.
Binary-only package scope does not make silent three-level output honest.
L2 therefore cannot be waived solely as out of domain under the supplied
rule. No GitHub issue was created, as instructed. A later fix should validate
these response columns, document their binary requirement and preserve the
ordinary-input regression coverage.

Ordinary-input comparisons used 348 calls per tree in separate Rscript
processes, comparing HEAD to both the immediate follow-up parent and pre-C25
develop. Captured printed output, returned data, error messages, warnings and
messages were compared with `all.equal(..., tolerance = 0)`; character output
was compared exactly. Returned glm objects were reduced to coefficients,
deviances, fitted values, residuals, model frames, observation counts and
residual degrees of freedom; plots to data and labels; glm summaries to their
numeric summary fields. This excludes environment addresses from comparisons.

```text
19d1604 vs HEAD: calls = 348 identical = 348 differing = 0
errors left/right = 0 0
bc76c35 vs HEAD: calls = 348 identical = 348 differing = 0
errors left/right = 0 0
```

The grid contains 13 model variants: numeric 0/1, labeled factor, reversed
factor, ordered factor, logical, unused third factor level, missing responses,
`I(math > 60)`, factor levels 1/3 with unused level 2, `subset =`, mtcars
numeric and logical-expression responses, and bank_marketing. Each model
runs regression with/without odds intervals, profile, gains, two confusion
cutoffs, fit statistics, HL, pairs, Gini, Lorenz, every R2 helper, residual
diagnostics, link test, collinearity diagnostics, ROC/KS data and residual plot
data. Additional calls cover five formula-interface cases, all six stepwise
methods, held-out gains/confusion/HL, LR and multi-model fit statistics, all
six data-column APIs on binary input, and an all-zero confusion matrix.

All trees emit the same 42 `glm.fit: fitted probabilities numerically 0 or 1
occurred` warnings across four mtcars odds-interval/link-test calls. These are
unchanged fitting warnings, not new input rejections. The all-zero confusion
matrix remains accepted (accuracy 1, specificity 1, sensitivity NA). No
ordinary-input regression was found.

The full, unfiltered `devtools::test()` ran on an archive of HEAD with no
package edits:

```text
TOTALS: files = 26 tests = 246 failed = 0 error = 0 skipped = 0 warning = 0 passed expectations = 2001
```

The initial full `devtools::check(document = FALSE, args = "--no-manual")`
attempt stopped at DESCRIPTION checking with 1 ERROR, 1 WARNING and 0 NOTEs.
Its only diagnostic text was the inherited Windows-invalid `C.UTF-8` locale
startup warnings (`LC_COLLATE`, `LC_CTYPE`, `LC_MONETARY`, `LC_TIME`). Preserved
that output, then repeated the complete check with process-local `LANG`,
`LC_ALL`, `LC_COLLATE`, `LC_CTYPE`, `LC_MONETARY` and `LC_TIME` set to `C`.
The retry retained the requested arguments and devtools' default `--as-cran`
setting; it uses `error_on = "never"` only to retain the result object for
reporting and a separate scratch `check_dir`.

The complete retry finished successfully, including examples,
`--run-donttest`, installed-package tests and rebuilding vignette outputs:

```text
* checking examples ... [13s] OK
* checking examples with --run-donttest ... [39s] OK
* checking tests ... [76s] OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [26s] OK
* DONE
Status: OK
CHECK_TOTALS: errors = 0 warnings = 0 notes = 0
```

Inside R CMD check, testthat reports
`[ FAIL 0 | WARN 0 | SKIP 2 | PASS 1998 ]`. The two skips are existing source-Rd
tests at `test-gini-index.R:83` and `test-stepwise-selection.R:62`: installed
packages do not contain raw `man/*.Rd` files at the paths tested by
`system.file("man", ...)`. Their three expectations passed in the direct
source-tree `devtools::test()` above, explaining its 2001 passes and zero
skips. These were test-authored skips, not audit exclusions. The full check
ran from 15:23:41 to 15:29:34 IST; the source-tree full test ran from 15:19:34
to 15:22:28 IST.

All runs used R 4.6.1 (2026-06-24 ucrt), Windows 11 x64, devtools 2.5.2,
testthat 3.3.2 and pkgload 1.5.3. The test/probe processes inherited the same
locale startup warnings and fell back to locale `C`; these are separate from
the zero testthat-warning total. The check retry sets `C` before R starts.

Reproduction scripts and complete logs are retained at
`C:/Users/DELL/AppData/Local/Temp/pkg-blorr-c25-rx-20261119-151846/`:

- `runtests.R`, `full-test.log`, `head-filter-test.log`,
  `mutation-test.log`, `base-filter-test.log` and their result RDS files;
- `probe.R`, `probe-head.log`, `probe-base.log`, `probe-mutant.log`;
- `grid.R`, `compare.R`, `grid-{head,parent,base}.rds`, and the two
  `grid-head.rds.compare-with-*.log` comparison logs;
- `runcheck.R`, `check.log`, `check-original-00check.log`,
  `runcheck-retry.R`, `check-retry.log` and `check-retry/blorr.Rcheck/`.

Representative commands (all R invocations use script files):

```powershell
# $auditScratch is the external scratch directory above.
git archive --format=tar -o "$auditScratch/head.tar" HEAD
tar --force-local -xf "$auditScratch/head.tar" -C "$auditScratch/head"
Rscript "$auditScratch/runtests.R" "$auditScratch/head" "$auditScratch/full-test.log"
Rscript "$auditScratch/runtests.R" "$auditScratch/head" "$auditScratch/head-filter-test.log" binary-response
Rscript "$auditScratch/runtests.R" "$auditScratch/mutant" "$auditScratch/mutation-test.log" binary-response
Rscript "$auditScratch/grid.R" "$auditScratch/head" "$auditScratch/grid-head.rds"
Rscript "$auditScratch/compare.R" "$auditScratch/grid-parent.rds" "$auditScratch/grid-head.rds"
Rscript "$auditScratch/compare.R" "$auditScratch/grid-base.rds" "$auditScratch/grid-head.rds"
Rscript "$auditScratch/runcheck.R" "$auditScratch/check" "$auditScratch/check.log"
$env:LANG = 'C'; $env:LC_ALL = 'C'; $env:LC_COLLATE = 'C'; $env:LC_CTYPE = 'C'; $env:LC_MONETARY = 'C'; $env:LC_TIME = 'C'
Rscript "$auditScratch/runcheck-retry.R" "$auditScratch/check" "$auditScratch/check-retry.log" "$auditScratch/check-retry"
```

No High inside the supported domain was found, M1's test gap is closed, and
ordinary inputs are unchanged. L1 still needs a small prose correction; L3 is
nonblocking. The merge verdict is no because known L2 silent output fails the
explicit honesty exception in the supplied stop rule. This audit makes no
package, test or NEWS fix and does not push or switch branches.

Ready to merge into develop: no
