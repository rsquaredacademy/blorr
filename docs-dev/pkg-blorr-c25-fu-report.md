# pkg-blorr-c25-fu: follow-up to the C25 audit (DeepSeek)

2026-11-19. Fix round for the two open findings of
`docs-dev/pkg-blorr-c25-aud-report.md` on branch `task/blorr-c25`
(HEAD at start `19d1604` "Audit pkg-blorr-c25 build"). Audited domain: binary
logistic regression; a response with three or more levels is outside the
documented domain (`@param model` on every entry point says binary only).

Scope: M1 (tests for the shared model gate) and L1 (NEWS bullet). L2 left as a
written follow-up below. No version bump; no code behaviour changed; nothing
else touched.

## Files changed

- `tests/testthat/test-binary-response.R` — new test block for the shared gate.
- `NEWS.md` — rewrote the three-level-response bullet under
  `# blorr (development version)` / `## Bug fixes`.
- `docs-dev/pkg-blorr-c25-fu-report.md` — this report.

No `R/` file changed. `R/blr-utils.R` was edited only transiently for the M1
mutation proof (the gate line was deleted, the test file run, and the line
restored byte for byte; `git diff -- R/blr-utils.R` is empty).

## M1 — tests for the shared gate in `blr_check_supported()`

Added a fifth test block in `tests/testthat/test-binary-response.R`:

```r
test_that("model functions reject a three-level response through the shared gate (#130)", {
  d <- three_level_data()
  m <- glm(ses3 ~ female + read, data = d, family = binomial)

  # both fit statistics and R2 helpers reach blr_check_supported() directly
  expect_error(
    blr_model_fit_stats(m),
    "The response `ses3` has 3 levels"
  )
  expect_error(
    blr_rsq_adj_count(m),
    "The response `ses3` has 3 levels"
  )

  # a stepwise entry point, with a full candidate set
  ms <- glm(ses3 ~ female + read + math, data = d, family = binomial)
  expect_error(
    blr_step_aic_forward(ms),
    "The response `ses3` has 3 levels"
  )
})
```

These calls do not go through `blr_regress()` or `resp_profile()`, so the only
thing that can raise the message is the gate line
`blr_check_binary_response(resp, names(model$model)[1], exactly = FALSE)` in
`blr_check_supported()`.

### Before (HEAD, no such test)

The audit's mutation check: with the gate line removed and only the HEAD test
suite, `devtools::test()` gave
`files = 26 tests = 245 failed = 0 ... passed expectations = 1998` — removing
the gate left the suite green. The same three calls returned numbers instead of
an error (audit M1 table: `blr_model_fit_stats(m)` printed a full fit table,
`blr_rsq_adj_count(m)` returned `0.06382979`, and
`blr_step_aic_forward(...)` printed a selection summary).

### After — proof by removing only the gate line

With the new tests in place and the gate line temporarily removed from
`R/blr-utils.R`:

```text
Rscript c25fu_runtests.R . binary-response

ℹ Testing blorr
binary-response: ...123........

══ Failed ══════════════════════════════════════════════════════════════════════
── 1. Failure ('test-binary-response.R:46:3'): model functions reject a three-le
Expected `blr_model_fit_stats(m)` to throw a error.

── 2. Failure ('test-binary-response.R:50:3'): model functions reject a three-le
Expected `blr_rsq_adj_count(m)` to throw a error.

── 3. Failure ('test-binary-response.R:57:3'): model functions reject a three-le
Expected `blr_step_aic_forward(ms)` to throw a error.

TOTALS: files = 1  tests = 5  failed = 3  error = 0  skipped = 0  warning = 0  passed expectations = 11
```

Restored gate line, same filter:

```text
ℹ Testing blorr
binary-response: ..............

TOTALS: files = 1  tests = 5  failed = 0  error = 0  skipped = 0  warning = 0  passed expectations = 14
```

Note on the general rule "the test must fail on current HEAD": the gate
behaviour itself is already correct on HEAD (M1 was a test gap, not a wrong
result). So the new tests pass on unmodified HEAD by design, and the required
proof for this item — as stated in the brief — is that they fail when that one
gate line is removed. That is shown above.

## L1 — NEWS bullet now matches the behaviour

### Before

```text
- A response with more than two levels is now rejected with an error naming
  the response and the number of levels found. `glm()` silently recodes a
  factor or character response with three or more levels as "first level"
  versus "everything else", so `blr_regress()`, the response profile
  (`resp_profile()`) and the printed response summary (`print_blr_reg()`)
  showed the frequencies of only the first two levels: for
  `glm(ses ~ female + read, hsb2, family = binomial())` the profile of the
  three-level `ses` response reported 47 and 95 of 200 observations and
  silently dropped the third level (58). Binary responses are unchanged.
```

Problems fixed: it claimed `resp_profile()` dropped a level (on base it
returned all three: `1 2 3 / 47 95 58`; only the printed summary dropped the
third); it named two internal, non-exported functions; it omitted that every
model entry point now rejects 3+ levels; and it omitted the single-outcome
`blr_regress()` change.

### After

```text
- A response with more than two levels is now rejected with an error naming
  the response and the number of levels found. `glm()` silently recodes a
  factor or character response with three or more levels as "first level"
  versus "everything else", so a three-level response was fitted without
  complaint. For `glm(ses ~ female + read, hsb2, family = binomial())` the
  three-level `ses` response was fitted and the summary printed by
  `blr_regress()` reported 47 and 95 of 200 observations, silently dropping
  the third level (58). Every model entry point now raises this error:
  `blr_regress()`, `blr_model_fit_stats()`, the `blr_rsq_*()` functions, the
  stepwise functions, `blr_gains_table()`, `blr_confusion_matrix()` and
  `blr_test_hosmer_lemeshow()`. A response with a single outcome is also
  rejected by `blr_regress()` at fit time with the same error naming the one
  level found; previously the object was built and printing it failed with
  "subscript out of bounds". Binary responses are unchanged.
```

Every claim was re-checked on this tree with `pkgload::load_all()`:
`blr_model_fit_stats()`, `blr_rsq_adj_count()` and `blr_step_aic_forward()`
raise "The response `ses3` has 3 levels"; `blr_regress(zero ~ read)` (all-zero
response) raises "The response `zero` has 1 level". `NEWS.md` line endings are
unchanged (CRLF); the dev section is still `# blorr (development version)` and
`Version:` is still `0.3.2.9000` (no bump).

L1 is documentation only, so it adds no behaviour test; the behaviours it
describes are covered by the existing `test-binary-response.R` tests
(`blr_regress()`, `resp_profile()`, `print_blr_reg()`) and the new M1 block.
It is not possible to write a testthat test that fails on the old NEWS text;
the check is a text review, shown above.

## Test totals (`devtools::test()`, Rscript script file, no pipes into R)

Baseline on unmodified HEAD before this round:

```text
TOTALS: files = 26  tests = 245  failed = 0  error = 0  skipped = 0  warning = 0  passed expectations = 1998
```

After this round (full suite):

```text
TOTALS: files = 26  tests = 246  failed = 0  error = 0  skipped = 0  warning = 0  passed expectations = 2001
```

Generated artifacts from the run (`tests/testthat/Rplots.pdf`, the `_snaps`
line-ending rewrites) were restored with `git checkout --`; the final working
tree contains only the two intended edits plus this report.

## Commands run

Scratch scripts:
`C:/Users/DELL/AppData/Local/Temp/opencode/c25fu_probe.R`,
`C:/Users/DELL/AppData/Local/Temp/opencode/c25fu_runtests.R`
(the latter runs `devtools::test(path, filter, reporter = "summary")` and prints
the TOTALS line).

```text
git status; git log --oneline -5; git branch --show-current
Rscript c25fu_probe.R                                # behaviour of the three calls, single-outcome message
Rscript c25fu_runtests.R .                           # baseline: 26 files / 245 tests / 1998 expectations
# M1 test added to tests/testthat/test-binary-response.R
Rscript c25fu_runtests.R . binary-response           # 5 tests / 14 expectations, green
# deleted only `blr_check_binary_response(...)` from blr_check_supported() in R/blr-utils.R
Rscript c25fu_runtests.R . binary-response           # 3 failures (shown above)
# restored the gate line
git diff -- R/blr-utils.R                            # empty
Rscript c25fu_runtests.R .                           # after: 26 files / 246 tests / 2001 expectations
git checkout -- tests/testthat/Rplots.pdf tests/testthat/_snaps
git diff --stat
```

No install, no push, no `gh`/GitHub, no branch switch, no worktree.

## Follow-ups (written out in full)

### L2 (deferred, unchanged from the audit) — data-column APIs still treat level 2 as the event for a 3+ level response

Pre-existing and not changed by the C25 build or this follow-up. `blr_woe_iv()`,
`blr_woe_iv_stats()`, `blr_segment()`, `blr_segment_dist()`,
`blr_segment_twoway()` and `blr_bivariate_analysis()` accept the response
column as data and silently use "level 2 versus the rest" (the package concepts
are binary: WoE and "1s" rates). Their `@param response` says only "Response
variable; column in data".

Reprex (same on `origin/develop` and HEAD):

```r
d <- hsb2; d$ses3 <- d$ses
blr_woe_iv(d, female, ses3)   # count_0s 44/61, count_1s 47/48 -> levels 1 and 3 pooled as "0s"
blr_segment(d, ses3, female)  # 1s% 52.00 / 44.00 = share of level 2 only
```

Expected: an error, or a documented "level 2 versus rest" rule. Actual: WoE/IV
and event rates for "level 2 vs everything else", with no message.

Recommendation: reuse `blr_check_binary_response()` on the response column in
those entry points (clear error naming the response and its level count) and
state "binary" in their `@param response` docs. Add `testthat` tests with a
three-level response that fail before the change, plus the ordinary-input
regression grid (binary factors, unused levels, logical, NA responses) to prove
valid inputs are not rejected.

Out-of-domain status: the package scope (binary logistic regression) and the
concepts imply a binary response, so a three-level column is outside the
documented domain. Audits should list it as backlog, not a release blocker,
provided no ordinary input is affected. It is deferred here by the brief's
"fix only these items" rule.

No other follow-ups from this round.
