# blbl: blorr printer label fixes (M5, M7)

2026-11-16. Branch `task/blorr-labels`; base `2a7d023` (blorr 0.3.2.9000).
Commits:

- `64b642f` `fix: print percentage cells and correct segment labels and headers (M5, M7) (blbl)`
- `1390b98` `test: declare descriptr in Suggests for the mtcarz tests (blbl)`

The two findings are one tightly coupled group of label/printer defects in the
same output file, so they share one fix commit. No API change; the values
returned in the fitted/segment objects are unchanged.

## Findings

| Finding | Test added | Fix (file:line) | Verified how | Status |
| --- | --- | --- | --- | --- |
| M5: association table printed proportions under `%` labels | `tests/testthat/test-blbl.R:8` (independent pair counts for `am ~ wt + hp`, unchanged returned proportions, printed 98.3806/1.6194/0.0000) and `:40` (segment `n%`/`1s%` printed as 34.00/73.00 etc.) | `R/blr-output.R:108,117,123,129` (association cells ×100, width recomputed), `R/blr-output.R:217-236` (`blr_segment()` `1s%`), `R/blr-output.R:239-272` (`blr_segment_dist()` `n%`/`1s%`) | Audit case reproduced on an installed archive of `2a7d023` and again on fixed `HEAD`; printed output before/after below. Counts/c/Somers' D/gamma/tau unchanged. | Fixed |
| M7: segment printers used factor codes and wrong headers | `tests/testthat/test-blbl.R:40` (levels 4/6/8, header `cyl`, rows counts 11/7/14 and rates .73/.43/.14) and `:67` (`gear` top, `cyl` left, table values checked against `table()`/32) | `R/blr-bivariate-analysis.R:281` stores the selected predictor name instead of the internal `pred`; `R/blr-output.R:243` converts the factor column with `as.character()` before concatenation; `R/blr-output.R:498,504` select `variable_2` for the top header and `variable_1` for the left header | Audit case before/after below; two-way cell values equal `table(d$cyl[am == "1"], d$gear[am == "1"]) / 32`; dist counts/rates match `tapply()` results. | Fixed |

### Convention chosen for M5

Percentages are printed on the **0-100 scale, keeping the `%` labels**. This is
the convention already used by blorr for its other `%`-named columns:
`blr_gains_table()` stores `cum_total_%`, `cum_1s_%` and `cum_0s_%` as
`proportion * 100` and divides by 100 only when plotting
(`R/blr-gains-table.R:332-335,351-352`). The printers were the only place where
a `%` label carried a 0-1 value. Values inside the returned objects are **not**
documented as percentages and stay exactly as before:

- `blr_regress()$blr_cord` keeps proportions (`concordance = 243/247`);
- `blr_segment_dist()$dist_table$n%` and `$1s%` keep proportions, because the
  segment-distribution plot consumes them on a 0-1 axis
  (`R/blr-bivariate-analysis.R:338-348`); only the printed table shows 0-100.
  Those two columns are stored rounded to two decimals, so the printer shows the
  percentage of the stored proportion (`0.34` → `34.00`), as it did before on the
  0-1 scale.

### M7 details

`blr_segment_dist()` stored `var_name <- names(dist_table)[1]`, which is the
internal column name `pred`; it now stores the deparsed predictor name. The row
labels were `c(header, factor_column)`, which coerces the factor to its integer
codes before `as.character()` is applied; conversion now happens first. The
two-way table's `varnames` are `c(response, variable_1, variable_2)`, so the top
header is `varnames[3]` and the left-hand header is `varnames[2]`.

Because the distribution plot title uses `x$var_name`, it now shows the
predictor name too (`prog` instead of `pred`); see the plot snapshot below.

## Before and after (audit cases)

Installed from a clean `git archive 2a7d023` into a temporary library, then from
fixed `HEAD`:

```text
### M5 association (am ~ wt + hp, mtcars)

before                                          after
% Concordant          0.9838                   % Concordant        98.3806
% Discordant          0.0162                   % Discordant         1.6194
% Tied                0.0000                   % Tied               0.0000
Pairs                  247                     Pairs                 247

### M7 segment distribution (am, cyl)

before                                          after
  pred      n     1s     n%     1s%             cyl    n     1s     n%       1s%
   1        11    8     0.34    0.73              4     11    8     34.00    73.00
   2        7     3     0.22    0.43              6     7     3     22.00    43.00
   3        14    2     0.44    0.14              8     14    2     44.00    14.00

### M7 two-way (am, cyl, gear)

before (top cyl, left am)                       after (top gear, left cyl)
                     cyl                                            gear
 am       3       4       5                     cyl      3       4       5
  4     0.000   0.188   0.062                    4     0.000   0.188   0.062
  6     0.000   0.062   0.031                    6     0.000   0.062   0.031
  8     0.000   0.000   0.062                    8     0.000   0.000   0.062
```

Counts 11/7/14 and conditional event rates 8/11=.727, 3/7=.429 and 2/14=.143
are the ones the audit states for levels 4/6/8; they are now attached to the
levels they describe. The association pair counts (243 concordant, 4
discordant, 247 pairs) are recomputed independently in the test with
`outer(fitted[am == 1], fitted[am == 0], "-")`.

## Tests

New file `tests/testthat/test-blbl.R`, four blocks:

1. M5 association: pair counts and returned `blr_cord` proportions checked
   against `glm(am ~ wt + hp)` fitted probabilities; printed `%` cells asserted
   as 98.3806 / 1.6194 / 0.0000 and asserted **not** to be the old 0.9838 /
   0.0162. `c` (0.9838) is deliberately still a proportion and is not asserted
   against.
2. M5/M7 distribution: `dist_table$variable` is the factor `4/6/8` and prints as
   `4/6/8`; `n` is 11/7/14 and `1s` is 8/3/2; the header is `cyl` and the
   internal `pred`/codes are absent; each row prints its level, counts and
   `34.00`/`22.00`/`44.00` and `73.00`/`43.00`/`14.00` percentages.
3. M7 two-way: table values and dimnames match the independent `table()` above;
   the top line is `gear` and the left header line is `cyl 3 4 5`; `am` does not
   appear.
4. Layout snapshots for `print(blr_segment_dist(mtcarz, am, cyl))` and
   `print(blr_segment_twoway(mtcarz, am, cyl, gear))`.

Red run against an installed `2a7d023` in a temporary library (the test file
copied to `.blbl-tmp/red-test/`, so the new snapshot is added rather than
compared): testthat stops reporting at its limit of 10 failures; those ten are
the M5/M7 assertions — missing `98.3806`/`1.6194`, the `cyl` header, the
`pred`/integer-code output, and the percentage cells. The two-way `gear`/`cyl`
header assertions also fail but fall beyond that limit. The snapshot block
simply records the old layout on that run. The explicit value assertions (not
the snapshot) are the red evidence.

Green after the fix, `NOT_CRAN=true devtools::test()`:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1987 ]
```

The explicit value assertions in `test-blbl.R` are the red evidence; the
snapshot block (`_snaps/blbl.md`) was generated from the fixed output.

## Snapshots updated

- `tests/testthat/_snaps/blr-regress.md` — hsb2 `% Concordant/Discordant/Tied`
  now 85.6116/14.2472/0.1412 instead of 0.8561/0.1425/0.0014.
- `tests/testthat/_snaps/bivariate-analysis.md` — `blr_segment()` `1s%` now
  percentages; two-way header `race`/`prog`; distribution header `race`,
  levels 1-4 and percentage columns.
- `tests/testthat/_snaps/blr-plots/segment-distribution-plot.svg` — only the
  plot title text changed, `pred` → `prog` (same 7037-byte SVG; the only diff is
  the title `<text>` element).

No `R-pre-4.6` snapshot variant is affected. This package keeps R-version
variants only for five residual plots (`_snaps/R-4.6/blr-plots`,
`_snaps/R-pre-4.6/blr-plots`); the three changed snapshots above are ordinary,
non-versioned text/SVG snapshots, so there is nothing for Claude to regenerate
for R < 4.6. If the Linux box's vdiffr produces a byte-different
`segment-distribution-plot.svg`, regenerate that single unversioned file there.

## Metadata

`DESCRIPTION: Suggests` now lists `descriptr`. The brief requires the M7 tests
to use `descriptr::mtcarz`, and without the declaration `R CMD check` reports
`'::' or ':::' import not declared from: 'descriptr'` as a WARNING. `descriptr`
is already an optional (app-only) package used through
`check_suggests("descriptr")` in `R/blr-launch-app.R:12`; this is a
test-time Suggests entry, not a new import or runtime dependency.

## Clean archive check

From a clean `git archive HEAD` copy in `.blbl-tmp/finalcheck/src`:

```text
R CMD build --no-manual src
R CMD check --as-cran --no-manual blorr_0.3.2.9000.tar.gz
```

Result: **0 errors, 0 warnings, 1 NOTE**.

```text
* checking examples ... [24s] OK
* checking examples with --run-donttest ... [48s] OK
* checking tests ...
  Running 'testthat.R' [98s]
 [98s] OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [42s] OK
Status: 1 NOTE
```

Installed check-mode test summary:
`[ FAIL 0 | WARN 0 | SKIP 50 | PASS 1928 ]` (the 50 skips are the existing
on-CRAN and source-Rd skips).

The sole NOTE is the expected development-version incoming note:

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

Version contains large components (0.3.2.9000)

Days since last update: 0
```

No other NOTE, WARNING or ERROR appeared.

## Commands run that changed something

```sh
# red run at 2a7d023 (after writing the test file)
Rscript -e "devtools::test(filter = 'blbl', stop_on_failure = FALSE)"

# after the fix
Rscript -e "devtools::test(filter = 'blbl', stop_on_failure = FALSE)"

# full suite
NOT_CRAN=true bash ~/.claude/bin/heavy-slot.sh pkg-blorr-lbl Rscript -e 'devtools::test(stop_on_failure = FALSE)'

# before/after audit cases from clean installations
git archive 2a7d023 | tar -x -C .blbl-tmp/base-src
R CMD INSTALL --no-docs --no-help -l .blbl-tmp/base-lib .blbl-tmp/base-src
git archive HEAD | tar -x -C .blbl-tmp/fix-src
R CMD INSTALL --no-docs --no-help -l .blbl-tmp/fix-lib .blbl-tmp/fix-src
Rscript .blbl-tmp/audit-case.R .blbl-tmp/base-lib
Rscript .blbl-tmp/audit-case.R .blbl-tmp/fix-lib

# clean archive build and check
git archive HEAD | tar -x -C .blbl-tmp/finalcheck/src
bash ~/.claude/bin/heavy-slot.sh pkg-blorr-lbl bash -c 'cd .blbl-tmp/finalcheck && R CMD build --no-manual src && R CMD check --as-cran --no-manual blorr_0.3.2.9000.tar.gz'

# commits
git commit -m "fix: print percentage cells and correct segment labels and headers (M5, M7) (blbl)"
git commit -m "test: declare descriptr in Suggests for the mtcarz tests (blbl)"
```

Restored files the test runner rewrote (line endings, `Rplots.pdf`) after each
run. All scratch, logs and temporary libraries are in `.blbl-tmp/`.

## Deferred

- `blr_segment()` (the "Event By Attributes" printer, `R/blr-output.R:217`) has
  the same two cosmetic problems in its header: it still prints `pred` and its
  returned `segment_data` does not store the predictor name. It is not one of
  the M7-listed printers, and fixing the header would require adding a new
  element to the returned object (an API change). Its factor codes were
  converted to levels and its `1s%` column moved to the percentage scale as
  part of the M5 consistency sweep, so only the header remains.
- The app-side H10 call gate (`inst/app-blorr/logic/logic_bivar.R`) is in the
  xplorerr worktree, not this one, and was not touched.
- Unrelated findings in the audit (H9/H11, M1-M4, M6) are other packages/tasks.
