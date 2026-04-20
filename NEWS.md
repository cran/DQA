# DQA

##  0.1.1 (2026-04-18)

### Fixed
- Fixed bug in Item_Missing_Check()

---

## 0.1.0 (2025-12-12)

### Added
 - Initial public release of the DQA package.
 - Functions for assessing multiple dimensions of data quality:
 - completeness, plausibility, concordance, conformance, currency, timeliness, correctness.
 - Documentation generated with roxygen2 for exported functions.
 - A set of unit tests (testthat) covering core functionality.
 - README, LICENSE (MIT) and basic vignettes included.

### Notes
 - Examples that are time-consuming are wrapped in \donttest{} to keep CRAN checks fast.
 - No special system libraries are required to build this package. On Windows, Rtools is required to build from source.
 

```
