# CLAUDE.md - GlassnodeR

## Project Overview

GlassnodeR is an R package that wraps the Glassnode API, returning cryptocurrency on-chain metrics as tidy tibbles. Covers Bitcoin, Ethereum, and other supported assets.

**Author:** James Chapman (Bank of Canada)
**License:** MIT
**Version:** 0.0.1

## Repository Structure

```
R/              # Source code organized by Glassnode API category
  utils.R       # Core API utilities (call_glassnode_api, make_params, sanitize_date)
  addresses.R   # Address metrics
  blockchain.R  # Block data metrics
  defi.R        # DeFi metrics (TVL, bridges)
  distribution.R # Distribution metrics (exchange/miner balances, gini, HODL waves)
  fees.R        # Fee metrics
  lightning.R   # Lightning Network metrics
  market.R      # Market metrics (price, MVRV, marketcap)
  mempool.R     # Mempool metrics
  mining.R      # Mining metrics
  supply.R      # Supply metrics
  transactions.R # Transaction metrics
man/            # Auto-generated roxygen2 documentation (do not edit by hand)
```

## Build & Development Commands

```r
devtools::document()   # Regenerate NAMESPACE and man/ pages from roxygen2 comments
devtools::check()      # Run R CMD check (linting, examples, CRAN checks)
devtools::install()    # Install package locally
devtools::build()      # Build source tarball
devtools::load_all()   # Load package for interactive development
```

Equivalent CLI: `R CMD build .` and `R CMD check GlassnodeR_0.0.1.tar.gz`

## Testing

The package declares `testthat (>= 3.0.0)` in Suggests but has no test suite yet. Tests would go in `tests/testthat/`.

## API Key

Functions use `Sys.getenv("GN_API_KEY")` as the default API key. The internal `call_glassnode_api()` in `utils.R` also checks `GLASSNODE_API_KEY`.

## Coding Conventions

- **Function naming:** `get_<metric>()` for all public functions, snake_case throughout
- **Pipe style:** Native R pipe `|>` (no magrittr dependency)
- **Return type:** All public functions return tibbles with a `date` column and metric value columns
- **Documentation:** roxygen2 comments above every exported function; `@rdname` groups related functions
- **Internal functions:** Prefixed with `call_` (e.g., `call_glassnode_api`, `call_address_api`), marked `@noRd`
- **No trailing whitespace or unnecessary blank lines**

## Standard Function Pattern

Every public `get_*` function follows this structure:

1. Accept parameters: `asset`, `since`, `until`, `frequency`, `api_key`, `as_date`
2. Build param list and call `make_params()`
3. Call `call_glassnode_api()` with the appropriate `/v1/metrics/<category>/<endpoint>` path
4. Convert to tibble, rename `t` -> `date` and `v` -> descriptive metric name
5. Convert UNIX timestamps to `POSIXct` (or `Date` when `as_date=TRUE` and `frequency="24h"`)
6. Return the tibble

## Dependencies (Imports)

dplyr, glue, httr, jsonlite, lubridate, plyr, purrr, rlang, tibble, tidyr

## Git Conventions

- **Main branch:** `master`
- **Commit style:** Imperative mood, concise single-line messages (e.g., "Add comprehensive API endpoint coverage for Lightning Network")
- **Worktrees** are used for feature branches
