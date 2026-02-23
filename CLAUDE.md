# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Rnvonturk is a personal R utilities package for fixed income and empirical finance research. It provides common data retrieval and connection management tasks so they can be reused across projects with a single function call. Data cleaning code for several functions is adapted from [Tidy Finance](https://www.tidy-finance.org/).

Functions:

- **Database connections**: SQL Server, WRDS, and SFTP via `create_connections()`
- **Compustat**: Annual accounting fundamentals via `fetch_compustat_annual()`
- **CRSP monthly**: Monthly stock returns with market cap and CCM links via `fetch_crsp_monthly()`
- **FISD data**: Filtered corporate bond sample via `fetch_fisd_sample()`
- **Trading calendars**: Bond trading day sequences via `fetch_bond_date_sequence()`
- **CRSP returns**: Daily stock returns via `get_daily_returns()`
- **Macro dates**: FOMC meeting dates via `get_fomc_dates()`, BLS release dates via `get_bls_release_dates()`

## Development Commands

```r
# Load package for interactive development
devtools::load_all()

# Regenerate documentation from Roxygen comments (required after editing R/*.R files)
devtools::document()

# Run package checks
devtools::check()

# Install from GitHub
pak::pak("nvonturk/Rnvonturk")
```

## Architecture

**File structure**: Each exported function lives in its own file under `R/`:
- `R/create_connections.R` — database connection management
- `R/fetch_compustat_annual.R` — Compustat annual fundamentals retrieval
- `R/fetch_crsp_monthly.R` — CRSP monthly stock returns retrieval
- `R/fetch_fisd_sample.R` — FISD bond data retrieval
- `R/fetch_bond_date_sequence.R` — trading calendar generation
- `R/get_bls_dates.R` — BLS macro announcement release dates
- `R/get_daily_returns.R` — CRSP daily stock returns retrieval
- `R/get_fomc_dates.R` — FOMC meeting dates and times

**Environment-based credentials**: Database credentials are loaded from a `.env` file at the project root (not tracked in git). Required variables:
- `mssql_uid`, `mssql_pw` — SQL Server credentials
- `WRDS_USER`, `WRDS_PASSWORD` — WRDS PostgreSQL credentials (if using WRDS)
- `sftp_username`, `sftp_pw` — SFTP credentials (if using SFTP)

**Dependencies**: RPostgres and sftp are required dependencies (installed automatically). The sftp package is sourced from GitHub (`stenevang/sftp`) via the `Remotes` field in DESCRIPTION.

**Roxygen2 workflow**: Documentation lives in R source file comments and auto-generates `NAMESPACE` and `man/*.Rd` files. Always run `devtools::document()` after modifying Roxygen comments.
