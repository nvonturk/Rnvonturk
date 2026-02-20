# Rnvonturk

Personal R utilities package for fixed income and empirical finance research. Provides common data retrieval and connection management tasks so they can be reused across projects with a single function call.

## Functions

- `create_connections()` — establishes a SQL Server connection and optionally WRDS and SFTP connections using credentials stored in environment variables (loaded from a project `.env` file).
- `fetch_fisd_sample()` — retrieves a filtered sample of corporate bond data from FISD via WRDS, applying standard research filters.
- `fetch_bond_date_sequence()` — generates a date sequence with trading day indicators using the QuantLib US Government Bond calendar.
- `get_bls_release_dates()` — downloads BLS macro announcement release dates (Employment Situation, CPI, PPI) from the FRED API.
- `get_daily_returns()` — retrieves daily stock returns from CRSP via WRDS, filtered to common US equities, with parallel batch processing.
- `get_fomc_dates()` — downloads FOMC meeting dates and announcement times from SF Fed Excel files.

## Acknowledgments

Data cleaning code for several functions is adapted from [Tidy Finance](https://www.tidy-finance.org/).

---

## Installation

Install from GitHub (private repo):

```r
install.packages("pak")
pak::pak("nvonturk/Rnvonturk")
