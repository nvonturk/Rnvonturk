---
name: add-function
description: Add a new exported function to the Rnvonturk R package, including Roxygen docs, dependency updates, documentation updates, and commit.
argument-hint: <function_name> <brief description>
user-invocable: true
allowed-tools: Read, Write, Edit, Glob, Grep, Bash
---

# Add Function to Rnvonturk

Add a new exported function to the Rnvonturk package following established conventions.

## Checklist

Complete every step below. Do NOT skip any step.

### 1. Create or update the R source file

Create `R/<function_name>.R` (one function per file, snake_case name). If the file already exists, add documentation to it.

**Required structure** (match the style of existing files like `fetch_fisd_sample.R`, `get_fomc_dates.R`, `get_bls_dates.R`):

```r
# Suppress R CMD check notes for NSE column names (if using dplyr/tidyverse NSE)
utils::globalVariables(c(
  "col1", "col2"
))

#' Title in Title Case
#'
#' One-paragraph description of what the function does and where
#' the data comes from.
#'
#' @details
#' Additional details, filter criteria, data sources, etc.
#' Use \itemize{ \item ... } for bullet lists.
#'
#' Mention required credentials if applicable.
#'
#' @param param1 Description.
#' @param param2 Description.
#'
#' @return Description of the return value (typically "A tibble with columns: ...").
#'
#' @importFrom pkg fun1 fun2
#' @export
#'
#' @examples
#' \dontrun{
#' result <- function_name(param1 = "value")
#' }
function_name <- function(param1, param2) {
  # implementation
}
```

**Key conventions:**
- `utils::globalVariables()` at the top of the file for any column names used in dplyr NSE (e.g., unquoted column names in `filter()`, `select()`, `mutate()`)
- `@importFrom` for every non-base function used (dplyr, purrr, etc.) — do NOT use `@import` for whole packages
- Always include `@export`
- Examples wrapped in `\dontrun{}` for functions requiring database/API connections
- Use `on.exit()` for cleanup (e.g., `DBI::dbDisconnect`)
- Use `Rnvonturk::create_connections()` for WRDS/SQL Server connections

### 2. Add new dependencies to DESCRIPTION

If the function uses packages not already in `Imports`, add them alphabetically to the `Imports:` field in `DESCRIPTION`. Check existing imports first — common ones already included: dplyr, DBI, lubridate, purrr, RPostgres, tibble, httr2, readxl, stringr, fredr, pbapply.

`parallel` is base R and does NOT need a DESCRIPTION entry.

### 3. Update CLAUDE.md

Add a bullet to the **File structure** section:
```
- `R/<function_name>.R` — short description
```

Keep the list alphabetically ordered by filename.

### 4. Update README.md

Add a bullet to the function list at the top of `README.md`:
```
- `<function_name>()` — one-line description of what it does.
```

Keep consistent with the existing bullet style.

### 5. Regenerate documentation

Run:
```r
devtools::document()
```

Verify that:
- `NAMESPACE` now contains `export(<function_name>)`
- `man/<function_name>.Rd` was created/updated

### 6. Run package checks

Run:
```r
devtools::check()
```

Verify 0 errors and 0 warnings. Pre-existing NOTEs (hidden files, CLAUDE.md, timestamp) are acceptable.

If there are new warnings or errors, fix them before proceeding.

### 7. Bump version

Increment the patch version in `DESCRIPTION` (e.g., 0.3.1 -> 0.3.2).

### 8. Commit and push

Stage all changed files and commit with message format:
```
Add <function_name> function for <brief description>
```

Then push to origin/master.
