---
name: update-function
description: Update an existing exported function in the Rnvonturk R package, regenerate docs, bump the minor version, commit, and push.
argument-hint: <function_name> <brief description of changes>
user-invocable: true
allowed-tools: Read, Write, Edit, Glob, Grep, Bash
---

# Update Function in Rnvonturk

Update an existing exported function in the Rnvonturk package, regenerate documentation, bump the minor version, commit, and push.

## Checklist

Complete every step below. Do NOT skip any step.

### 1. Apply the code changes

Edit the relevant file(s) under `R/`. If the user has already made changes in conversation, verify those changes are saved. If not, apply the requested modifications now.

**Conventions to maintain:**
- `utils::globalVariables()` at the top of the file for any column names used in dplyr NSE
- `@importFrom` for every non-base function used — do NOT use `@import` for whole packages
- Keep `@export` on all public functions
- Examples wrapped in `\dontrun{}` for functions requiring database/API connections
- Use `on.exit()` for cleanup (e.g., `DBI::dbDisconnect`)

### 2. Update Roxygen documentation

If the function signature, parameters, or return value changed, update the Roxygen comments in the source file to match.

### 3. Add new dependencies to DESCRIPTION (if any)

If the changes introduce packages not already in `Imports`, add them alphabetically to the `Imports:` field in `DESCRIPTION`.

### 4. Update CLAUDE.md (if needed)

If the function's purpose or file location changed, update the relevant bullet in the **Functions** list and the **File structure** section.

### 5. Update README.md (if needed)

If the function's purpose or interface changed materially, update the corresponding bullet in `README.md`.

### 6. Regenerate documentation

Run:
```r
devtools::document()
```

Verify that:
- `NAMESPACE` exports are correct
- `man/<function_name>.Rd` was updated

### 7. Run package checks

Run:
```r
devtools::check()
```

Verify 0 errors and 0 warnings. Pre-existing NOTEs (hidden files, CLAUDE.md, timestamp) are acceptable.

If there are new warnings or errors, fix them before proceeding.

### 8. Bump minor version

Increment the **minor** version in `DESCRIPTION` (e.g., 1.1.2 → 1.2.0). Reset the patch version to 0 when bumping minor.

### 9. Commit and push

Stage all changed files and commit with message format:
```
Update <function_name>: <brief description of changes>
```

Then push to origin/main.
