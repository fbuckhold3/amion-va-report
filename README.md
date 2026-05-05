# VA Monthly Rotation Report

Live VA rotation reporting for the SLU IM Residency, pulling from Amion.

Two pieces of deployable content share one codebase:

- **Scheduled job** (`va_monthly_report.Rmd`) — runs on Posit Connect on
  the last day of Feb / Mar / Apr, emails out
  `VA_Report_<MMM>_<YYYY>.xlsx`.
- **Interactive Shiny app** (`app.R`) — month/class/AY filters, on-screen
  tables, one-click Excel download. Login-gated through Connect.

Both call shared functions in `R/va_report.R` so logic is in one place.

## What's in the report

For a target month, for each IM resident (R1/R2/R3/Chief, excluding Neuro/
Anesth/Psych):

1. Days at the VA in each of 12 categories: Inpatient Floors, Cardiology
   Consult, Nephrology, ID, ED, Rheumatology, Endocrinology, Continuity
   Clinic, Same Day Clinic, MICU, QI, Other.
2. **Half-day logic**: `0800-1200` → 0.5 (AM), `1300-1700` → 0.5 (PM),
   else 1.0.
3. **1.0/date cap**: If a resident has a full-day rotation row plus an
   on-call row on the same date, only one is counted.
4. **`VACA` is excluded** (it's vacation, not VA — early versions of the
   regex incorrectly matched it).
5. **Electives tab**: residents on Amion's `Elective` assignment for the
   month, with date ranges. Amion doesn't record the elective location, so
   their VA days have to be looked up against the program's elective
   tracking spreadsheet — this tab tells you which residents to check.

## Project layout

```
amion/
├── R/
│   └── va_report.R           # Shared: fetch_amion_data / build_va_report / write_va_workbook
├── app.R                     # Shiny app
├── va_monthly_report.R       # Scheduled job entry point (sources R/va_report.R)
├── va_monthly_report.Rmd     # Connect render wrapper for the scheduled job
├── deploy.R                  # Deploys both content items to Connect Cloud
├── manifest.json             # Generated; rebuilt by deploy.R
├── renv.lock / renv/         # Locked package versions
├── .Rprofile                 # Activates renv
├── .Renviron                 # Local secrets (gitignored) — AMION_LO=...
├── .gitignore
└── README.md
```

## Local setup

```bash
git clone https://github.com/<owner>/amion-va-report.git
cd amion-va-report
```

Restore packages:

```r
renv::restore()
```

Run the scheduled job locally for a specific month:

```bash
REPORT_MONTH=4 REPORT_YEAR=2026 Rscript va_monthly_report.R
```

Run the Shiny app locally:

```r
shiny::runApp(".")
```

## Posit Connect Cloud deployment

Both content items are deployed by `deploy.R`:

```r
# one-time: register the server + API key (see deploy.R header)

source("deploy.R")            # deploys both
# or:
deploy_scheduled_report()
deploy_shiny_app()
```

In Connect's UI for **each** content item:

- **Access** (Settings → Access): set to *Specific users or groups* and
  add the residency administrators. Connect requires login for any
  protected content; the user's Posit Connect Cloud identity is the auth.

For the **scheduled report only**:

- **Schedule** (Settings → Schedule): custom, last day of Feb / Mar /
  Apr at 23:00 CT — cron `0 23 L 2,3,4 *`.
- **Email** (Settings → Email): enable on render so the workbook is
  attached to the email.
- (Optional) `REPORT_MONTH` / `REPORT_YEAR` env vars to target a
  different month than the current one.

### Auth model — how "password protection" works on Connect

Connect Cloud doesn't use a per-app shared password. Instead:

- The content has an **access list**. Anyone not on it cannot open the
  app or see the scheduled report. When they hit the URL they get
  redirected to Posit Connect Cloud's login page.
- Users sign in with their existing Posit Connect Cloud account
  (Google/GitHub/email — managed by Posit, not by you).
- For programmatic / token-based access (e.g. a script triggering a
  render), Connect issues per-user **API keys** under Settings → Account
  on Connect Cloud. Pass those as `Authorization: Key <api-key>` headers.

If you actually need a *single shared password* (e.g. you can't make
every reviewer sign up for Connect), we'd need to add `shinymanager` or
a custom auth layer to the app. Say the word and that's a small follow-up.

The Amion `Lo=` value (`ADMINSLUIM`) is just the program's public lookup
code on amion.com, not a secret. It's hard-coded as the default; override
via the `AMION_LO` env var if another program ever forks this app.

## Reconciling against the manual MDFEA report

The manual report is hand-curated from the program's block schedule, while
this one reads daily Amion shifts. They will not match exactly. Points to
check after each run:

- **Electives tab** — for any resident here, look up the elective location
  in your tracking spreadsheet. If the elective was at the VA, manually add
  those days.
- **Chiefs** — included via `Staff Type = Chief`. Sanity-check against
  whatever your manual chief tracking shows.
- **VA QI / VA Same Day Clinic** — broken out as their own columns since
  they're real VA rotations the manual report counts.

## Adding new academic years

Append a new URL to `AMION_BASE` in `R/va_report.R`:

```r
sprintf(AMION_BASE, amion_lo, "7-26", 365L)
```

`academic_year()` auto-derives the AY label from any date.
