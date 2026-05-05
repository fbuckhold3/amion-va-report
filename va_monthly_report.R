# =============================================================================
# VA Monthly Rotation Report — scheduled job
# SLU Internal Medicine Residency
#
# Pulls live schedule data from the Amion API and writes a formatted Excel
# workbook for the target month. The Rmd wrapper used by Posit Connect
# (va_monthly_report.Rmd) sources this file.
#
# All real logic lives in R/va_report.R so the Shiny app (app.R) and this
# scheduled script call the same code.
# =============================================================================

source("R/va_report.R")

# -----------------------------------------------------------------------------
# Target month — env-var override for Connect; defaults to current month
# -----------------------------------------------------------------------------
env_month <- Sys.getenv("REPORT_MONTH", unset = NA)
env_year  <- Sys.getenv("REPORT_YEAR",  unset = NA)
if (!is.na(env_month) && nzchar(env_month)) {
  REPORT_MONTH <- as.integer(env_month)
  REPORT_YEAR  <- as.integer(env_year)
} else {
  today_ <- Sys.Date()
  REPORT_MONTH <- as.integer(format(today_, "%m"))
  REPORT_YEAR  <- as.integer(format(today_, "%Y"))
}

FILTER_YEAR  <- NULL
FILTER_CLASS <- NULL
OUTPUT_DIR   <- Sys.getenv("OUTPUT_DIR", unset = ".")

# -----------------------------------------------------------------------------
# Fetch + build + write
# -----------------------------------------------------------------------------
message("Fetching Amion data...")
data_all <- fetch_amion_data()

message(sprintf("Building report for %s %d...",
                month.name[REPORT_MONTH], REPORT_YEAR))
report <- build_va_report(
  data_all, REPORT_MONTH, REPORT_YEAR,
  filter_year = FILTER_YEAR, filter_class = FILTER_CLASS
)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
fname <- file.path(OUTPUT_DIR,
                   sprintf("VA_Report_%s_%d.xlsx",
                           month.abb[REPORT_MONTH], REPORT_YEAR))
write_va_workbook(report, fname)

# Expose names the Rmd wrapper renders into its summary table
summary_wide <- report$summary
va_data      <- report$va_data
va_capped    <- report$va_capped
month_label  <- report$month_label

message(sprintf("Report saved: %s", fname))
message(sprintf("  Residents in report : %d", nrow(summary_wide)))
message(sprintf("  Total VA days logged: %.1f",
                sum(summary_wide$Total_VA_Days, na.rm = TRUE)))
message(sprintf("  Residents on elective: %d (need manual VA reconciliation)",
                nrow(report$electives)))
