# =============================================================================
# Posit Connect Cloud deployment helper
#
# Two pieces of content are deployed:
#   1. va-monthly-rotation-report  (Rmd)   — scheduled job, emails xlsx
#   2. va-rotation-app             (Shiny) — interactive viewer + download
#
# Both share R/va_report.R for the actual logic.
#
# One-time setup (do this once per machine):
#   rsconnect::addServer(
#     url  = "https://api.connect.posit.cloud/",  # adjust for your tenant
#     name = "connect-cloud"
#   )
#   rsconnect::connectApiUser(
#     account = "<your-username>",
#     server  = "connect-cloud",
#     apiKey  = "<your-api-key>"
#   )
#
# Then to deploy or redeploy:
#   source("deploy.R")           # deploys both
#   deploy_scheduled_report()    # just the .Rmd
#   deploy_shiny_app()           # just the app
# =============================================================================

SERVER_NAME <- "connect-cloud"   # change to whatever you named your server

shared_files <- c("R/va_report.R", "renv.lock", ".Rprofile", "README.md")

deploy_scheduled_report <- function() {
  rsconnect::deployApp(
    appDir        = ".",
    appPrimaryDoc = "va_monthly_report.Rmd",
    appFiles      = c("va_monthly_report.Rmd", "va_monthly_report.R",
                      shared_files),
    appTitle      = "VA Monthly Rotation Report (scheduled)",
    appName       = "va-monthly-rotation-report",
    server        = SERVER_NAME,
    forceUpdate   = TRUE
  )
}

deploy_shiny_app <- function() {
  rsconnect::deployApp(
    appDir        = ".",
    appPrimaryDoc = "app.R",
    appFiles      = c("app.R", shared_files),
    appTitle      = "VA Rotation Report (interactive)",
    appName       = "va-rotation-app",
    server        = SERVER_NAME,
    forceUpdate   = TRUE
  )
}

if (!interactive() || identical(Sys.getenv("DEPLOY_BOTH"), "1")) {
  deploy_scheduled_report()
  deploy_shiny_app()
}

# After first deploy, configure each in the Connect UI:
#
# Scheduled report (va-monthly-rotation-report):
#   - Schedule: Custom -> last day of Feb, Mar, Apr at 23:00 CT
#               cron: 0 23 L 2,3,4 *
#   - Vars:     AMION_LO=<your Amion Lo= token>
#               (REPORT_MONTH/REPORT_YEAR optional; defaults to current month)
#   - Email:    enable on render
#   - Access:   Specific users or groups (whoever should receive the report)
#
# Interactive app (va-rotation-app):
#   - Vars:     AMION_LO=<your Amion Lo= token>
#   - Access:   Specific users or groups — Connect handles the login wall.
#               There is no in-app password; users sign in with their
#               Posit Connect Cloud account. Add the people who should
#               see the report to the access list.
