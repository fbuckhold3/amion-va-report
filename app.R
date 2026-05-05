# =============================================================================
# VA Monthly Rotation Report — Shiny app
#
# Lets users select a target month + filters, view the report tabs in-browser,
# and download the same .xlsx the scheduled job produces.
#
# Auth: deploy on Posit Connect with content access set to "Specific users
# or groups" — Connect handles login. See README for details. The Amion
# program token (Lo=) is read from the AMION_LO env var, set as a Connect
# Var (not in the bundle).
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(bslib)
})

source("R/va_report.R")

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

months_named <- setNames(1:12, month.name)
# Span everything we pull from Amion (AMION_START_AY) through next calendar
# year so future months in the in-progress AY are also pickable.
year_choices <- AMION_START_AY : (as.integer(format(Sys.Date(), "%Y")) + 1L)

ui <- page_sidebar(
  title = "VA Rotation Report — SLU IM Residency",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 320,
    selectInput("month", "Report month",
                choices = months_named,
                selected = as.integer(format(Sys.Date(), "%m"))),
    selectInput("year", "Report year",
                choices = year_choices,
                selected = as.integer(format(Sys.Date(), "%Y"))),
    selectInput("class_filter", "Class filter",
                choices = c("All" = "", "R1", "R2", "R3", "Chief"),
                selected = "", multiple = TRUE),
    selectInput("ay_filter", "Academic year filter",
                choices = c("All" = ""),  # populated reactively from data
                selected = ""),
    actionButton("refresh", "Refresh data from Amion",
                 class = "btn-primary", width = "100%"),
    hr(),
    downloadButton("dl_xlsx", "Download Excel report",
                   class = "btn-success", style = "width: 100%;"),
    hr(),
    htmlOutput("status")
  ),

  navset_card_tab(
    nav_panel("Summary",
              p(em("One row per IM resident; columns are VA categories.")),
              DTOutput("tbl_summary")),
    nav_panel("Daily Detail",
              p(em("One row per VA day-component after the 1.0/date cap.")),
              DTOutput("tbl_detail")),
    nav_panel("Class Summary",
              p(em("VA days aggregated by class.")),
              DTOutput("tbl_class")),
    nav_panel("Electives - Reconcile",
              div(class = "alert alert-warning",
                  strong("Manual reconciliation required."),
                  " Residents on Amion 'Elective' for this month. Cross-",
                  "reference your elective tracking spreadsheet to add VA ",
                  "days for any of them on a VA-located elective."),
              DTOutput("tbl_electives")),
    nav_panel("Assignment Inventory",
              p(em("All VA-prefixed assignment names seen — for QA / regex tuning.")),
              DTOutput("tbl_inventory"))
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  # Data is fetched once per session, refreshable on demand
  data_all <- reactiveVal(NULL)

  fetch_now <- function() {
    showNotification("Fetching Amion data...", type = "message",
                     duration = 3, id = "fetching")
    res <- tryCatch(fetch_amion_data(), error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("Fetch failed:", conditionMessage(res)),
                       type = "error", duration = NULL)
      return(NULL)
    }
    data_all(res)
    removeNotification("fetching")
    showNotification("Amion data refreshed.", type = "default", duration = 2)
  }

  # Initial fetch on session start
  observe({ if (is.null(data_all())) fetch_now() })

  observeEvent(input$refresh, { fetch_now() })

  # Populate the AY filter with whatever years the loaded data actually has
  observe({
    req(data_all())
    ays <- sort(unique(data_all()$Academic_Year))
    current <- isolate(input$ay_filter)
    if (is.null(current)) current <- ""
    updateSelectInput(session, "ay_filter",
                      choices = c("All" = "", ays),
                      selected = current)
  })

  filt_year <- reactive({
    v <- input$ay_filter
    if (is.null(v) || length(v) == 0 || identical(v, "") || all(v == "")) NULL else v
  })
  filt_class <- reactive({
    v <- input$class_filter
    v <- v[v != ""]
    if (length(v) == 0) NULL else v
  })

  report <- reactive({
    req(data_all())
    build_va_report(
      data_all(),
      report_month = as.integer(input$month),
      report_year  = as.integer(input$year),
      filter_year  = filt_year(),
      filter_class = filt_class()
    )
  })

  output$status <- renderUI({
    r <- report()
    HTML(sprintf(
      "<div style='font-size:12px; color:#555'>
         <b>%s</b><br>
         Residents in report: %d<br>
         Total VA days: %.1f<br>
         Residents on elective: %d
       </div>",
      r$month_label, nrow(r$summary),
      sum(r$summary$Total_VA_Days, na.rm = TRUE),
      nrow(r$electives)
    ))
  })

  dt_opts <- list(pageLength = 25, scrollX = TRUE, dom = "ftip")

  output$tbl_summary   <- renderDT(datatable(report()$summary,
                                             options = dt_opts, rownames = FALSE,
                                             filter = "top"))
  output$tbl_detail    <- renderDT(datatable(report()$detail,
                                             options = dt_opts, rownames = FALSE,
                                             filter = "top"))
  output$tbl_class     <- renderDT(datatable(report()$class_summary,
                                             options = list(dom = "t"),
                                             rownames = FALSE))
  output$tbl_electives <- renderDT(datatable(report()$electives,
                                             options = dt_opts, rownames = FALSE,
                                             filter = "top"))
  output$tbl_inventory <- renderDT(datatable(report()$assignment_inventory,
                                             options = dt_opts, rownames = FALSE,
                                             filter = "top"))

  output$dl_xlsx <- downloadHandler(
    filename = function() {
      sprintf("VA_Report_%s_%d.xlsx",
              month.abb[as.integer(input$month)],
              as.integer(input$year))
    },
    content = function(file) {
      write_va_workbook(report(), file)
    },
    contentType = paste0("application/",
                         "vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  )
}

shinyApp(ui, server)
