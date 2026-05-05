# =============================================================================
# VA Monthly Rotation Report — Shiny app
#
# Lets users select a target month + filters, view the report tabs in-browser,
# and download the same .xlsx the scheduled job produces.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(bslib)
})

source("R/va_report.R")

# -----------------------------------------------------------------------------
# Period choices: single "Month YYYY" dropdown for the last ~5 years through
# next year. Far easier than two separate selects.
# -----------------------------------------------------------------------------

build_period_choices <- function() {
  today  <- Sys.Date()
  ay_start_now <- current_ay_start(today)
  end_year   <- ay_start_now + 1L          # through next AY end
  start_year <- AMION_START_AY             # 2022
  yrs <- start_year:end_year
  periods <- expand.grid(year = yrs, m = 1:12)
  periods <- periods[order(periods$year, periods$m), ]
  # Hide far-future months that have no possible data
  cutoff <- as.Date(sprintf("%d-%02d-01", end_year + 1L, 1))
  periods <- periods[as.Date(sprintf("%d-%02d-01", periods$year, periods$m)) < cutoff, ]
  labels <- sprintf("%s %d", month.name[periods$m], periods$year)
  values <- sprintf("%d-%02d", periods$year, periods$m)
  setNames(values, labels)
}

default_period <- function() {
  today <- Sys.Date()
  # Default to *previous* completed month — that's almost always what you want
  ref <- seq(today, length.out = 2, by = "-1 month")[2]
  sprintf("%d-%02d", as.integer(format(ref, "%Y")), as.integer(format(ref, "%m")))
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- page_sidebar(
  title = "VA Rotation Report — SLU IM Residency",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 320,
    selectInput("period", "Report period",
                choices = build_period_choices(),
                selected = default_period(),
                selectize = TRUE),

    selectInput("class_filter", "Class",
                choices  = c("All" = "", "R1", "R2", "R3", "Chief"),
                selected = "", multiple = TRUE),

    selectInput("ay_filter", "Academic year",
                choices = c("All" = ""),  # populated reactively
                selected = ""),

    actionButton("generate", "Generate report",
                 class = "btn-primary", width = "100%",
                 icon = icon("rotate")),

    hr(),

    downloadButton("dl_xlsx", "Download Excel report",
                   class = "btn-success", style = "width: 100%;"),

    hr(),

    actionLink("refetch", "Refresh data from Amion"),
    htmlOutput("status")
  ),

  card(
    card_header(textInput("name_search", NULL,
                          placeholder = "Search by resident name...",
                          width = "100%")),
    navset_card_tab(
      nav_panel("Summary",
                p(em("One row per resident; columns are VA categories. ",
                     "Type a name above or use the box at top-right of the table.")),
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
                p(em("All VA-prefixed assignment names — for QA / regex tuning.")),
                DTOutput("tbl_inventory"))
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  data_all <- reactiveVal(NULL)

  fetch_now <- function() {
    showNotification("Fetching Amion data...", type = "message",
                     duration = NULL, id = "fetching")
    res <- tryCatch(fetch_amion_data(), error = function(e) e)
    removeNotification("fetching")
    if (inherits(res, "error")) {
      showNotification(paste("Fetch failed:", conditionMessage(res)),
                       type = "error", duration = NULL)
      return(NULL)
    }
    data_all(res)
    showNotification("Amion data loaded.", type = "default", duration = 2)
  }

  observe({ if (is.null(data_all())) fetch_now() })
  observeEvent(input$refetch, { fetch_now() })

  observe({
    req(data_all())
    ays <- sort(unique(data_all()$Academic_Year))
    current <- isolate(input$ay_filter)
    if (is.null(current)) current <- ""
    updateSelectInput(session, "ay_filter",
                      choices = c("All" = "", ays),
                      selected = current)
  })

  # ----- Report rebuilds ONLY when "Generate report" is clicked -----
  parse_period <- function(s) {
    p <- strsplit(s, "-", fixed = TRUE)[[1]]
    list(year = as.integer(p[1]), month = as.integer(p[2]))
  }

  filt_year <- reactive({
    v <- input$ay_filter
    if (is.null(v) || identical(v, "") || all(v == "")) NULL else v
  })
  filt_class <- reactive({
    v <- input$class_filter
    v <- v[v != ""]
    if (length(v) == 0) NULL else v
  })

  report <- eventReactive(
    eventExpr = list(input$generate, data_all()),
    valueExpr = {
      req(data_all(), input$period)
      p <- parse_period(input$period)
      build_va_report(
        data_all(),
        report_month = p$month,
        report_year  = p$year,
        filter_year  = filt_year(),
        filter_class = filt_class()
      )
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  output$status <- renderUI({
    r <- report()
    HTML(sprintf(
      "<div style='font-size:12px; color:#555; margin-top:8px;'>
         <b>%s</b><br>
         Residents: %d<br>
         Total VA days: %.1f<br>
         On elective: %d
       </div>",
      r$month_label, nrow(r$summary),
      sum(r$summary$Total_VA_Days, na.rm = TRUE),
      nrow(r$electives)
    ))
  })

  # ----- DT tables — global search box only, no per-column clutter -----
  dt_opts <- list(
    pageLength = 25,
    scrollX    = TRUE,
    dom        = "ftip",     # filter (search), table, info, pagination
    autoWidth  = TRUE
  )

  # Apply name-search across every table by piping it into DT's search
  apply_search <- function(proxy_id, term) {
    if (is.null(term)) term <- ""
    DT::updateSearch(dataTableProxy(proxy_id),
                     keywords = list(global = term))
  }
  observeEvent(input$name_search, {
    for (id in c("tbl_summary","tbl_detail","tbl_class",
                 "tbl_electives","tbl_inventory")) {
      apply_search(id, input$name_search)
    }
  }, ignoreNULL = FALSE)

  output$tbl_summary <- renderDT({
    datatable(report()$summary, options = dt_opts, rownames = FALSE,
              class = "compact stripe hover")
  })
  output$tbl_detail <- renderDT({
    datatable(report()$detail, options = dt_opts, rownames = FALSE,
              class = "compact stripe hover")
  })
  output$tbl_class <- renderDT({
    datatable(report()$class_summary,
              options = list(dom = "t", scrollX = TRUE),
              rownames = FALSE, class = "compact stripe hover")
  })
  output$tbl_electives <- renderDT({
    datatable(report()$electives, options = dt_opts, rownames = FALSE,
              class = "compact stripe hover")
  })
  output$tbl_inventory <- renderDT({
    datatable(report()$assignment_inventory, options = dt_opts, rownames = FALSE,
              class = "compact stripe hover")
  })

  output$dl_xlsx <- downloadHandler(
    filename = function() {
      p <- parse_period(input$period)
      sprintf("VA_Report_%s_%d.xlsx", month.abb[p$month], p$year)
    },
    content = function(file) write_va_workbook(report(), file),
    contentType = paste0("application/",
                         "vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  )
}

shinyApp(ui, server)
