# =============================================================================
# VA Monthly Rotation Report — Shiny app
#
# UX: pick month + year, click Generate, the app fetches that month from
# Amion and renders the report. No data is fetched until the user asks.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(bslib)
})

source("R/va_report.R")

# -----------------------------------------------------------------------------
# Choices and defaults
# -----------------------------------------------------------------------------

months_named <- setNames(1:12, month.name)

year_choices <- AMION_START_AY : (as.integer(format(Sys.Date(), "%Y")) + 1L)

# Default to the *previous* completed month — the typical use-case
default_month <- {
  ref <- seq(Sys.Date(), length.out = 2, by = "-1 month")[2]
  as.integer(format(ref, "%m"))
}
default_year <- {
  ref <- seq(Sys.Date(), length.out = 2, by = "-1 month")[2]
  as.integer(format(ref, "%Y"))
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- page_sidebar(
  title = "VA Rotation Report — SLU IM Residency",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 320,

    # Native HTML <select> — instant, no selectize widget overhead.
    # Side-by-side so it feels like one compact control.
    div(style = "display: flex; gap: 8px;",
        div(style = "flex: 2;",
            selectInput("month", "Month",
                        choices  = months_named,
                        selected = default_month,
                        selectize = FALSE)),
        div(style = "flex: 1;",
            selectInput("year", "Year",
                        choices  = year_choices,
                        selected = default_year,
                        selectize = FALSE))),

    # Quick-jump prev/next month so you don't have to use dropdowns at all
    div(style = "display: flex; gap: 4px; margin-top: -10px;",
        actionButton("prev_month", "<", class = "btn-sm",
                     style = "flex: 1;"),
        actionButton("next_month", ">", class = "btn-sm",
                     style = "flex: 1;")),

    selectInput("class_filter", "Class (optional)",
                choices  = c("All" = "", "R1", "R2", "R3", "Chief"),
                selected = "", multiple = TRUE,
                selectize = FALSE),

    actionButton("generate", "Generate report",
                 class = "btn-primary btn-lg", width = "100%",
                 icon = icon("rotate")),

    hr(),

    downloadButton("dl_xlsx", "Download Excel",
                   class = "btn-success", style = "width: 100%;"),

    hr(),
    htmlOutput("status")
  ),

  card(
    card_header(uiOutput("report_title", style = "font-size: 18px;")),
    uiOutput("name_search_ui"),
    uiOutput("body")
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  # The current report (NULL until user clicks Generate)
  current_report <- reactiveVal(NULL)
  current_label  <- reactiveVal(NULL)

  # Prev/next month navigation
  shift_month <- function(delta) {
    m <- as.integer(input$month)
    y <- as.integer(input$year)
    new_d <- seq(as.Date(sprintf("%d-%02d-15", y, m)),
                 length.out = 2, by = sprintf("%+d month", delta))[2]
    new_m <- as.integer(format(new_d, "%m"))
    new_y <- as.integer(format(new_d, "%Y"))
    if (new_y %in% year_choices) {
      updateSelectInput(session, "month", selected = new_m)
      updateSelectInput(session, "year",  selected = new_y)
    }
  }
  observeEvent(input$prev_month, shift_month(-1))
  observeEvent(input$next_month, shift_month( 1))

  filt_class <- reactive({
    v <- input$class_filter
    v <- v[v != ""]
    if (length(v) == 0) NULL else v
  })

  # ----- Generate: fetch only the requested month, then build the report -----
  observeEvent(input$generate, {
    m <- as.integer(input$month)
    y <- as.integer(input$year)

    showNotification(sprintf("Fetching Amion data for %s %d...",
                             month.name[m], y),
                     type = "message", duration = NULL, id = "fetching")

    res <- tryCatch({
      data_month <- fetch_amion_month(m, y)
      build_va_report(data_month, m, y, filter_class = filt_class())
    }, error = function(e) e)

    removeNotification("fetching")

    if (inherits(res, "error")) {
      showNotification(paste("Failed:", conditionMessage(res)),
                       type = "error", duration = NULL)
      return(invisible(NULL))
    }
    current_report(res)
    current_label(res$month_label)
    showNotification("Report ready.", type = "default", duration = 2)
  }, ignoreNULL = TRUE)

  # ----- Heading -----
  output$report_title <- renderUI({
    lbl <- current_label()
    if (is.null(lbl)) {
      HTML("<span style='color:#888'>Pick a month and year, then click ",
           "<b>Generate report</b>.</span>")
    } else {
      HTML(sprintf("<b>Draft VA data for MDFEA — %s</b>", lbl))
    }
  })

  # ----- Status block -----
  output$status <- renderUI({
    r <- current_report()
    if (is.null(r)) return(HTML("<div style='font-size:12px; color:#888;
                                            margin-top:8px;'>
                                  No report generated yet.</div>"))
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

  # ----- Name search appears once a report is loaded -----
  output$name_search_ui <- renderUI({
    req(current_report())
    div(style = "padding: 8px 0;",
        textInput("name_search", NULL,
                  placeholder = "Search by resident name...",
                  width = "100%"))
  })

  # ----- Body: tabs (only render when a report exists) -----
  output$body <- renderUI({
    if (is.null(current_report())) {
      return(div(class = "alert alert-info",
                 style = "margin-top: 12px;",
                 "Choose a month and year on the left, then click ",
                 strong("Generate report"),
                 " to pull live data from Amion."))
    }
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
                    " Residents on Amion 'Elective' for this month. ",
                    "Cross-reference your elective tracking spreadsheet ",
                    "to add VA days for any of them on a VA-located elective."),
                DTOutput("tbl_electives")),
      nav_panel("Assignment Inventory",
                p(em("All VA-prefixed assignment names — for QA / regex tuning.")),
                DTOutput("tbl_inventory"))
    )
  })

  dt_opts <- list(pageLength = 25, scrollX = TRUE,
                  dom = "ftip", autoWidth = TRUE)

  output$tbl_summary   <- renderDT({
    datatable(current_report()$summary, options = dt_opts, rownames = FALSE,
              class = "compact stripe hover")
  })
  output$tbl_detail    <- renderDT({
    datatable(current_report()$detail, options = dt_opts, rownames = FALSE,
              class = "compact stripe hover")
  })
  output$tbl_class     <- renderDT({
    datatable(current_report()$class_summary,
              options = list(dom = "t", scrollX = TRUE),
              rownames = FALSE, class = "compact stripe hover")
  })
  output$tbl_electives <- renderDT({
    datatable(current_report()$electives, options = dt_opts, rownames = FALSE,
              class = "compact stripe hover")
  })
  output$tbl_inventory <- renderDT({
    datatable(current_report()$assignment_inventory,
              options = dt_opts, rownames = FALSE,
              class = "compact stripe hover")
  })

  # Thread the name-search box into every table's global search
  observeEvent(input$name_search, {
    if (is.null(current_report())) return()
    term <- input$name_search
    if (is.null(term)) term <- ""
    for (id in c("tbl_summary","tbl_detail","tbl_class",
                 "tbl_electives","tbl_inventory")) {
      DT::updateSearch(dataTableProxy(id),
                       keywords = list(global = term))
    }
  }, ignoreNULL = FALSE)

  output$dl_xlsx <- downloadHandler(
    filename = function() {
      r <- current_report()
      if (is.null(r)) return("VA_Report.xlsx")
      sprintf("VA_Report_%s_%d.xlsx",
              month.abb[r$report_month], r$report_year)
    },
    content = function(file) {
      r <- current_report()
      if (is.null(r)) {
        showNotification("Generate a report first.", type = "warning")
        return()
      }
      write_va_workbook(r, file)
    },
    contentType = paste0("application/",
                         "vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  )
}

shinyApp(ui, server)
