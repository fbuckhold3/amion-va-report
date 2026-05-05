# =============================================================================
# Shared logic for the VA monthly rotation report.
#
# Provides:
#   fetch_amion_data(amion_lo, urls = NULL)         -> tibble of all rows
#   build_va_report(data_all, month, year, ...)     -> list of report tables
#   write_va_workbook(report, path)                 -> writes .xlsx
#
# The scheduled script (va_monthly_report.R) and the Shiny app (app.R)
# both call these — keeps the logic in one place.
# =============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(openxlsx)
})

AMION_COLS <- c(
  "Name", "Staff ID", "Backup Staff ID", "Assignment Name",
  "Assignment ID", "Backup Assignment ID", "Date", "Start Time",
  "End Time", "Staff Type", "Pager", "Tel", "Email", "Messagable",
  "Shift Note", "Assignment Type", "Grouping"
)

VA_CATEGORIES <- c(
  "VA Inpatient Floors", "VA Cardiology Consult", "VA Nephrology",
  "VA Infectious Disease", "VA Emergency Dept", "VA Rheumatology",
  "VA Endocrinology", "VA Continuity Clinic", "VA Same Day Clinic",
  "VA MICU", "VA QI", "VA Other"
)

EXCLUDE_STAFF_TYPES <- c("Neuro", "Anesth", "Psych")

AMION_BASE <- "http://www.amion.com/cgi-bin/ocs?Lo=%s&Rpt=625c&Month=%s&Days=%d"

# -----------------------------------------------------------------------------
# Data fetch
# -----------------------------------------------------------------------------

#' Fetch all Amion year-blocks and return a single combined data frame
#' @param amion_lo The Lo= value (program token) from Amion
#' @param urls Optional vector of full URLs; otherwise built from year blocks
fetch_amion_data <- function(amion_lo = Sys.getenv("AMION_LO"),
                             urls = NULL) {
  if (!nzchar(amion_lo) && is.null(urls)) {
    stop("AMION_LO is not set. Add it to .Renviron locally or to Posit ",
         "Connect Vars.")
  }
  if (is.null(urls)) {
    urls <- c(
      sprintf(AMION_BASE, amion_lo, "7-22", 365L),
      sprintf(AMION_BASE, amion_lo, "7-23", 366L),
      sprintf(AMION_BASE, amion_lo, "7-24", 365L),
      sprintf(AMION_BASE, amion_lo, "7-25", 365L)
    )
  }
  read_one <- function(url) {
    resp <- GET(url, timeout(60))
    stop_for_status(resp)
    txt <- content(resp, as = "text", encoding = "UTF-8")
    lines <- unlist(strsplit(txt, "\n"))
    lines <- lines[-(1:8)]
    df <- read.csv(text = paste(lines, collapse = "\n"),
                   header = FALSE, stringsAsFactors = FALSE)
    colnames(df) <- AMION_COLS
    df
  }
  data_all <- do.call(rbind, lapply(urls, read_one))
  data_all$Date <- mdy(data_all$Date)
  data_all$Academic_Year <- academic_year(data_all$Date)
  data_all
}

academic_year <- function(d) {
  yr      <- year(d)
  start_y <- ifelse(month(d) >= 7, yr, yr - 1)
  paste0(start_y, "-", start_y + 1)
}

# -----------------------------------------------------------------------------
# Classification helpers
# -----------------------------------------------------------------------------

classify_va <- function(name) {
  case_when(
    grepl("VA Floors|VA Floor ", name, ignore.case = TRUE)    ~ "VA Inpatient Floors",
    grepl("VA Cards|VA Cardiology", name, ignore.case = TRUE) ~ "VA Cardiology Consult",
    grepl("VA Nephro", name, ignore.case = TRUE)              ~ "VA Nephrology",
    grepl("VA ID\\b|VA Infectious", name, ignore.case = TRUE) ~ "VA Infectious Disease",
    grepl("VA ED\\b|VA Emergency", name, ignore.case = TRUE)  ~ "VA Emergency Dept",
    grepl("VA Rheum", name, ignore.case = TRUE)               ~ "VA Rheumatology",
    grepl("VA Endo", name, ignore.case = TRUE)                ~ "VA Endocrinology",
    grepl("VA Clinic|VA PC|VA Primary", name, ignore.case = TRUE) ~ "VA Continuity Clinic",
    grepl("VA MICU|VA ICU", name, ignore.case = TRUE)         ~ "VA MICU",
    grepl("VA QI", name, ignore.case = TRUE)                  ~ "VA QI",
    grepl("VA Same Day|VA Same day", name, ignore.case = TRUE) ~ "VA Same Day Clinic",
    TRUE                                                       ~ "VA Other"
  )
}

to_int_time <- function(x) {
  s <- formatC(suppressWarnings(as.integer(x)), width = 4, flag = "0")
  suppressWarnings(as.integer(s))
}

assign_priority <- function(at) {
  case_when(
    tolower(at) == "r" ~ 1L,
    tolower(at) == "o" ~ 2L,
    tolower(at) == "c" ~ 3L,
    tolower(at) == "m" ~ 4L,
    TRUE               ~ 5L
  )
}

collapse_ranges <- function(dates) {
  if (length(dates) == 0) return("")
  d <- sort(unique(dates))
  brk <- c(0, which(diff(d) != 1), length(d))
  out <- character(length(brk) - 1)
  for (i in seq_len(length(brk) - 1)) {
    s <- d[brk[i] + 1]; e <- d[brk[i + 1]]
    out[i] <- if (s == e) format(s, "%m/%d") else
              sprintf("%s-%s", format(s, "%m/%d"), format(e, "%m/%d"))
  }
  paste(out, collapse = ", ")
}

# -----------------------------------------------------------------------------
# Build report tables
# -----------------------------------------------------------------------------

#' Build the full set of report tables for a given target month
#' @param data_all Tibble from fetch_amion_data()
#' @param report_month integer 1-12
#' @param report_year integer
#' @param filter_year Optional academic year string (e.g. "2025-2026")
#' @param filter_class Optional vector of classes (e.g. c("R1","R2"))
#' @return list with elements: month_label, summary, detail, class_summary,
#'                              assignment_inventory, electives, va_data, va_capped
build_va_report <- function(data_all,
                            report_month,
                            report_year,
                            filter_year  = NULL,
                            filter_class = NULL) {

  data_month <- data_all %>%
    filter(month(Date) == report_month, year(Date) == report_year) %>%
    filter(!grepl(paste(EXCLUDE_STAFF_TYPES, collapse = "|"),
                  `Staff Type`, ignore.case = TRUE)) %>%
    filter(`Staff Type` %in% c("R1", "R2", "R3", "Chief"))

  if (!is.null(filter_year))  data_month <- data_month %>% filter(Academic_Year %in% filter_year)
  if (!is.null(filter_class)) data_month <- data_month %>% filter(`Staff Type` %in% filter_class)

  # ----- VA classification + per-row day value -----
  va_data <- data_month %>%
    # ^VA followed by space/slash/hyphen/end (excludes VACA = vacation)
    filter(grepl("^VA([ /\\-]|$)", `Assignment Name`, ignore.case = TRUE)) %>%
    mutate(
      VA_Category = classify_va(`Assignment Name`),
      s_int = to_int_time(`Start Time`),
      e_int = to_int_time(`End Time`),
      Day_Value = case_when(
        s_int ==  800 & e_int == 1200 ~ 0.5,
        s_int == 1300 & e_int == 1700 ~ 0.5,
        TRUE                          ~ 1.0
      ),
      Half = case_when(
        s_int ==  800 & e_int == 1200 ~ "AM",
        s_int == 1300 & e_int == 1700 ~ "PM",
        TRUE                          ~ "Full"
      ),
      prio = assign_priority(`Assignment Type`)
    )

  # ----- Cap at 1.0 day per (resident, date) -----
  va_capped <- va_data %>%
    group_by(Name, Date) %>%
    group_modify(~ {
      g <- .x
      if (any(g$Day_Value == 1.0)) {
        keep <- g %>% filter(Day_Value == 1.0) %>%
          arrange(prio, `Assignment Name`) %>% slice(1)
        keep$Day_Value <- 1.0
        keep
      } else {
        total <- sum(g$Day_Value)
        if (total > 1.0) g$Day_Value <- g$Day_Value * (1.0 / total)
        g
      }
    }) %>%
    ungroup()

  month_label <- paste(month.name[report_month], report_year)

  # ----- Summary -----
  summary_long <- va_capped %>%
    group_by(Name, `Staff Type`, Academic_Year, VA_Category) %>%
    summarise(Days = sum(Day_Value), .groups = "drop")

  summary_wide <- summary_long %>%
    pivot_wider(names_from = VA_Category, values_from = Days, values_fill = 0)
  for (cat in VA_CATEGORIES) if (!cat %in% names(summary_wide)) summary_wide[[cat]] <- 0
  summary_wide <- summary_wide %>%
    select(Name, `Staff Type`, Academic_Year, all_of(VA_CATEGORIES)) %>%
    mutate(Total_VA_Days = rowSums(across(all_of(VA_CATEGORIES))))

  all_im <- data_month %>% distinct(Name, `Staff Type`, Academic_Year)
  summary_wide <- all_im %>%
    left_join(summary_wide, by = c("Name", "Staff Type", "Academic_Year")) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    arrange(`Staff Type`, Name) %>%
    rename(Resident = Name, Class = `Staff Type`, Acad_Year = Academic_Year)

  # ----- Daily detail -----
  detail <- va_capped %>%
    select(Resident = Name, Class = `Staff Type`, Acad_Year = Academic_Year,
           Date, VA_Category, Assignment = `Assignment Name`,
           Start_Time = `Start Time`, End_Time = `End Time`, Half, Day_Value) %>%
    arrange(Class, Resident, Date, Half)

  # ----- Class summary -----
  class_summary <- va_capped %>%
    group_by(Class = `Staff Type`, VA_Category) %>%
    summarise(Days = sum(Day_Value), .groups = "drop") %>%
    pivot_wider(names_from = VA_Category, values_from = Days, values_fill = 0)
  for (cat in VA_CATEGORIES) if (!cat %in% names(class_summary)) class_summary[[cat]] <- 0
  class_summary <- class_summary %>%
    select(Class, all_of(VA_CATEGORIES)) %>%
    mutate(Total = rowSums(across(all_of(VA_CATEGORIES)))) %>%
    arrange(Class)

  # ----- Assignment inventory -----
  assignment_inventory <- va_data %>%
    group_by(`Assignment Name`, VA_Category, Half) %>%
    summarise(Rows = n(), .groups = "drop") %>%
    arrange(VA_Category, desc(Rows))

  # ----- Electives — for manual VA reconciliation -----
  electives <- data_month %>%
    filter(`Assignment Name` == "Elective") %>%
    arrange(Name, Date) %>%
    group_by(Name, `Staff Type`, Academic_Year) %>%
    summarise(
      Elective_Days = n(),
      Date_Ranges   = collapse_ranges(Date),
      Action        = "Check elective spreadsheet for VA days",
      .groups = "drop"
    ) %>%
    rename(Resident = Name, Class = `Staff Type`, Acad_Year = Academic_Year) %>%
    arrange(Class, Resident)

  list(
    month_label          = month_label,
    report_month         = report_month,
    report_year          = report_year,
    summary              = summary_wide,
    detail               = detail,
    class_summary        = class_summary,
    assignment_inventory = assignment_inventory,
    electives            = electives,
    va_data              = va_data,
    va_capped            = va_capped
  )
}

# -----------------------------------------------------------------------------
# Write workbook
# -----------------------------------------------------------------------------

#' Write the report to an .xlsx workbook
#' @param report The list returned by build_va_report()
#' @param path   Output file path
write_va_workbook <- function(report, path) {

  wb <- createWorkbook()
  style_title  <- createStyle(fontSize = 13, fontColour = "#FFFFFF",
                              fgFill = "#2c3e50", halign = "LEFT",
                              textDecoration = "bold")
  style_header <- createStyle(fontSize = 10, fontColour = "#FFFFFF",
                              fgFill = "#34495e", halign = "CENTER",
                              textDecoration = "bold", wrapText = TRUE,
                              border = "Bottom", borderColour = "#FFFFFF")
  style_data   <- createStyle(fontSize = 9, halign = "LEFT")
  style_number <- createStyle(fontSize = 9, halign = "CENTER", numFmt = "0.0")
  style_zero   <- createStyle(fontSize = 9, halign = "CENTER",
                              fontColour = "#AAAAAA", numFmt = "0.0")
  style_date   <- createStyle(fontSize = 9, numFmt = "MM/DD/YYYY", halign = "CENTER")

  write_sheet <- function(sheet_name, title_text, df,
                          date_cols = NULL, number_cols = NULL,
                          freeze_col = 1) {
    addWorksheet(wb, sheet_name, gridLines = FALSE)
    writeData(wb, sheet_name, title_text, startRow = 1, startCol = 1)
    mergeCells(wb, sheet_name, rows = 1, cols = 1:ncol(df))
    addStyle(wb, sheet_name, style_title, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
    setRowHeights(wb, sheet_name, rows = 1, heights = 22)
    setRowHeights(wb, sheet_name, rows = 2, heights = 4)
    writeDataTable(wb, sheet_name, df, startRow = 3, startCol = 1,
                   tableStyle = "TableStyleLight9",
                   withFilter = TRUE, bandedRows = TRUE)
    addStyle(wb, sheet_name, style_header,
             rows = 3, cols = 1:ncol(df), gridExpand = TRUE)
    if (nrow(df) > 0) {
      data_rows <- 4:(nrow(df) + 3)
      addStyle(wb, sheet_name, style_data,
               rows = data_rows, cols = 1:min(3, ncol(df)), gridExpand = TRUE)
      if (!is.null(number_cols)) {
        for (col_idx in number_cols) {
          col_data <- df[[col_idx]]
          for (r in seq_along(col_data)) {
            s <- if (!is.na(col_data[r]) && col_data[r] == 0) style_zero else style_number
            addStyle(wb, sheet_name, s, rows = r + 3, cols = col_idx)
          }
        }
      }
      if (!is.null(date_cols)) {
        addStyle(wb, sheet_name, style_date,
                 rows = data_rows, cols = date_cols, gridExpand = TRUE)
      }
    }
    freezePane(wb, sheet_name, firstActiveRow = 4,
               firstActiveCol = freeze_col + 1)
    setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = "auto")
  }

  ml <- report$month_label

  write_sheet("Summary", paste("VA Rotation Summary —", ml),
              report$summary,
              number_cols = which(sapply(report$summary, is.numeric)))

  write_sheet("Daily Detail", paste("VA Daily Assignment Detail —", ml),
              report$detail,
              date_cols   = which(colnames(report$detail) == "Date"),
              number_cols = which(colnames(report$detail) == "Day_Value"),
              freeze_col  = 2)

  write_sheet("Class Summary", paste("VA Days by Class —", ml),
              report$class_summary,
              number_cols = which(sapply(report$class_summary, is.numeric)))

  write_sheet("Assignment Inventory",
              paste("All VA Assignment Names Seen —", ml),
              report$assignment_inventory,
              number_cols = which(colnames(report$assignment_inventory) == "Rows"))

  write_sheet("Electives - Reconcile",
              paste("Residents on Elective —", ml,
                    "(verify VA days against elective spreadsheet)"),
              report$electives,
              number_cols = which(colnames(report$electives) == "Elective_Days"),
              freeze_col  = 2)

  # How To Use tab
  addWorksheet(wb, "How To Use", gridLines = FALSE)
  instructions <- data.frame(
    Topic = c(
      "Run interactively", "Run on Posit Connect",
      "Override target month",
      "Filter by academic year", "Filter by class",
      "---",
      "Tab: Summary", "Tab: Daily Detail", "Tab: Class Summary",
      "Tab: Assignment Inventory", "Tab: Electives - Reconcile",
      "---",
      "Half-day rule", "Same-date cap",
      "---",
      "MANUAL RECONCILIATION REQUIRED",
      "Electives at VA", "VA QI", "VA Same Day Clinic", "Chief residents"
    ),
    Description = c(
      "Edit REPORT_MONTH/REPORT_YEAR or use the Shiny app.",
      "Schedule the script in Connect; defaults to current month.",
      "Set env vars REPORT_MONTH=4 REPORT_YEAR=2026.",
      "FILTER_YEAR='2025-2026' (NULL = all years).",
      "FILTER_CLASS=c('R1','R2') (NULL = all classes).",
      "",
      "One row per IM resident; columns are VA categories.",
      "One row per VA day-component (after capping). Verify specific dates.",
      "VA days aggregated by R1/R2/R3/Chief.",
      "Raw assignment names matching '^VA*'. QA / regex tuning.",
      "Residents on Amion 'Elective' assignment with date ranges. Cross-reference your elective spreadsheet to add VA days for these people.",
      "",
      "0800-1200 -> 0.5 (AM); 1300-1700 -> 0.5 (PM); else -> 1.0.",
      "Per (resident,date): full-day rows dedupe to one (priority r>o>others); half-days sum capped at 1.0.",
      "",
      "Items below need human reconciliation against other source documents.",
      "Residents on 'elective' rotations at the VA do NOT appear as VA-prefixed assignments. See the Electives tab.",
      "VA QI is a real rotation that counts toward VA FTE; broken out as its own column.",
      "VA Same Day = AM/PM same-day clinic blocks at the VA; broken out as its own column.",
      "Chief residents (Staff Type = 'Chief' in Amion) are included alongside R1/R2/R3."
    )
  )
  writeDataTable(wb, "How To Use", instructions, startRow = 2, startCol = 1,
                 tableStyle = "TableStyleLight2")
  setColWidths(wb, "How To Use", cols = 1:2, widths = c(28, 90))

  saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
