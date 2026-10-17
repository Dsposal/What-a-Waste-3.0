############################################################
# E-WASTE EXPLORER
# R/trade.R — Chatham House circular economy trade data
#
# PURPOSE
# -------
# Read a csv exported from
#   https://resourcetransitions.earth/data/circular-economy-trade
# (electronics / e-waste category, value units) and
# standardise it to:
#   year | exporter_iso3 | exporter | importer_iso3 |
#   importer | value_usd | weight_kg
#
# WHY THE COLUMN MATCHING IS FLEXIBLE
# -----------------------------------
# Export headers vary between site versions (e.g.
# "Exporter ISO3", "Value (1000USD)", "Weight (1000kg)").
# find_col() matches on patterns and scales values when
# the header says "1000". Check the console message after
# loading to confirm which columns were picked.
#
# AI NOTES
# --------
# If load_trade() errors, print names(raw) and add the
# missing header pattern to the patterns list below.
############################################################

library(readr)
library(dplyr)

# Find the first column whose name matches any pattern
find_col <- function(nms, patterns) {
  for (p in patterns) {
    hit <- grep(p, nms, ignore.case = TRUE, value = TRUE)
    if (length(hit)) return(hit[1])
  }
  NA_character_
}

# Multiplier implied by a header such as "Value (1000USD)"
header_multiplier <- function(header) {
  if (is.na(header)) return(1)
  if (grepl("1000|thousand", header, ignore.case = TRUE)) 1000 else 1
}

#' Load and standardise a trade csv
#' @param path csv exported from resourcetransitions.earth
#' @return tibble in the standard schema (see header)
load_trade <- function(path) {
  raw <- readr::read_csv(path, show_col_types = FALSE)
  nms <- names(raw)

  cols <- list(
    year          = find_col(nms, c("^year$", "year")),
    exporter_iso3 = find_col(nms, c("exporter.*iso", "^exporter.?code", "reporter.*iso")),
    exporter      = find_col(nms, c("^exporter$", "^exporter.?name", "^reporter$")),
    importer_iso3 = find_col(nms, c("importer.*iso", "^importer.?code", "partner.*iso")),
    importer      = find_col(nms, c("^importer$", "^importer.?name", "^partner$")),
    value         = find_col(nms, c("value", "usd", "trade.?value")),
    weight        = find_col(nms, c("weight", "kg", "tonne", "quantity"))
  )

  # A value-units export and a weight-units export are both fine,
  # but the file needs at least one of the two measures.
  required <- c("exporter", "importer")
  missing <- required[vapply(cols[required], is.na, logical(1))]
  if (is.na(cols$value) && is.na(cols$weight)) missing <- c(missing, "value or weight")
  # ISO columns can stand in for name columns
  if ("exporter" %in% missing && !is.na(cols$exporter_iso3)) missing <- setdiff(missing, "exporter")
  if ("importer" %in% missing && !is.na(cols$importer_iso3)) missing <- setdiff(missing, "importer")
  if (length(missing)) {
    stop("Could not find column(s): ", paste(missing, collapse = ", "),
         "\nColumns in file: ", paste(nms, collapse = " | "))
  }

  message("Trade columns used: ",
          paste(names(cols), unlist(cols), sep = " = ", collapse = "; "))

  pick <- function(key) if (is.na(cols[[key]])) NA else raw[[cols[[key]]]]

  tibble(
    year          = suppressWarnings(as.integer(pick("year"))),
    exporter_iso3 = as.character(pick("exporter_iso3")),
    exporter      = as.character(pick("exporter")),
    importer_iso3 = as.character(pick("importer_iso3")),
    importer      = as.character(pick("importer")),
    value_usd     = suppressWarnings(as.numeric(pick("value"))) * header_multiplier(cols$value),
    weight_kg     = suppressWarnings(as.numeric(pick("weight"))) * header_multiplier(cols$weight)
  ) %>%
    mutate(
      exporter = coalesce(exporter, exporter_iso3),
      importer = coalesce(importer, importer_iso3),
      exporter_iso3 = coalesce(exporter_iso3, name_to_iso3(exporter)),
      importer_iso3 = coalesce(importer_iso3, name_to_iso3(importer))
    ) %>%
    filter(!is.na(value_usd) | !is.na(weight_kg), exporter != importer)
}

# Which column to sum: value in USD, or weight converted to tonnes
measure_col <- function(trade, measure) {
  m <- if (measure == "weight") "weight_kg" else "value_usd"
  if (all(is.na(trade[[m]]))) {
    stop("This trade file has no ", measure, " column. Re-download with units = ", measure, ".")
  }
  m
}
measure_scale <- function(measure) if (measure == "weight") 1 / 1000 else 1  # kg -> t

#' Exports, imports and net balance for chosen countries
#' @param trade standardised trade tibble
#' @param iso3 character vector of focus countries
#' @param yr year to keep (NULL = all years in file)
#' @param measure "value" (USD) or "weight" (tonnes)
trade_balance <- function(trade, iso3, yr = NULL, measure = c("value", "weight")) {
  measure <- match.arg(measure)
  m <- measure_col(trade, measure); k <- measure_scale(measure)
  if (!is.null(yr) && any(!is.na(trade$year))) trade <- filter(trade, year == yr)
  exports <- trade %>% filter(exporter_iso3 %in% iso3) %>%
    group_by(iso3 = exporter_iso3) %>% summarise(exports = sum(.data[[m]], na.rm = TRUE) * k)
  imports <- trade %>% filter(importer_iso3 %in% iso3) %>%
    group_by(iso3 = importer_iso3) %>% summarise(imports = sum(.data[[m]], na.rm = TRUE) * k)
  tibble(iso3 = iso3) %>%
    left_join(exports, by = "iso3") %>%
    left_join(imports, by = "iso3") %>%
    mutate(across(c(exports, imports), ~ coalesce(.x, 0)),
           net = exports - imports)
}

#' Largest bilateral flows involving the focus countries
#' @param direction "exports" or "imports"
#' @param measure "value" (USD) or "weight" (tonnes)
top_flows <- function(trade, iso3, n = 10, direction = c("exports", "imports"),
                      yr = NULL, measure = c("value", "weight")) {
  direction <- match.arg(direction); measure <- match.arg(measure)
  m <- measure_col(trade, measure); k <- measure_scale(measure)
  if (!is.null(yr) && any(!is.na(trade$year))) trade <- filter(trade, year == yr)
  trade$amount <- trade[[m]] * k
  flows <- if (direction == "exports") {
    filter(trade, exporter_iso3 %in% iso3) %>%
      transmute(focus_iso3 = exporter_iso3, focus = exporter, partner = importer, amount)
  } else {
    filter(trade, importer_iso3 %in% iso3) %>%
      transmute(focus_iso3 = importer_iso3, focus = importer, partner = exporter, amount)
  }
  flows %>%
    group_by(focus_iso3, partner) %>%
    summarise(focus = first(focus), amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
    group_by(focus_iso3) %>%
    slice_max(amount, n = n, with_ties = FALSE) %>%
    ungroup()
}

#' Electronics / e-waste trade in TONNES per country
#'
#' Turns a weight-units export from
#'   resourcetransitions.earth/embeds/trade?category=29&units=weight
#' into one row per country with tonnes exported and imported,
#' then adds the e-waste generated (WaW3) and population
#' (derive_population()) so trade can be read per person and
#' against the waste each country produces.
#'
#' @param trade standardised trade tibble from load_trade()
#' @param countries build_country_table() output
#' @param yr year to keep (NULL = all years in the file)
#' @return tibble: iso3, country, exported_t, imported_t, net_import_t,
#'   ewaste_tonnes, population, net_import_kg_per_person,
#'   exported_pct_of_generated
extract_waste <- function(trade, countries, yr = NULL) {
  if (all(is.na(trade$weight_kg))) {
    stop("No weight column in this trade file. Re-download it with units = weight.")
  }
  if (!is.null(yr) && any(!is.na(trade$year))) trade <- filter(trade, year == yr)

  exported <- trade %>%
    filter(!is.na(exporter_iso3)) %>%
    group_by(iso3 = exporter_iso3) %>%
    summarise(exported_t = sum(weight_kg, na.rm = TRUE) / 1000)
  imported <- trade %>%
    filter(!is.na(importer_iso3)) %>%
    group_by(iso3 = importer_iso3) %>%
    summarise(imported_t = sum(weight_kg, na.rm = TRUE) / 1000)

  full_join(exported, imported, by = "iso3") %>%
    mutate(across(c(exported_t, imported_t), ~ coalesce(.x, 0)),
           net_import_t = imported_t - exported_t) %>%
    left_join(select(countries, iso3, country, ewaste_tonnes, ewaste_year, population),
              by = "iso3") %>%
    mutate(
      country = coalesce(country, iso3),
      net_import_kg_per_person = net_import_t * 1000 / population,
      exported_pct_of_generated = exported_t / ewaste_tonnes
    ) %>%
    arrange(desc(exported_t + imported_t))
}

#' URL of the Chatham House trade map embed, so the app's
#' iframe follows the year and units the user picks.
#' Category 29 is the one used in this study (electronics).
trade_embed_url <- function(year = 2024, units = c("weight", "value"), category = 29) {
  units <- match.arg(units)
  sprintf("https://resourcetransitions.earth/embeds/trade?year=%s&category=%s&units=%s&autozoom=1",
          year, category, units)
}
