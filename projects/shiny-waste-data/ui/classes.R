############################################################
# E-WASTE EXPLORER
# R/classes.R — object model
#
# TWO CLASSES
# -----------
# 1. EwasteStore (R6, reference class)
#    One shared object holding every dataset the app uses.
#    The server asks it questions (focus(), top(), ...)
#    instead of filtering data frames inline. Trade data can
#    be attached later (upload) with set_trade().
#
# 2. ewaste_profile (S3, value class)
#    A lightweight snapshot of ONE country, with print() and
#    format() methods. Used for the headline cards and is
#    handy in the console:  store$profile("IND")
#
# WHY BOTH?
# ---------
# R6 suits mutable app state (the store changes when a user
# uploads trade data). S3 suits small immutable records that
# should print nicely. This is a common R pattern.
############################################################

library(R6)

EwasteStore <- R6Class("EwasteStore",
  public = list(
    waw = NULL,        # raw WaW3 tibble
    countries = NULL,  # build_country_table() output
    trade = NULL,      # standardised trade tibble or NULL

    #' @param waw_path path to WaW3.csv
    #' @param trade_path optional Chatham House csv
    initialize = function(waw_path = "data/WaW3.csv", trade_path = NULL) {
      self$waw <- load_waw(waw_path)
      register_iso_names(self$waw)
      self$countries <- build_country_table(self$waw)
      if (!is.null(trade_path) && file.exists(trade_path)) self$set_trade(trade_path)
      invisible(self)
    },

    #' Attach or replace trade data
    set_trade = function(path) {
      self$trade <- load_trade(path)
      invisible(self)
    },

    has_trade = function() !is.null(self$trade) && nrow(self$trade) > 0,

    #' Rows for chosen countries, in the order given
    focus = function(iso3 = FOCUS_DEFAULT) {
      self$countries %>%
        filter(iso3 %in% !!iso3) %>%
        arrange(match(iso3, !!iso3))
    },

    #' Top n countries by any numeric column
    top = function(n = 10, by = "ewaste_tonnes", min_population = 1e6) {
      self$countries %>%
        filter(is.na(population) | population >= min_population) %>%
        filter(!is.na(.data[[by]])) %>%
        slice_max(.data[[by]], n = n)
    },

    #' Median kg/person (countries > 1m people, avoids
    #' small islands dominating)
    median_per_person = function(min_population = 1e6) {
      self$countries %>%
        filter(population >= min_population) %>%
        pull(kg_per_person) %>%
        median(na.rm = TRUE)
    },

    #' Combined share of world e-waste & population for a set
    combined_share = function(iso3 = FOCUS_DEFAULT) {
      self$focus(iso3) %>%
        summarise(share_ewaste = sum(share_ewaste, na.rm = TRUE),
                  share_population = sum(share_population, na.rm = TRUE))
    },

    years_available = function() {
      if (!self$has_trade()) return(integer())
      sort(unique(na.omit(self$trade$year)), decreasing = TRUE)
    },

    #' S3 profile for one country
    profile = function(iso3) {
      ewaste_profile(self$focus(iso3), self$median_per_person())
    },

    print = function(...) {
      cat("<EwasteStore>\n",
          " countries with e-waste data:", nrow(self$countries), "\n",
          " trade rows:", if (self$has_trade()) nrow(self$trade) else "none", "\n")
      invisible(self)
    }
  )
)

#' Constructor for the S3 country profile
#' @param row one-row tibble from build_country_table()
#' @param world_median median kg/person for context
ewaste_profile <- function(row, world_median = NA_real_) {
  stopifnot(nrow(row) == 1)
  structure(
    list(
      iso3 = row$iso3, country = row$country, year = row$ewaste_year,
      tonnes = row$ewaste_tonnes, population = row$population,
      kg_per_person = row$kg_per_person,
      share_ewaste = row$share_ewaste, share_population = row$share_population,
      burden_ratio = row$burden_ratio, epr = row$ewaste_epr,
      world_median = world_median
    ),
    class = "ewaste_profile"
  )
}

format.ewaste_profile <- function(x, ...) {
  pct <- function(v) if (is.na(v)) "n/a" else sprintf("%.1f%%", 100 * v)
  c(
    sprintf("%s (%s), data year %s", x$country, x$iso3, x$year),
    sprintf("  E-waste:      %s tonnes", format(round(x$tonnes), big.mark = ",")),
    sprintf("  Population:   %s m", if (is.na(x$population)) "n/a" else round(x$population / 1e6)),
    sprintf("  Per person:   %.1f kg (median %.1f kg)", x$kg_per_person, x$world_median),
    sprintf("  Share:        %s of e-waste vs %s of population", pct(x$share_ewaste), pct(x$share_population)),
    sprintf("  E-waste EPR:  %s", ifelse(is.na(x$epr), "unknown", x$epr))
  )
}

print.ewaste_profile <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
