############################################################
# COUNTRY NAME NORMALISATION
#
# PURPOSE
# -------
# Natural Earth and WaW use different country names.
#
# Examples:
# - United States -> United States of America
# - Viet Nam -> Vietnam
# - Russian Federation -> Russia
#
# AI NOTES
# --------
# If map joins fail, check this file first.
#
# Unmatched countries can be added to the lookup
# rather than modifying the source data.
############################################################

# Manual corrections required for country joins.
#
# Kept explicit rather than using fuzzy matching so that
# results remain predictable and reviewable.
country_lookup <- data.frame(
  REF_AREA_LABEL = c(
    "United States",
    "Russian Federation",
    "Viet Nam",
    "Turkiye",
    "Korea, Rep.",
    "Egypt, Arab Rep.",
    "Iran, Islamic Rep.",
    "Lao PDR",
    "Slovak Republic",
    "Venezuela, RB",
    "Congo, Dem. Rep.",
    "Congo, Rep.",
    "Gambia, The",
    "Bahamas, The",
    "Yemen, Rep.",
    "Brunei Darussalam",
    "Cote d'Ivoire",
    "Syrian Arab Republic",
    "Kyrgyz Republic"
  ),
  NE_NAME = c(
    "United States of America",
    "Russia",
    "Vietnam",
    "Turkey",
    "South Korea",
    "Egypt",
    "Iran",
    "Laos",
    "Slovakia",
    "Venezuela",
    "Democratic Republic of the Congo",
    "Republic of the Congo",
    "Gambia",
    "Bahamas",
    "Yemen",
    "Brunei",
    "Ivory Coast",
    "Syria",
    "Kyrgyzstan"
  ),
  stringsAsFactors = FALSE
)

waw_clean <- waw %>%
  left_join(
    country_lookup,
    by = "REF_AREA_LABEL"
  ) %>%
  mutate(
    country_name = ifelse(
      is.na(NE_NAME),
      REF_AREA_LABEL,
      NE_NAME
    )
  )