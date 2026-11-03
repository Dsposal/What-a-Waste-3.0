# Shiny Waste Data

What a Waste 3.0 hack-day prototype.

## Live Demo

<https://what-a-waste.shinyapps.io/shiny-waste-data/>

## Objectives

- Explore WaW 3.0 data
- Compare waste indicators
- Visualise trends
- Build open waste data tools

## Waste transition: individual materials

The Waste Transition view can show total municipal solid waste or an
individual material from the MSW composition data, including textiles,
plastics, food, and paper. Material records are reported as a percentage of
MSW, not as an independently measured annual or per-person quantity. The
view estimates those quantities by multiplying each country's latest
available composition share by its 2030 MSW generation projections, both for
tonnes per year and kilograms per person per day. The share and its reporting
year are shown in the country hover text. Applying the same share to the
per-person measure is mathematically consistent because it is a proportion of
total MSW; the limitation is whether an older reported share still represents
the projected waste mix.

This is a useful exploratory estimate, not a directly reported material
projection: composition observations vary by country and year, and applying
them to 2030 assumes the waste mix has not changed. The source data contains
126 textile composition observations (0.04%–11.94%, median 3.71%), reported
between 1995 and 2024. It does not include GDP, GNI, HDI, or Textile Exchange
data, so those correlations and external validation require separately
sourced, country- and year-aligned datasets before making substantive
conclusions.

The CSV import also reports 26 parsing issues. This view normalizes the
1995 textile value for Mali, which readr infers as logical `TRUE` rather than
numeric `1`; the other parsing issues have not been audited here and could
affect other indicators.
