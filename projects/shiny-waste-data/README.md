# Shiny Waste Data

**A hackathon project built to be explored, questioned, and improved.**

This is an experimental R Shiny dashboard for exploring data from the World
Bank's *What a Waste 3.0* database. Jump in, have a play, follow a question
that interests you, and share what you learn. You do not need to be an R
expert or waste specialist to get started. If you use an AI coding assistant,
you are welcome to ask it to help you understand the project, try an idea, or
make a contribution.

This is a prototype, not an official World Bank product or a source of policy
advice. Some views combine reported observations with projections or
exploratory estimates; their assumptions and limitations are called out below
and in the app.

## Try the app

Explore the [live demo](https://what-a-waste.shinyapps.io/shiny-waste-data/).
The app is also open source in this repository, so you can run it locally and
experiment with the code.

### Run it locally

1. Install [R](https://cran.r-project.org/) and, optionally,
   [RStudio Desktop](https://posit.co/download/rstudio-desktop/).
2. Open `shiny-waste-data.Rproj` in RStudio, or set your R working directory
   to this project folder. The app uses paths relative to this folder.
3. Install the packages used by the app if you do not already have them:

   ```r
   install.packages(c(
     "shiny", "readr", "dplyr", "tidyr", "ggplot2", "DT",
     "leaflet", "sf", "rnaturalearth", "cartogram", "plotly",
     "scales", "viridisLite"
   ))
   ```

4. Start the app from this folder:

   ```r
   shiny::runApp()
   ```

   In RStudio, you can also open `app.R` and select **Run App**.

The `sf` package may need system geospatial libraries on some platforms; use
the installation guidance for your operating system if its installation
reports missing dependencies.

## What's here

The app has eight tabs. Try the controls, compare views, and use the caveats
as prompts for further questions:

| Tab | What to explore |
| --- | --- |
| Home | Project orientation and links for contributors |
| Data Explorer | Country or global MSW generation data by year |
| Map | Country-level MSW generation with selectable year, palette, and scale |
| Waste Transition | Animated comparison of total waste or a selected material, by tonnes and per person |
| Country race | Animated country rankings, in total tonnes or per person |
| EPR and treatment | Recorded extended producer responsibility status and treatment survey indicators |
| Where waste goes | Sankey diagram of treatment and disposal by income group or country |
| Treatment gap 2050 | Scenario showing where additional waste could be unmanaged if current treatment shares persist |

## How to explore and contribute

There is no fixed hackathon brief for this prototype. You could improve a
chart, add an explanation, check an assumption, connect another dataset, make
the app easier to use, or document a question you cannot answer yet. Small,
well-explained experiments are welcome. Before changing the code, look at the
relevant tab and its data helper; the map, charts, and data transformations
are kept in separate files so they are easier to explore.

For AI-assisted work, start with the project's
[AI contribution guidance](llms.txt). Work on a branch and open a pull
request for human review. Keep documentation with your changes: explain what
you tried, how to reproduce it, and any assumptions or limitations. Run
relevant checks and say clearly what you did and did not verify.

### Project layout

```text
app.R                 Shiny app entry point, tab wiring, and server logic
ui/                   One R file for each tab's user interface
R/                    Data preparation and chart/map helpers
data/WaW3.csv         WaW 3.0 dataset used by the app
data/                 Additional circular-economy workbooks for exploration
data_prep.R           Standalone helper to reshape the CSV for exploration
llms.txt              AI contribution guidance for this project only
```

`app.R` explicitly sources the files in `R/` and `ui/`; keep that source order
in mind when adding a helper that depends on an object created in another
file. The additional workbooks and `data_prep.R` are not currently wired into
the Shiny app.

## Interpreting the charts

These views are for exploration. A visual pattern is a starting point for
investigation, not proof of cause and effect.

- **Reported data and projections:** The World Bank dataset includes reported
  observations and projections. In the Country race, values from 2030 to 2050
  are projections; the years between the dataset's 2022, 2030, 2040, and 2050
  points are linearly interpolated for animation only. The Data Explorer's
  dashed line is a simple linear guide fitted to those four points, not a
  separate official forecast.
- **Waste Transition estimates:** Material composition observations are
  percentages of MSW, not independent material tonnage or per-person
  measurements. For a selected material, the app applies each country's
  latest available composition share to its 2030 projected MSW generation.
  The resulting tonnes-per-year and kilograms-per-person-per-day values are
  exploratory estimates, not directly reported material projections. This
  assumes the reported composition still represents the 2030 waste mix; the
  hover text shows the share and its reporting year.
- **Treatment surveys:** Treatment views use each country's latest complete
  treatment survey, which may combine compatible years where a single year
  does not describe the whole waste stream. A missing treatment category is
  not automatically evidence of zero treatment; read the notes in the app.
- **Where waste goes in 2050:** 2050 projected waste generation is combined
  with today's treatment shares. This is a "shares stay the same" scenario,
  not a forecast of future treatment.
- **Treatment gap 2050:** The map applies today's share of waste outside
  controlled facilities to projected waste growth from 2022 to 2050. It is a
  scenario, not a prediction; countries without a complete survey cannot be
  ranked by this method and are shown separately.
- **Country comparisons:** Per-person rankings omit countries with fewer than
  one million people. This reduces the influence of very small territories;
  it is a display choice, not a judgement about those places.

The source CSV currently produces 26 readr parsing issues. The Waste
Transition calculation handles the 1995 Mali textile value, which is read as
logical `TRUE` rather than numeric `1`; the other parsing issues have not
been audited and may affect other indicators. Investigating and documenting
them is a useful contribution.

## Data and scope

The app reads `data/WaW3.csv`. The `data/` folder also contains Circular
Economy Earth workbooks that are available for future experiments but are not
currently loaded into the app. Before using additional data, document its
source, version or date, units, geographic and time coverage, licensing, and
any transformations. Do not silently overwrite source data or imply that an
external dataset is already part of the app.

## Acknowledgements

This prototype explores the World Bank's
[What a Waste Global Database](https://datacatalog.worldbank.org/search/dataset/0039597/what-a-waste-global-database).
See the dataset source for its attribution and licensing terms.
