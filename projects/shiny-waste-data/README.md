# E-waste explorer (Shiny)

An R Shiny app for a study on **e-waste and the electronics trade**, built on the
structure of [What-a-Waste-3.0 / shiny-waste-data](https://github.com/Dsposal/What-a-Waste-3.0/tree/main/projects/shiny-waste-data).

It contrasts the largest e-waste producers — **China, the USA and India** — with
their share of the world's population, and (once you add the data) with their
electronics trade from the Chatham House
[circular economy trade](https://resourcetransitions.earth/data/circular-economy-trade) explorer.

## The study question

> Do the countries that generate the most e-waste also hold the most people — and
> how does their electronics trade relate to the waste they generate?

## What the app shows

| Tab | Chart | Question it answers |
|---|---|---|
| Overview | Headline cards + grouped bars (share of e-waste vs share of population) | Who produces more than their population share? |
| Per person | kg per person bars + log-log scatter of every country | Is it a big-country effect or a high-consumption effect? |
| Electronics trade | Exports vs imports + top partners per country | Who sends electronics/e-waste where? |
| Data | Filterable table + csv download | The numbers behind every chart |
| Method | Sources and caveats | How figures were derived |

### Early findings from the WaW3 data

- **China** ≈ 12.1 Mt (2022): ~20% of reported e-waste, ~18% of population.
- **USA** ≈ 7.2 Mt (2022): ~12% of e-waste from ~4% of population (~21 kg/person).
- **India** ≈ 1.6 Mt (2021): ~3% of e-waste from ~18% of population (~1.1 kg/person).
- In WaW3, India is **not** the third-largest producer — Bangladesh, Japan, Brazil
  and others report more. Other sources (e.g. the Global E-waste Monitor) rank India
  higher, so check which source your study uses and say so.

## Run it

```r
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "readr",
                   "plotly", "DT", "R6", "scales"))
shiny::runApp("ewaste-shiny")
```

`data/WaW3.csv` is included (copied from the What-a-Waste-3.0 repo).

### Adding the trade data

The **Electronics trade** tab shows the live Chatham House map
(`/embeds/trade?category=29`), following the year and units you pick. The embed is
interactive only: its numbers can't be read into R, so download the csv as well.

1. Open the Chatham House link above, choose category 29, the year and
   **Weight** units (tonnes, used by `extract_waste()`) or **Value** units (USD).
2. Download the csv.
3. Either upload it in the **Electronics trade** tab, or save it as
   `data/trade_electronics.csv` so it loads at start-up.

The loader matches headers flexibly (`Exporter ISO3`, `Value (1000USD)`, etc.) and
prints which columns it used. Values given in thousands are scaled to units.

## Project structure

```
ewaste-shiny/
├── app.R            UI + server (only calls store methods and plot_* functions)
├── R/
│   ├── lookups.R    focus countries, fixed colours, name → ISO3
│   ├── data.R       load + reshape WaW3 (pure functions)
│   ├── trade.R      load + summarise trade csv
│   ├── classes.R    EwasteStore (R6) and ewaste_profile (S3)
│   └── plots.R      plotly chart functions
├── ui/tabs.R        one function per tab
├── data/            WaW3.csv (+ optional trade_electronics.csv)
├── LLM_PROMPT.md    prompt for extending the app with an AI assistant
└── README.md
```

## Classes

**`EwasteStore` (R6)** — one shared object holding all data.

| Method | Returns |
|---|---|
| `$new(waw_path, trade_path)` | loads everything |
| `$focus(iso3)` | rows for chosen countries |
| `$top(n, by)` | top-n countries by any column |
| `$median_per_person()` | median kg/person (countries > 1m people) |
| `$combined_share(iso3)` | joint share of e-waste and population |
| `$set_trade(path)` | attach an uploaded trade csv |
| `$profile(iso3)` | an `ewaste_profile` |

**`ewaste_profile` (S3)** — one country snapshot with `print()` / `format()`:

```r
store <- EwasteStore$new()
store$profile("IND")
#> India (IND), data year 2021
#>   E-waste:      1,601,155 tonnes
#>   Per person:   1.1 kg (median 7.0 kg)
#>   Share:        2.7% of e-waste vs 18.2% of population
```

## Key functions

| File | Function | Purpose |
|---|---|---|
| data.R | `load_waw()` | read csv |
| data.R | `extract_ewaste()` | e-waste tonnes, latest year per country |
| data.R | `derive_population()` | population implied by MSW units; uses 2022, else latest year with both units |
| data.R | `extract_epr()` | e-waste EPR status |
| data.R | `build_country_table()` | join + per-capita + shares + burden ratio |
| trade.R | `load_trade()` | standardise Chatham House csv |
| trade.R | `extract_waste()` | tonnes exported/imported per country, joined to e-waste generated and population |
| trade.R | `trade_embed_url()` | URL for the Chatham House map embed |
| trade.R | `trade_balance()` | exports, imports, net (value or weight) |
| trade.R | `top_flows()` | biggest partners |
| plots.R | `plot_share_contrast()` | e-waste share vs population share |
| plots.R | `plot_per_person()` | kg/person with median line |
| plots.R | `plot_population_scatter()` | all countries, log-log |
| plots.R | `plot_trade_balance()` | exports vs imports |
| plots.R | `plot_top_partners()` | partner bars per country |
| plots.R | `plot_export_vs_generated()` | exported tonnes vs e-waste generated |

## Method notes and caveats

- E-waste: WaW3 indicator `WM_NMSW_COMP`, product `E_W`, tonnes/year. Countries
  report different years; the latest observed year (≤ 2026) is used.
- Population isn't in WaW3; it's implied from MSW generation
  (tonnes ÷ (kg/person/day × 365)), as in the original app.
- Shares use only countries with both e-waste and population data.
- Trade **value** ≠ **volume**. Re-export with weight units for tonnes.
- Under HS 2022, heading 8549 covers electrical and electronic waste and scrap;
  earlier years have no dedicated code, so e-waste trade before 2022 is estimated.

## Design choices

- One axis per chart; no dual axes.
- China, USA and India keep fixed, colour-blind-safe colours; other countries are grey.
- Two-measure charts use two neutrals + legend + direct labels.
- Every mark has a hover tooltip; the Data tab is the table view.
