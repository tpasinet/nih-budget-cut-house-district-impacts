# 0 Load Libraries ----
library(tidyverse)
library(rvest)
library(DT)
library(htmltools)
library(htmlwidgets)

# 1 Get US House 2024 Election Results ----
us_house_election_results_2024 <- "https://en.wikipedia.org/wiki/2024_United_States_House_of_Representatives_elections" |>
  read_html() |>
  html_elements("table") |>
  html_table() |>
  # This is a brutish hack in the interest of time. The state result tables were identified manually.
  (\(.) .[18:67])() |>
  map( ~ .x |> select(1, last_col()) |> rename_with(.cols = 2, ~ "Candidates")) |>
  bind_rows() |>
  filter(District != "Location") |>
  mutate(
    District = str_replace(District, "at-large", "0"),
    state = str_extract(District, "^[A-z ]+"),
    district = str_extract(District, "\\d+$") |> str_pad(2, side = "left", pad = "0"),
    winner = str_extract(Candidates, "(?<=Y ).+(?=%)")
  ) |>
  separate_wider_delim(winner,
                       regex(" \\(|\\) "),
                       names = c("representative", "party", "vote_pct")) |>
  select (-c("District", "Candidates")) |>
  mutate(across(where(is.character), str_trim)) |>
  distinct() |>
  mutate(vote_pct = as.numeric(vote_pct) / 100)

# 2 Get House Representative Phone Numbers ----
house_rep_phone <- "https://www.house.gov/representatives" |>
  read_html() |>
  html_elements("table") |>
  html_table() |>
  # This is a brutish hack in the interest of time. The alphabetical rep list tables were identified manually.
  (\(.) .[57:81])() |>
  bind_rows() |>
  mutate(
    District = str_replace(District, "At Large", "0"),
    state = str_extract(District, "^[A-z ]+") |> str_trim(),
    district = str_extract(District, "\\d+") |> str_pad(2, side = "left", pad = "0")
  ) |>
  select(state, district, phone = Phone) |>
  distinct()

# 3 Get NIH FY26 Budget Cut Impacts ----
nih_fy26_budget_cut_impacts <- "https://osf.io/download/2cgyh/" |>
  read_csv() |>
  mutate(
    district = str_extract(GEOID, "\\d{2}$"),
    district_code = paste0(state_code, "-", district)
  ) |>
  select(state,
         district,
         district_code,
         budg_NIH_cuts_econ_loss,
         budg_NIH_cuts_job_loss)

# 4 Combine Data ----
combined_data <- us_house_election_results_2024 |>
  left_join(house_rep_phone, join_by(state, district)) |>
  left_join(nih_fy26_budget_cut_impacts, join_by(state, district)) |>
  select(
    state,
    district,
    district_code,
    representative,
    phone,
    party,
    vote_pct,
    budg_NIH_cuts_econ_loss,
    budg_NIH_cuts_job_loss
  )

# 5 Create Table of Vote Opportunities ----
combined_data |>
  filter(party == "Republican"
         & vote_pct < 0.52) |>
  select(
    state,
    district,
    representative,
    phone,
    vote_pct,
    budg_NIH_cuts_econ_loss,
    budg_NIH_cuts_job_loss
  ) |>
  datatable(
    caption = tags$caption(
      style = "caption-side: top; text-align: left; color: black; font-size: 150%;",
      "Republican House Members Who Won Close 2024 Races Sorted by FY26 NIH Budget Cut Impact"
    ),
    colnames = c(
      "State",
      "District",
      "Representative",
      "Phone",
      "Vote %",
      "Economic Loss",
      "Job Loss"
    ),
    rownames = FALSE,
    options = list(
      dom = "t",
      autoWidth = TRUE,
      pageLength = 100,
      order = list(list(5, "desc"))
    )
  ) |>
  formatPercentage("vote_pct", digits = 1) |>
  formatCurrency("budg_NIH_cuts_econ_loss", digits = 0) |>
  formatRound("budg_NIH_cuts_job_loss", digits = 0) |>
  saveWidget("output/tables/nih_cut_district_impact_opportunities.html",
             selfcontained = TRUE)

# 6 Create Interactive Table of District Impacts ----
combined_data |>
  datatable(
    caption = tags$caption(style = "caption-side: top; text-align: left; color: black; font-size: 150%;", "FY26 NIH Budget Cut Impacts by District"),
    colnames = c(
      "State",
      "District",
      "District Code",
      "Representative",
      "Phone",
      "Party",
      "Vote %",
      "Economic Loss",
      "Job Loss"
    ),
    rownames = FALSE,
    filter = "top",
    extensions = "Buttons",
    options = list(
      pageLength = 100,
      order = list(list(7, "desc")),
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel")
    )
  ) |>
  formatPercentage('vote_pct', digits = 1) |>
  formatCurrency("budg_NIH_cuts_econ_loss", digits = 0) |>
  formatRound("budg_NIH_cuts_job_loss", digits = 0) |>
  saveWidget("output/tables/nih_cut_district_impact_interactive.html",
             selfcontained = TRUE)
