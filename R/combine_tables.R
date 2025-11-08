library(tidyverse)
library(wisconsink12)
library(glue)

public <- read_rds("data/public_schools_table.rda")
private <- read_rds("data/private_schools_table.rda")
base <- "https://cityforwardcollective.github.io/wi_report_cards/report_cards"


full_table <- private |>
  select(
    year,
    district,
    school_name = span_html,
    report_card_type = rc_type,
    school_key = public_school_key,
    download_url = url
  ) |>
  bind_rows(
    public |>
      select(
        year,
        district,
        school_name = span_html,
        school_key = public_school_key,
        district_key = district_agency_key,
        download_url
      )
  ) |>
  mutate(school_year = glue("{year-1}-{year-2000}")) |>
  select(school_year, everything()) |>
  select(-year)


with_codes <- left_join(
  full_table |>
    select(-c(download_url, school_name)),
  schools |>
    transmute(
      school_year,
      dpi_true_id,
      school_name,
      school_key = str_pad(agency_key, width = 6, pad = "0", side = "left"),
      accurate_agency_type,
      broad_agency_type
    )
) |>
  unique() |>
  mutate(
    link = case_when(
      accurate_agency_type == "Private" ~ glue(
        "{base}/{district}/{school_name} ",
        "{report_card_type} School Report Card {school_year}.pdf"
      ),
      TRUE ~ glue(
        "{base}/{district}/{school_name} ",
        "School Report Card {school_year}.pdf"
      )
    ),
    report_card_type = ifelse(
      accurate_agency_type != "Private",
      "All Students",
      report_card_type
    )
  ) |>
  filter(!is.na(report_card_type)) |>
  # Add school_key to the id_cols to make each row unique
  pivot_wider(
    id_cols = c(
      school_year,
      district,
      school_name,
      school_key,
      district_key,
      dpi_true_id,
      accurate_agency_type,
      broad_agency_type
    ),
    names_from = report_card_type,
    values_from = link
  ) |>
  mutate(
    rc_n = case_when(
      is.na(`All Students`) ~ "choice_only",
      is.na(`Choice Students`) ~ "all_only",
      TRUE ~ "both"
    )
  )

saveRDS(with_codes, "data/full_table.rda")

write_csv(with_codes, "data/school_id_table.csv")

with_codes |>
  filter(is.na(dpi_true_id))
