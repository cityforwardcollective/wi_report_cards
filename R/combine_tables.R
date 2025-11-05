library(tidyverse)
library(wisconsink12)
library(glue)

public <- read_rds("data/public_schools_table.rda")
private <- read_rds("data/private_schools_table.rda")

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
  full_table,
  schools |>
    transmute(
      school_year,
      dpi_true_id,
      school_key = str_pad(agency_key, width = 6, pad = "0", side = "left"),
      accurate_agency_type,
      broad_agency_type
    )
)

saveRDS(with_codes, "data/full_table.rda")

with_codes |>
  filter(is.na(dpi_true_id))
