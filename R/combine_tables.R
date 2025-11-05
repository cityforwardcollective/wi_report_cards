library(tidyverse)

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
  )

saveRDS(full_table, "data/full_table.rda")
