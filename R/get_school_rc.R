library(httr)
library(rvest)
library(tidyverse)
library(glue)

districts <- read_rds("data/districts.rda")
school_year <- "2023-24"
year <- glue("20{str_sub(school_year, -2, -1)}")
school_id <- read_rds("data/school_id.rda")

# will need to loop for every district
# !!!need to debug, not downloading everything for some reason
# update: script is only pulling the first school from each school type section

get_urls_and_downloads <- function(
  inds = 1:nrow(districts),
  download_reports = TRUE
) {
  map_df(inds, function(i) {
    this <- districts[i, ]
    cat(crayon::cyan(glue("{this$DistrictName}"), "\n"))

    url <- glue(
      "https://apps6.dpi.wi.gov/reportcards?SelectedSchoolYear={year}&SelectedDistrict=",
      "{this$DistrictAgencyKey}&SelectedPublicSchool="
    )

    # Get and parse
    resp <- GET(
      url,
      user_agent("Mozilla/5.0 (compatible; R-script/1.0; +https://example.com)")
    )
    stop_for_status(resp)

    page <- read_html(content(resp, as = "text", encoding = "UTF-8"))

    # Select each div.mt-4 as a "block"
    blocks <- page |> html_elements("div.mt-4")

    # Extract desired parts
    these_schools <- map_dfr(blocks, function(div) {
      # HTML content inside <span class="font-agency">
      # gives school name
      span_html <- div |>
        html_elements("span.font-agency") |>
        html_text(trim = TRUE)

      if (length(span_html) == 0) {
        # fallback if no inner children
        span_html <- div |>
          html_element("span.font-agency") |>
          as.character()
      }

      # URL inside <div class="col-4"> <a href="...">
      url_link <- div |>
        html_elements("div.col-4 a") |>
        html_attr("href")

      url_link <- url_link[-which(str_detect(url_link, "zipFile"))]

      safe_filename <- str_replace_all(
        span_html,
        "/",
        "-"
      )

      tibble(
        district = this$DistrictName,
        span_html = safe_filename,
        url = glue::glue("https://apps6.dpi.wi.gov{url_link}")
      ) |>
        filter(!is.na(span_html)) |>
        left_join(school_id, by = c("span_html" = "school_name"))
    })

    if (download_reports) {
      walk(1:nrow(these_schools), function(j) {
        this_url <- these_schools[j, ]
        dist_dir <- glue("report_cards/{this_url$district}")
        if (!dir.exists(dist_dir)) {
          dir.create(dist_dir)
        }
        # Simple download
        download.file(
          url = this_url$url,
          destfile = glue(
            "report_cards/{this_url$district}/{this_url$span_html} School Report Card {school_year}.pdf"
          ),
          mode = "wb" # Use binary mode for non-text files
        )
      })
    }

    return(these_schools)
  })
}

report_urls <- get_urls_and_downloads(download_reports = FALSE)
saveRDS(report_urls, "data/")
