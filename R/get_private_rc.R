library(httr)
library(rvest)
library(tidyverse)
library(glue)

districts <- read_rds("data/private_cities.rda")
school_year <- "2023-24"
year <- glue("20{str_sub(school_year, -2, -1)}")
school_id <- read_rds("data/private_id.rda")

# will need to loop for every district
# !!!need to debug, not downloading everything for some reason
# update: script is only pulling the first school from each school type section

get_urls_and_downloads <- function(
  inds = 1:nrow(school_id),
  download_reports = TRUE
) {
  map_df(inds, function(i) {
    this <- school_id[i, ]
    cat(crayon::cyan(glue("{this$school_name}"), "\n"))

    # url <- glue(
    #   "https://apps6.dpi.wi.gov/reportcards?SelectedSchoolYear={year}&SelectedCity=",
    #   "{this$city}&SelectedPrivateSchool={this$public_school_key}"
    # )
    url <- modify_url(
      "https://apps6.dpi.wi.gov/reportcards",
      query = list(
        SelectedSchoolYear = year,
        SelectedCity = this$city,
        SelectedPrivateSchool = this$public_school_key
      )
    )

    # Get and parse
    resp <- GET(
      url,
      user_agent("Mozilla/5.0 (compatible; R-script/1.0; +https://example.com)")
    )
    stop_for_status(resp)

    page <- read_html(content(resp, as = "text", encoding = "UTF-8"))

    # Select each div.mt-4 as a "block"
    blocks <- page |> html_elements("div.col-12.col-lg-10")

    # Extract desired parts
    these_schools <- map_dfr(blocks, function(div) {
      # HTML content inside <span class="font-agency">
      # gives school name
      span_html <- div |>
        html_elements("h5") |>
        html_text(trim = TRUE)

      if (length(span_html) == 0) {
        # fallback if no inner children
        span_html <- div |>
          html_element("h5") |>
          as.character()
      }

      # URL inside <div class="col-4"> <a href="...">
      url_link <- div |>
        html_elements("div.mt-2 a") |>
        html_attr("href")

      rc_type <- div |>
        html_elements("div.mt-2 a") |>
        html_text(trim = TRUE)
      this_rc <- rc_type[-which(str_detect(rc_type, "All School Years"))] |>
        str_remove("School Report Card Detail - ")

      url_link <- url_link[-which(str_detect(url_link, "zipFile"))]

      safe_filename <- str_replace_all(
        span_html,
        "/",
        "-"
      )

      tibble(
        district = this$city,
        span_html = safe_filename,
        rc_type = this_rc,
        url = glue::glue("https://apps6.dpi.wi.gov{url_link}")
      ) |>
        filter(!is.na(span_html)) |>
        mutate(
          span_html = str_remove(span_html, glue("^{school_year}")) |>
            str_trim()
        ) |>
        left_join(
          school_id,
          by = c("span_html" = "school_name", "district" = "city")
        )
    })

    if (download_reports) {
      walk(1:nrow(these_schools), function(j) {
        this_url <- these_schools[j, ]
        dist_dir <- glue("report_cards/{this_url$district} Private")
        if (!dir.exists(dist_dir)) {
          dir.create(dist_dir)
        }
        # Simple download
        download.file(
          url = this_url$url,
          destfile = glue(
            "{dist_dir}/{this_url$span_html} {this_url$rc_type} School Report Card {school_year}.pdf"
          ),
          mode = "wb" # Use binary mode for non-text files
        )
      })
    }

    return(these_schools)
  })
}

report_urls <- get_urls_and_downloads(download_reports = TRUE)

saveRDS(report_urls, "data/private_schools_table.rda")
