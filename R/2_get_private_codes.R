library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

url <- "https://apps6.dpi.wi.gov/reportcards/CityRead?filterValue=&year=2025"

resp <- GET(
  url,
  user_agent("Mozilla/5.0 (compatible; R-script/1.0; +https://example.com)")
)
stop_for_status(resp)

# the response body is text that LOOKS like HTML with <pre> ... >
# but we don't need to parse HTML — just strip the tags

txt <- content(resp, as = "text", encoding = "UTF-8")

# if the server literally wraps JSON like:
# <pre>[{...}, {...}]</pre>
# we can remove the <pre> tags:

txt_clean <- sub("^\\s*<pre[^>]*>", "", txt) # remove opening <pre ...>
txt_clean <- sub("</pre>\\s*$", "", txt_clean) # remove closing </pre>

# now txt_clean should be valid JSON
dat <- fromJSON(txt_clean, flatten = TRUE)

# often this endpoint returns an array of district rows, so:
df <- as_tibble(dat)
df

saveRDS(df, "data/private_cities.rda")


# Get school codes

url <- "https://apps6.dpi.wi.gov/reportcards/PrivateSchoolRead?filterValue=&year=2025&city="

resp <- GET(
  url,
  user_agent("Mozilla/5.0 (compatible; R-script/1.0; +https://example.com)")
)
stop_for_status(resp)

# the response body is text that LOOKS like HTML with <pre> ... >
# but we don't need to parse HTML — just strip the tags

txt <- content(resp, as = "text", encoding = "UTF-8")

# if the server literally wraps JSON like:
# <pre>[{...}, {...}]</pre>
# we can remove the <pre> tags:

txt_clean <- sub("^\\s*<pre[^>]*>", "", txt) # remove opening <pre ...>
txt_clean <- sub("</pre>\\s*$", "", txt_clean) # remove closing </pre>

# now txt_clean should be valid JSON
dat <- fromJSON(txt_clean, flatten = TRUE)

# often this endpoint returns an array of district rows, so:
df <- as_tibble(dat)
school_id <- df |>
  select(
    year = Year,
    city = City,
    public_school_key = PrivateSchoolKey,
    school_name = SchoolName
  ) |>
  mutate(
    school_name = str_replace_all(
      school_name,
      "/",
      "-"
    )
  )

saveRDS(school_id, "data/private_id.rda")
