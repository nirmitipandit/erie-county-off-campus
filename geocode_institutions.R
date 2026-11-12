# ----------------------------------------------------------
# 1. Packages and project root
# ----------------------------------------------------------

library(dplyr)
library(here)
library(tidygeocoder)

here::i_am("geocode_institutions.r")


# ----------------------------------------------------------
# 2. Read the institution-address output
# ----------------------------------------------------------

input_path <- here(
  "data",
  "processed",
  "eriecounty_institution_addresses.csv"
)

if (!file.exists(input_path)) {
  stop(
    "Institution-address CSV not found. ",
    "Run extract_institutions.r first."
  )
}

institutions <- read.csv(
  input_path,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

required_columns <- c(
  "institution",
  "address",
  "address_source",
  "address_source_url"
)

missing_columns <- setdiff(
  required_columns,
  names(institutions)
)

if (length(missing_columns) > 0L) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

institutions <- institutions |>
  mutate(
    address = trimws(address),
    address = na_if(address, ""),
    address = na_if(address, "Not available")
  )


# ----------------------------------------------------------
# 3. Geocode unique addresses using ArcGIS
# ----------------------------------------------------------

addresses_to_geocode <- institutions |>
  filter(!is.na(address)) |>
  distinct(address)

if (nrow(addresses_to_geocode) == 0L) {
  stop("No usable addresses were found.")
}

geocoded_addresses <- addresses_to_geocode |>
  tidygeocoder::geocode(
    address = address,
    method = "arcgis",
    lat = latitude,
    long = longitude
  ) |>
  select(
    address,
    latitude,
    longitude
  )


# ----------------------------------------------------------
# 4. Join ArcGIS coordinates to all institution records
# ----------------------------------------------------------

institutions_geocoded <- institutions |>
  left_join(
    geocoded_addresses,
    by = "address"
  ) |>
  mutate(
    geocode_status = case_when(
      is.na(address) ~
        "No address recorded",

      is.finite(latitude) &
        is.finite(longitude) &
        between(latitude, -90, 90) &
        between(longitude, -180, 180) ~
        "ArcGIS coordinates returned; review required",

      TRUE ~
        "No usable coordinates returned"
    ),

    geocoder = if_else(
      !is.na(address),
      "ArcGIS",
      NA_character_
    ),

    geocoded_date_utc = if_else(
      !is.na(address),
      format(
        Sys.time(),
        format = "%Y-%m-%d",
        tz = "UTC"
      ),
      NA_character_
    )
  )


# ----------------------------------------------------------
# 5. Save the results
# ----------------------------------------------------------

output_path <- here(
  "data",
  "processed",
  "eriecounty_institutions_geocoded.csv"
)

write.csv(
  institutions_geocoded,
  file = output_path,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ----------------------------------------------------------
# 6. Print a review summary
# ----------------------------------------------------------

print(
  institutions_geocoded |>
    count(geocode_status)
)

print(
  institutions_geocoded |>
    select(
      institution,
      address,
      latitude,
      longitude,
      geocode_status
    )
)

message("Saved geocoded institution data to: ", output_path)
message("Review returned locations before using them in analysis.")

