
# ----------------------------------------------------------
# 1. Packages
# ----------------------------------------------------------

# Run once in the RStudio Console:
# install.packages(c(
#   "rvest", "here", "dplyr", "tibble"
# ))

library(rvest)
library(here)
library(dplyr)
library(tibble)

# Identify the project root using this script's location.
here::i_am("extract_institutions.r")


# ----------------------------------------------------------
# 2. Helper functions
# ----------------------------------------------------------

escape_cell <- function(x) {
  x <- as.character(x)

  x[is.na(x)] <- "Not available"

  x <- gsub(
    "[\r\n]+",
    " ",
    x
  )

  gsub(
    "|",
    "\\|",
    x,
    fixed = TRUE
  )
}

make_markdown_table <- function(headers, columns) {
  if (length(headers) != length(columns)) {
    stop(
      "The number of table headers does not match ",
      "the number of columns."
    )
  }

  row_count <- length(columns[[1]])

  if (!all(lengths(columns) == row_count)) {
    stop("Markdown table columns have different lengths.")
  }

  header_line <- paste0(
    "| ",
    paste(headers, collapse = " | "),
    " |"
  )

  separator_line <- paste0(
    "| ",
    paste(
      rep(":---", length(headers)),
      collapse = " | "
    ),
    " |"
  )

  body_lines <- vapply(
    seq_len(row_count),
    function(i) {
      cells <- vapply(
        columns,
        function(column) {
          escape_cell(column[i])
        },
        character(1)
      )

      paste0(
        "| ",
        paste(cells, collapse = " | "),
        " |"
      )
    },
    character(1)
  )

  c(
    header_line,
    separator_line,
    body_lines
  )
}


# ----------------------------------------------------------
# 3. Download the NYSED institution list
# ----------------------------------------------------------

source_url <- "https://data.nysed.gov/profile.php?county=14"

message("Downloading the NYSED Erie County profile...")

page <- read_html(source_url)

page_text <- page |>
  html_element("body") |>
  html_text2()


# ----------------------------------------------------------
# 4. Extract institutions before the closed section
# ----------------------------------------------------------

lines <- strsplit(
  page_text,
  "\n",
  fixed = TRUE
)[[1]]

lines <- trimws(lines)
lines <- lines[nzchar(lines)]

normalized <- toupper(
  gsub(
    "[[:space:]]+",
    " ",
    lines
  )
)

start_heading <- "HIGHER EDUCATION INSTITUTIONS IN ERIE COUNTY"
stop_heading <- "CLOSED HIGHER EDUCATION INSTITUTIONS"

start_index <- which(
  normalized == start_heading
)

if (length(start_index) != 1L) {
  stop(
    "Institution heading was not found uniquely. ",
    "Review the NYSED page before updating the data. ",
    "Existing output files were not changed."
  )
}

stop_candidates <- which(
  normalized == stop_heading &
    seq_along(normalized) > start_index
)

if (length(stop_candidates) == 0L) {
  stop(
    "Closed institutions heading was not found. ",
    "Extraction stopped to avoid including closed institutions. ",
    "Existing output files were not changed."
  )
}

stop_index <- stop_candidates[1]

if (stop_index <= start_index + 1L) {
  stop(
    "No institution records were found between the headings. ",
    "Existing output files were not changed."
  )
}

institution_names <- lines[
  seq.int(
    start_index + 1L,
    stop_index - 1L
  )
]

institution_names <- unique(institution_names)

institution_names <- institution_names[
  nzchar(institution_names)
]

if (length(institution_names) == 0L) {
  stop(
    "No institutions were extracted. ",
    "Existing output files were not changed."
  )
}

retrieved_date <- format(
  Sys.time(),
  format = "%Y-%m-%d",
  tz = "UTC"
)

results <- tibble(
  institution = institution_names,
  county = "Erie",
  state = "New York",
  source_url = source_url,
  retrieved_date_utc = retrieved_date
) |>
  arrange(tolower(institution))


# ----------------------------------------------------------
# 5. Supplemental addresses and source websites
#
# These values are maintained in this script.
# The source websites are not scraped for addresses.
# ----------------------------------------------------------

supplemental_addresses <- tribble(
  ~institution,
  ~address,
  ~address_source_url,

  "BRYANT & STRATTON COLLEGE - AMHERST",
  "3650 Millersport Highway, Getzville, NY 14068",
  "https://www.bryantstratton.edu/location/buffalo-ny/amherst/",

  "BRYANT & STRATTON COLLEGE - BUFFALO",
  "110 Broadway, Buffalo, NY 14203",
  "https://www.bryantstratton.edu/location/buffalo-ny/buffalo/",

  "BRYANT & STRATTON COLLEGE - SOUTHTOW",
  "180 Redtail Road, Orchard Park, NY 14127",
  "https://www.bryantstratton.edu/location/buffalo-ny/orchard-park/",

  "BUFFALO STATE UNIVERSITY",
  "1300 Elmwood Avenue, Buffalo, NY 14222",
  "https://suny.buffalostate.edu/",

  "CANISIUS UNIVERSITY OF BUFFALO, NY",
  "2001 Main Street, Buffalo, NY 14208-1098",
  "https://www.canisius.edu/quick-links/contact-us",

  "D'YOUVILLE UNIVERSITY",
  "320 Porter Avenue, Buffalo, NY 14201",
  "https://www.dyu.edu/",

  "DAEMEN UNIVERSITY",
  "4380 Main Street, Amherst, NY 14226",
  "https://www.daemen.edu/",

  "ERIE COMM COLLEGE-CITY CAMPUS",
  "121 Ellicott Street, Buffalo, NY 14203",
  "https://www.ecc.edu/",

  "HILBERT COLLEGE",
  "5200 South Park Avenue, Hamburg, NY 14075",
  "https://www.hilbert.edu/",

  "SISTERS CHARITY HOSP SCH OF NURSING",
  "2157 Main Street, Buffalo, NY 14214",
  "https://www.chsbuffalo.org/sisters-of-charity-hospital/",

  "SUNY BUFFALO",
  "190 Founders Promenade, Getzville, NY 14068",
  "https://www.buffalo.edu/",

  "TROCAIRE COLLEGE",
  "360 Choate Avenue, Buffalo, NY 14220",
  "https://trocaire.edu/",

  "VILLA MARIA COLLEGE OF BUFFALO",
  "240 Pine Ridge Road, Buffalo, NY 14225",
  "https://www.villa.edu/"
)

author_verified_institutions <- c(
  "SISTERS CHARITY HOSP SCH OF NURSING",
  "SUNY BUFFALO"
)


# ----------------------------------------------------------
# 6. Validate supplemental records
# ----------------------------------------------------------

if (anyDuplicated(supplemental_addresses$institution)) {
  stop(
    "Each institution must appear only once ",
    "in supplemental_addresses. ",
    "Existing output files were not changed."
  )
}

unknown_institutions <- setdiff(
  supplemental_addresses$institution,
  results$institution
)

if (length(unknown_institutions) > 0L) {
  stop(
    "These supplemental names do not match ",
    "the current NYSED list: ",
    paste(
      unknown_institutions,
      collapse = "; "
    ),
    ". Review the names before continuing. ",
    "Existing output files were not changed."
  )
}


# ----------------------------------------------------------
# 7. Join supplemental addresses to NYSED institutions
# ----------------------------------------------------------

institution_addresses_final <- results |>
  left_join(
    supplemental_addresses,
    by = "institution"
  ) |>
  mutate(
    address_source = case_when(
      is.na(address) ~
        "Not available",

      institution %in% author_verified_institutions ~
        "Project-author verified",

      TRUE ~
        "Official institution website"
    ),

    address_method = case_when(
      is.na(address) ~
        "No supplemental address recorded",

      institution %in% author_verified_institutions ~
        paste(
          "Address independently verified",
          "and selected by project author"
        ),

      TRUE ~
        "Supplemental address recorded from official website"
    ),

    osm_status = "Not queried",

    source_link = if_else(
      !is.na(address_source_url),
      paste0(
        "[",
        address_source,
        "](",
        address_source_url,
        ")"
      ),
      address_source
    )
  ) |>
  arrange(tolower(institution))


# ----------------------------------------------------------
# 8. Prepare the About Markdown table
# ----------------------------------------------------------

about_markdown <- c(
  make_markdown_table(
    headers = c(
      "Institution",
      "County"
    ),
    columns = list(
      results$institution,
      results$county
    )
  ),

  "",

  paste0(
    "Source: [NYSED Erie County profile](",
    source_url,
    ")."
  ),

  "",

  paste0(
    "NYSED list retrieved: ",
    retrieved_date,
    " (UTC)."
  ),

  "",

  paste(
    "Institutions listed in NYSED's separate",
    "closed institutions section are excluded."
  ),

  ""
)


# ----------------------------------------------------------
# 9. Prepare the Home Markdown table
# ----------------------------------------------------------

home_markdown <- c(
  make_markdown_table(
    headers = c(
      "Institution",
      "Address",
      "Source"
    ),
    columns = list(
      institution_addresses_final$institution,
      institution_addresses_final$address,
      institution_addresses_final$source_link
    )
  ),

  "",

  paste0(
    "Institution list: [NYSED Erie County profile](",
    source_url,
    ")."
  ),

  "",

  paste0(
    "NYSED list retrieved and table generated: ",
    retrieved_date,
    " (UTC)."
  ),

  "",

  paste(
    "Addresses are supplemental records maintained",
    "in extract_institutions.r.",
    "Source links identify the associated official websites.",
    "These websites are not automatically scraped",
    "for updated addresses during this workflow."
  ),

  "",

  paste(
    "The Sisters Charity Hospital School of Nursing entry",
    "uses the Sisters of Charity Hospital address:",
    "2157 Main Street, Buffalo, NY 14214.",
    "The SUNY Buffalo entry uses",
    "190 Founders Promenade, Getzville, NY 14068.",
    "Both locations were independently verified",
    "and selected by the project author."
  ),

  "",

  paste(
    "OpenStreetMap was not queried in this workflow",
    "because the live requests were timing out.",
    "No OSM feature matches are claimed."
  ),

  ""
)


# ----------------------------------------------------------
# 10. Create output directories
# ----------------------------------------------------------

data_directory <- here(
  "data",
  "processed"
)

include_directory <- here(
  "_includes"
)

dir.create(
  data_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  include_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


# ----------------------------------------------------------
# 11. Save CSV outputs
# ----------------------------------------------------------

institutions_csv_path <- here(
  "data",
  "processed",
  "eriecounty_institutions.csv"
)

addresses_csv_path <- here(
  "data",
  "processed",
  "eriecounty_institution_addresses.csv"
)

write.csv(
  results,
  file = institutions_csv_path,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

address_export <- institution_addresses_final |>
  select(-source_link)

write.csv(
  address_export,
  file = addresses_csv_path,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ----------------------------------------------------------
# 12. Save Markdown includes
# ----------------------------------------------------------

about_markdown_path <- here(
  "_includes",
  "_institutions.md"
)

home_markdown_path <- here(
  "_includes",
  "_institution_addresses.md"
)

writeLines(
  enc2utf8(about_markdown),
  con = about_markdown_path,
  useBytes = TRUE
)

writeLines(
  enc2utf8(home_markdown),
  con = home_markdown_path,
  useBytes = TRUE
)


# ----------------------------------------------------------
# 13. Completion summary
# ----------------------------------------------------------

missing_addresses <- institution_addresses_final |>
  filter(is.na(address)) |>
  select(institution)

message(
  "Saved ",
  nrow(results),
  " NYSED institution records."
)

message(
  "Saved ",
  nrow(institution_addresses_final),
  " institution-address records."
)

message(
  "Institutions without a recorded address: ",
  nrow(missing_addresses)
)

if (nrow(missing_addresses) > 0L) {
  message(
    "Missing addresses: ",
    paste(
      missing_addresses$institution,
      collapse = "; "
    )
  )
}

message("OSM was not queried.")
message("Institution CSV: ", institutions_csv_path)
message("Address CSV: ", addresses_csv_path)
message("About table: ", about_markdown_path)
message("Home table: ", home_markdown_path)
message("Ready for review and Quarto rendering.")
