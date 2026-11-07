#' One time installation
# install.packages(c("rvest", "here"))

#' Libraries required
library(rvest)
library("here")

#' Attempt to extract list of higher education institutions in Erie County
source_url <- "https://data.nysed.gov/profile.php?county=14"

page <- read_html(source_url)
page_text <- html_text2(html_element(page, "body"))

#' Break the page into nonempty lines

lines <- strsplit(page_text, "\n", fixed = TRUE)[[1]]
lines <- trimws(lines)
lines <- lines[nzchar(lines)]

#' Normalize headings for matching

normalized <- toupper(gsub("[[:space:]]+", " ", lines))

start_heading <- "HIGHER EDUCATION INSTITUTIONS IN ERIE COUNTY"
stop_heading <- "CLOSED HIGHER EDUCATION INSTITUTIONS"

start_index <- which(normalized == start_heading)

if (length(start_index) != 1L) {
  stop(
    "Institution heading was not found uniquely.",
    "Review the NYSED page before updating the data.")}

stop_candidates <- which(
  normalized == stop_heading &
    seq_along(normalized) > start_index)

if (length(stop_candidates) == 0L) {
  stop(
    "Closed institutions heading was not found.",
    "Extraction stopped to avoid including closed institutions.",
    "Existing output files were not changed.")}

stop_index <- stop_candidates[1]

if (stop_index <= start_index + 1L) {
  stop(
    "No institution records were found between the headings.",
    "Existing output files were not changed.")}

#' Extract ONLY the lines before the closed institutions heading.
institution_names <- lines[
  seq.int(start_index + 1L, stop_index - 1L)]

institution_names <- unique(institution_names)
institution_names <- institution_names[nzchar(institution_names)]

if (length(institution_names) == 0L) {
  stop("No institutions extracted. Existing outputs were not changed.")}

#' Create the results table

retrieved_date <- format(
  Sys.time(),
  format = "%Y-%m-%d",
  tz = "UTC")

results <- data.frame(
  institution = institution_names,
  county = "Erie",
  state = "New York",
  source_url = source_url,
  retrieved_date_utc = retrieved_date,
  stringsAsFactors = FALSE)

results <- results[
  order(tolower(results$institution)),
  ,
  drop = FALSE]

rownames(results) <- NULL

print(results, row.names = FALSE)

#' Create output folders

data_directory <- here("data", "processed")
include_directory <- here("_includes")

dir.create(
  data_directory,
  recursive = TRUE,
  showWarnings = FALSE)

dir.create(
  include_directory,
  recursive = TRUE,
  showWarnings = FALSE)

#' Save the CSV

csv_path <- here(
  "data",
  "processed",
  "eriecounty_institutions.csv")

write.csv(
  results,
  file = csv_path,
  row.names = FALSE,
  fileEncoding = "UTF-8")

#' Create a static markdown table for About.qmd

#' Escape pipes so names cannot break the markdown table
safe_names <- gsub(
  "|",
  "\\|",
  results$institution,
  fixed = TRUE)

markdown_lines <- c(
  "| Institution | County |",
  "| :--- | :--- |",
  paste0("| ", safe_names, " | Erie |"),
  "",
  paste0(
    "Source: [NYSED Erie County profile](",
    source_url,
  ")."
),
"",
paste0("Retrieved: ", retrieved_date, " (UTC)."),
"",
paste(
  "Institutions listed in NYSED's separate",
  "closed institutions section are excluded."),
"")

markdown_path <- here(
  "_includes",
  "_institutions.md")

writeLines(enc2utf8(markdown_lines),
           con = markdown_path,
           useBytes = TRUE)

#' Report Completion

message(
  "Saved",
  nrow(results),
  "insitution records.")

message("CSV: ", csv_path)
message("Quarto table: ", markdown_path)
message("Closed institutions section excluded.")
message("Review the generated table before committing to Github.")
