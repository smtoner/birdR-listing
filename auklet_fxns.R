# rearrange and rename data columns
backhack <- function(file){
  data<- read_csv(paste(file))
  data <- data[,-c(8,23)]
  colnames(data)[20] <- "Species Comments"
  colnames(data) <- colnames(auklet:::eb_data_cols)
  return(data)
  write.csv(data, "MyEBirdData_backhack.csv", row.names = FALSE)
}


# modified mstrimas function--fixes Date/Time format issue, updates taxonomy, and removes header check
eb_sightings2 <- function (file, countable = FALSE) 
{
  stopifnot(is.character(file), length(file) == 1, file.exists(file))
  col_formats <- readr::cols(.default = readr::col_character(), 
                             Latitude = readr::col_double(), Longitude = readr::col_double(),
                             Time = readr::col_time(format = "%I:%M %p"), Date = readr::col_date(format = "%m/%d/%y"), 
                             `Duration (Min)` = readr::col_integer(), `All Obs Reported` = readr::col_logical(), 
                             `Distance Traveled (km)` = readr::col_double(),`Area Covered (ha)` = readr::col_double(), `Number of Observers` = readr::col_integer())
  s <- suppressWarnings(readr::read_csv(file, col_types = col_formats, 
                                        na = ""))
  probs <- readr::problems(s) %>% dplyr::filter(!(is.na(.data$col) & 
                                                    .data$expected > .data$actual))
  if (nrow(probs) > 0) {
    m <- paste0(nrow(probs), " problems parsing eBird sightings file\n", 
                "Use readr::problems(x) to view errors")
    warning(m)
    probs <- NULL
  }
  attr(s, "problems") <- probs
  nm <- s %>% names() %>% tolower() %>% stringr::str_replace_all(" \\([a-z]+\\)", 
                                                                 "") %>% stringr::str_replace_all("[^a-z]", "_")
  names(s) <- nm
  s_p <- stringr::str_split(s$state_province, "-", n = 2, 
                            simplify = TRUE)
  s$state_province <- NULL
  s$country <- s_p[, 1]
  s$state_province <- s_p[, 2]
  s <- s %>% dplyr::select(-.data$common_name, -.data$taxonomic_order) %>% 
    dplyr::select(.data$submission_id, .data$count, .data$country, 
                  .data$state_province, dplyr::everything()) %>% dplyr::rename(name_scientific = .data$scientific_name) %>% 
    dplyr::right_join(eb_taxonomy2, ., by = "name_scientific") %>% 
    dplyr::select(.data$submission_id, dplyr::everything())
  n_miss <- sum(is.na(s$species_code))
  if (n_miss != 0) {
    m <- paste0("Error joining sightings to taxonomy: ", 
                format(n_miss, big.mark = ","), " records not found in taxononmy\n", 
                "Try updating this package and re-downloading your sightings")
    warning(m)
  }
  if (countable) {
    s <- eb_countable(s)
  }
  class(s) <- c("eb_sightings", class(s))
  return(s)
}

# another mstrimas function; I fixed the taxonomy reference
# 
eb_lifelist_day2 <- function(x) {
# determine species list
day_list <- eb_countable(x) %>%
  # remove life list uploads
  dplyr::filter(.data$date != as.Date("1900-01-01")) %>%
  dplyr::mutate(year = lubridate::year(.data$date),
                month = lubridate::month(.data$date),
                day = lubridate::day(.data$date)) %>%
  # species seen on each day
  dplyr::group_by(.data$report_as, .data$month, .data$day) %>%
  dplyr::arrange(.data$report_as, .data$month, .data$day, .data$year) %>%
  dplyr::filter(dplyr::row_number(.data$year) == 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(.data$year, .data$month, .data$day, .data$report_as)

# bring in taxonomuy
day_list <- eb_taxonomy2 %>%
  dplyr::filter(!is.na(.data$report_as), .data$category == "species") %>%
  dplyr::select(.data$report_as, .data$order, .data$family,
                .data$species_common, .data$species_scientific) %>%
  dplyr::left_join(day_list, ., by = "report_as") %>%
  dplyr::rename(species_code = .data$report_as) %>%
  dplyr::arrange(.data$month, .data$day, .data$year, .data$species_code)

class(day_list) <- c("eb_lifelist_day", class(day_list))
return(day_list)
}