# R/00_utils/helpers_policy.R — ABAWD waiver / treatment timing (document-based)
# Used by 01_build/02_clean_laus.R and 03_merge_panel.R

# Canonical 83 Michigan counties
MI_83 <- c(
  "Alcona", "Alger", "Allegan", "Alpena", "Antrim", "Arenac", "Baraga", "Barry", "Bay", "Benzie", "Berrien", "Branch",
  "Calhoun", "Cass", "Charlevoix", "Cheboygan", "Chippewa", "Clare", "Clinton", "Crawford", "Delta", "Dickinson", "Eaton",
  "Emmet", "Genesee", "Gladwin", "Gogebic", "Grand Traverse", "Gratiot", "Hillsdale", "Houghton", "Huron", "Ingham",
  "Ionia", "Iosco", "Iron", "Isabella", "Jackson", "Kalamazoo", "Kalkaska", "Kent", "Keweenaw", "Lake", "Lapeer", "Leelanau",
  "Lenawee", "Livingston", "Luce", "Mackinac", "Macomb", "Manistee", "Marquette", "Mason", "Mecosta", "Menominee", "Midland",
  "Missaukee", "Monroe", "Montcalm", "Montmorency", "Muskegon", "Newaygo", "Oakland", "Oceana", "Ogemaw", "Ontonagon",
  "Osceola", "Oscoda", "Otsego", "Ottawa", "Presque Isle", "Roscommon", "Saginaw", "St. Clair", "St. Joseph", "Sanilac",
  "Schoolcraft", "Shiawassee", "Tuscola", "Van Buren", "Washtenaw", "Wayne", "Wexford"
)

# Document-based waiver coverage (approval areas)
waived_2017 <- c(
  "Alcona", "Alger", "Allegan", "Alpena", "Antrim", "Arenac", "Baraga", "Barry", "Bay", "Benzie", "Berrien", "Branch",
  "Calhoun", "Cass", "Charlevoix", "Cheboygan", "Chippewa", "Clare", "Clinton", "Crawford", "Delta", "Dickinson", "Eaton",
  "Emmet", "Genesee", "Gladwin", "Gogebic", "Grand Traverse", "Gratiot", "Hillsdale", "Houghton", "Huron", "Ingham",
  "Ionia", "Iosco", "Iron", "Isabella", "Jackson", "Kalamazoo", "Kalkaska", "Keweenaw", "Lake", "Lapeer", "Leelanau",
  "Lenawee", "Livingston", "Luce", "Mackinac", "Macomb", "Manistee", "Marquette", "Mason", "Mecosta", "Menominee",
  "Midland", "Missaukee", "Monroe", "Montcalm", "Montmorency", "Muskegon", "Newaygo", "Oceana", "Ogemaw", "Ontonagon",
  "Osceola", "Oscoda", "Otsego", "Presque Isle", "Roscommon", "Saginaw", "Sanilac", "Schoolcraft", "Shiawassee",
  "St. Clair", "St. Joseph", "Tuscola", "Van Buren", "Wayne", "Wexford"
)
waived_2018_2019 <- c(
  "Alcona", "Alger", "Alpena", "Antrim", "Arenac", "Baraga", "Bay", "Benzie", "Branch", "Calhoun", "Cass", "Charlevoix",
  "Cheboygan", "Chippewa", "Clare", "Crawford", "Delta", "Dickinson", "Emmet", "Genesee", "Gladwin", "Gogebic", "Gratiot",
  "Hillsdale", "Houghton", "Huron", "Iosco", "Iron", "Isabella", "Jackson", "Kalkaska", "Keweenaw", "Lake", "Lapeer",
  "Leelanau", "Lenawee", "Luce", "Mackinac", "Macomb", "Manistee", "Marquette", "Mason", "Mecosta", "Menominee", "Midland",
  "Missaukee", "Monroe", "Montcalm", "Montmorency", "Muskegon", "Newaygo", "Oceana", "Ogemaw", "Ontonagon", "Osceola",
  "Oscoda", "Otsego", "Presque Isle", "Roscommon", "Saginaw", "Sanilac", "Schoolcraft", "Shiawassee", "St. Clair",
  "St. Joseph", "Tuscola", "Van Buren", "Wayne", "Wexford"
)
cover_2016_statewide <- MI_83
cover_2017_79        <- waived_2017
cover_2018_2019_69   <- waived_2018_2019
noncovered_2017_4    <- setdiff(cover_2016_statewide, cover_2017_79)
removed_2018_01_10   <- setdiff(cover_2017_79, cover_2018_2019_69)
phasein_2018_07_68   <- setdiff(cover_2018_2019_69, "Wayne")
phasein_2018_10_1    <- "Wayne"

#' Add waiver_covered and time_limit_proxy (enforced) to panel with county, year, month
#' Expects panel to have county (clean name), and year/month or ym (Date).
add_waiver_policy <- function(panel) {
  if (!requireNamespace("lubridate", quietly = TRUE)) stop("Need lubridate")
  if (!"ym" %in% names(panel) && all(c("year", "month") %in% names(panel))) {
    panel <- panel %>% dplyr::mutate(ym = lubridate::make_date(.data$year, .data$month, 1L))
  }
  panel %>%
    dplyr::filter(.data$ym <= as.Date("2019-12-01")) %>%
    dplyr::mutate(
      waiver_covered = dplyr::case_when(
        .data$ym >= as.Date("2016-01-01") & .data$ym <= as.Date("2016-12-01") ~ 1L,
        .data$ym >= as.Date("2017-01-01") & .data$ym <= as.Date("2017-12-01") ~ as.integer(.data$county %in% cover_2017_79),
        .data$ym >= as.Date("2018-01-01") & .data$ym <= as.Date("2019-12-01") ~ as.integer(.data$county %in% cover_2018_2019_69),
        TRUE ~ NA_integer_
      ),
      time_limit_proxy = dplyr::case_when(
        .data$ym < as.Date("2017-01-01") ~ 0L,
        .data$county %in% noncovered_2017_4   & .data$ym >= as.Date("2017-01-01") ~ 1L,
        .data$county %in% removed_2018_01_10 & .data$ym >= as.Date("2018-01-01") ~ 1L,
        .data$county %in% phasein_2018_07_68 & .data$ym >= as.Date("2018-07-01") ~ 1L,
        .data$county %in% phasein_2018_10_1  & .data$ym >= as.Date("2018-10-01") ~ 1L,
        TRUE ~ 0L
      ),
      enforced = .data$time_limit_proxy
    )
}

#' Derive G (cohort first-treatment month) and G_int from panel with time_limit_proxy
#' Panel must have id (county_id or county), ym_date or (year, month), time_limit_proxy
derive_G <- function(panel) {
  if (!"ym_date" %in% names(panel) && all(c("year", "month") %in% names(panel))) {
    panel <- panel %>% dplyr::mutate(ym_date = lubridate::make_date(.data$year, .data$month, 1L))
  }
  id_key <- if ("county_id" %in% names(panel)) "county_id" else "county"
  to_t <- function(y, m) as.integer(y) * 12L + (as.integer(m) - 1L)
  out <- panel %>%
    dplyr::group_by(.data[[id_key]]) %>%
    dplyr::summarise(
      G_date = if (any(.data$time_limit_proxy == 1L, na.rm = TRUE)) {
        min(.data$ym_date[.data$time_limit_proxy == 1L], na.rm = TRUE)
      } else as.Date(NA),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      G_int = ifelse(is.na(.data$G_date), NA_integer_,
                     to_t(lubridate::year(.data$G_date), lubridate::month(.data$G_date))),
      G = ifelse(is.na(.data$G_date), "0", format(.data$G_date, "%Y-%m"))
    ) %>%
    dplyr::select(dplyr::all_of(c(id_key, "G", "G_int")))
  names(out)[names(out) == id_key] <- "id_key"
  out
}
