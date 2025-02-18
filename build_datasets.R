
# Optionally run this right away to spin up the compute (it can take a while)
sconn::sc()



# Data prep ---------------------------------------------------------------


# LSOA:LAD lookup ---------------------------------------------------------

# Just for completeness / lineage: data now uploaded to databricks.
# https://github.com/francisbarton/boundr
lsoa11_lad18_lookup_ew <- boundr::lookup("lsoa", "lad", within_year = 2018)

# TODO: Scotland and Northern Ireland data area lookups here



# Helper functions --------------------------------------------------------



read_in_jg_data <- function(table_name) {
  dplyr::tbl(sconn::sc(), dbplyr::in_catalog("strategyunit", "csds_jg", table_name))
}
read_in_reference_data <- function(table_name) {
  dplyr::tbl(sconn::sc(), dbplyr::in_catalog("su_data", "reference", table_name))
}



# List of cols to count by, possibly needs refinement!
provider_count_cols <- \() c(
  "provider_org_id",
  "lad18cd",
  "lad18nm",
  "age_int",
  "gender_cat"
  ## Other columns we might want to bring in later:
  # "TeamType",
  # "TeamTypeDescription",
  # "ConsultationMediumSU",
  # "AttendanceOutcomeSU",
)

icb_count_cols <- \() c(
  "icb22cdh",
  "icb22nm",
  "lad18cd",
  "lad18nm",
  "age_int",
  "gender_cat"
)




# Use LSOA data from NHSE to allocate CSDS contacts to 2018 local authorities
# (function currently unused below).
match_2011_lsoas_to_2018_lads <- function(.data) {
  lookup_tbl <- read_in_reference_data("lsoa11_lad18_lookup_eng") |>
    dplyr::select(!"lsoa11nm")
  .data |>
    dplyr::left_join(lookup_tbl, "lsoa11cd")
}


# Summarise CSDS contacts table (function currently unused below)
count_fin_year_care_contacts <- function(.data, fin_year_start) {
  fys <- as.Date(paste0(as.integer(fin_year_start), "-04-01"))
  fye <- as.Date(paste0(as.integer(fin_year_start) + 1L, "-03-31"))

  .data |>
    dplyr::filter(dplyr::if_any("contact_date", \(x) dplyr::between(x, {{ fys }}, {{ fye }}))) |>
    dplyr::mutate(dplyr::across("age_int", \(x) dplyr::if_else(x > 90L, 90L, x))) |>
    dplyr::count(dplyr::pick(tidyselect::all_of(count_cols())), name = "contacts")
}


# Read in data tables -----------------------------------------------------


# CSDS data as prepared by JG
# Contains 191mn rows
# Data for each care contact from 2021-04-01 to 2023-03-31
csds_data_full_valid <- read_in_jg_data("full_contact_based_valid")
lsoa11_lad18_lookup_ew <- read_in_reference_data("lsoa11_lad18_lookup_ew")
# Read in Scotland and Northern Ireland area lookups, and then bind_rows()
# ...
# lad18_lookup_uk <- lad18_lookup_ew etc,

# This is a dummy version for now (it's just all providers!)
consistent_providers <- csds_data_full_valid |>
  dplyr::summarise(dplyr::across("OrgID_Provider", unique)) |>
  dplyr::pull("OrgID_Provider")



# 327,140 rows (by provider and LA)
csds_contacts_2022_23_provider_summary <- csds_data_full_valid |>
  dplyr::filter(dplyr::if_any("Der_Financial_Year", \(x) x == "2022/23")) |>
  dplyr::rename(
    provider_org_id = "OrgID_Provider",
    age_int = "AgeYr_Contact_Date",
    gender_cat = "Gender",
    lsoa11cd = "Der_Postcode_yr2011_LSOA",
    contact_date = "Contact_Date"
  ) |>
  dplyr::left_join(lad18_lookup, "lsoa11cd") |>
  dplyr::mutate(dplyr::across("age_int", \(x) dplyr::if_else(x > 90L, 90L, x))) |>
  dplyr::count(dplyr::pick(tidyselect::all_of(provider_count_cols())), name = "contacts") |>
  dplyr::collect() |>
  dplyr::mutate(dplyr::across(c("age_int", "contacts"), as.integer))

csds_contacts_2022_23_provider_summary |>
  saveRDS("csds_contacts_prov_summ.rds")
csds_contacts_2022_23_provider_summary <- readRDS("csds_contacts_prov_summ.rds")

# 63,744 rows (by ICB and LA)
csds_contacts_2022_23_icb_summary <- csds_data_full_valid |>
  dplyr::filter(
    dplyr::if_any("Der_Financial_Year", \(x) x == "2022/23") &
      dplyr::if_any("OrgID_Provider", \(x) x %in% {{ consistent_providers }})
  ) |>
  dplyr::rename(
    age_int = "AgeYr_Contact_Date",
    gender_cat = "Gender",
    lsoa11cd = "Der_Postcode_yr2011_LSOA",
    contact_date = "Contact_Date"
  ) |>
  dplyr::left_join(lad18_lookup, "lsoa11cd") |>
  dplyr::mutate(dplyr::across("age_int", \(x) dplyr::if_else(x > 90L, 90L, x))) |>
  dplyr::count(dplyr::pick(tidyselect::all_of(icb_count_cols())), name = "contacts") |>
  dplyr::collect() |>
  dplyr::mutate(dplyr::across(c("age_int", "contacts"), as.integer))

csds_contacts_2022_23_icb_summary |>
  saveRDS("csds_contacts_icb_summ.rds")
csds_contacts_2022_23_icb_summary <- readRDS("csds_contacts_icb_summ.rds")



# Population projections data ---------------------------------------------


ppp_locn <- "/Volumes/su_data/nhp/population-projections/demographic_data"

popn_proj_orig <- sconn::sc() |>
  sparklyr::spark_read_parquet("popn_proj_data", ppp_locn)

# 1,305,304 rows
popn_proj_tidy <- popn_proj_orig |>
  filter(dplyr::if_any("year", \(x) x >= 2022L)) |>
  dplyr::select(c(
    cal_yr = "year",
    lad18cd = "area_code",
    age_int = "age",
    gender_cat = "sex",
    "value"
  )) |>
  dplyr::collect() |>
  dplyr::mutate(
    dplyr::across("gender_cat", \(x) dplyr::if_else(x == 1L, "Male", "Female")),
    dplyr::across("gender_cat", as.factor),
    dplyr::across(c("age_int", "cal_yr"), as.integer),
    dplyr::across("value", as.numeric)
  ) |>
  # This arrange() step is crucial for the calculation of growth coefficients
  # in future steps, using lead() and first().
  dplyr::arrange(dplyr::pick("cal_yr"))


saveRDS(popn_proj_tidy, "popn_proj_tidy.rds")
popn_proj_tidy <- readRDS("popn_proj_tidy.rds")



# Calculate growth coefficients per financial year ------------------------


# 1,245,972 rows
popn_fy_projected <- popn_proj_tidy |>
  dplyr::mutate(
    fin_year = paste0(.data[["cal_yr"]], "_", dplyr::lead(.data[["cal_yr"]]) %% 100),
    fin_year_popn = (.data[["value"]] * 0.75) + (dplyr::lead(.data[["value"]]) * 0.25),
    growth_coeff = .data[["fin_year_popn"]] / dplyr::first(.data[["fin_year_popn"]]),
    .by = tidyselect::all_of(c("lad18cd", "age_int", "gender_cat")),
    .keep = "unused"
  ) |>
  dplyr::filter(!dplyr::if_any("fin_year_popn", is.na))



# Calculate projected community contacts ----------------------------------

common_cols <- \() c("lad18cd", "lad18nm", "age_int", "gender_cat")

join_popn_proj_data <- function(x, y = popn_fy_projected) {
  x |>
    dplyr::left_join(y, intersect(colnames(y), common_cols())) |>
    dplyr::mutate(
      projected_contacts = .data[["contacts"]] * .data[["growth_coeff"]]
    )
}

summarise_total_contacts_by_year <- function(x) {
  x |>
    purrr::map(\(x) {
      dplyr::summarise(x, dplyr::across("projected_contacts", sum), .by = "fin_year")
    })
}

add_age_groups <- function(.data) {
  .data |>
    dplyr::mutate(age_group_cat = dplyr::case_when(
      age_int == 0L ~ "0",
      age_int %in% seq(1L, 4L) ~ "1-4",
      age_int %in% seq(5L, 9L) ~ "5-9",
      age_int %in% seq(10L, 15L) ~ "10-15",
      age_int %in% seq(16L, 17L) ~ "16-17",
      age_int %in% seq(18L, 34L) ~ "18-34",
      age_int %in% seq(35L, 49L) ~ "35-49",
      age_int %in% seq(50L, 64L) ~ "50-64",
      age_int %in% seq(65L, 74L) ~ "65-74",
      .default = "85+"
    ))
}

# Purely invented new groupings - they might be useful for some charts?
add_broad_age_groups <- function(.data) {
  .data |>
    dplyr::mutate(broad_age_cat = dplyr::case_when(
      age_int %in% seq(0L, 15L) ~ "0-15",
      age_int %in% seq(16L, 49L) ~ "16-49",
      age_int %in% seq(50L, 74L) ~ "50-74",
      .default = "75+"
    ))
}

refine_contacts_data <- function(.data) {
  .data |>
    dplyr::filter(
      dplyr::if_any("lad18cd", \(x) !is.na(x)) &
        dplyr::if_any("age_int", \(x) !is.na(x)) &
        dplyr::if_any("gender_cat", \(x) x %in% c("Female", "Male"))
    )
}



