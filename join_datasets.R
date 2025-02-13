# See `build_datasets.R` for the creation of the initial data objects used below


# Calculate growth coefficients per financial year ------------------------


# 1,245,972 rows
popn_fy_projected <- popn_proj_tidy |>
  dplyr::mutate(
    fin_year = paste0(.data[["cal_yr"]], "_", lead(.data[["cal_yr"]]) %% 100),
    fin_year_popn = (.data[["value"]] * 0.75) + (lead(.data[["value"]]) * 0.25),
    growth_coeff = .data[["fin_year_popn"]] / first(.data[["fin_year_popn"]]),
    .by = all_of(c("lad18cd", "age_int", "gender_cat")),
    .keep = "unused"
  ) |>
  dplyr::filter(!if_any("fin_year_popn", is.na))



# Calculate projected community contacts ----------------------------------

common_cols <- \() c("lad18cd", "lad18nm", "age_int", "gender_cat")

join_popn_proj_data <- function(x, y = popn_fy_projected) {
  x |>
    dplyr::left_join(y, any_of(common_cols())) |>
    dplyr::mutate(
      projected_contacts = .data[["contacts"]] * .data[["growth_coeff"]]
    )
}

summarise_total_contacts_by_year <- function(x) {
  x |>
    purrr::map(\(x) {
      dplyr::summarise(x, across("projected_contacts", sum), .by = "fin_year")
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

# I have just invented these groupings - they might be useful for some charts?
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
      if_any("lad18cd", \(x) !is.na(x)) &
        if_any("age_int", \(x) !is.na(x)) &
        if_any("gender_cat", \(x) x %in% c("Female", "Male"))
    )
}



# Data split by provider
contacts_fy_projected_provider <- csds_contacts_2022_23_provider_summary |>
  # This filter results in:
  #   * 1,750,507 contacts with no LAD (no locatable postcode)
  #   * 127,757 contacts with no age recorded
  #   * 143,540 contacts with gender missing or 'Unknown'
  # being excluded from the dataset.
  # 1.84mn contacts in total are excluded (1.91% of all contacts in the dataset)
  refine_contacts_data() |>
  # Nesting creates a list-col "data", with a single tibble per row/provider
  tidyr::nest(.by = "provider_org_id") |>
  dplyr::mutate(across("data", \(x) purrr::map(x, join_popn_proj_data))) |>
  # Example of creating a summary tibble for each provider using custom fn.:
  dplyr::mutate(
    total_contacts_by_year = summarise_total_contacts_by_year(.data[["data"]])
  )



# Data split by ICB
contacts_fy_projected_icb <- csds_contacts_2022_23_icb_summary |>
  refine_contacts_data() |>
  # Nesting creates a list-col "data", with a single tibble per row/provider
  tidyr::nest(.by = c("icb22cdh", "icb22nm")) |>
  dplyr::mutate(across("data", \(x) purrr::map(x, join_popn_proj_data))) |>
  # Example of creating a summary tibble for each ICB using custom fn.
  dplyr::mutate(
    total_contacts_by_year = summarise_total_contacts_by_year(.data[["data"]])
  )



# Data split by local authority
contacts_fy_projected_by_la <- csds_contacts_2022_23_icb_summary |>
  refine_contacts_data() |>
  dplyr::summarise(across("contacts", sum), .by = all_of(common_cols())) |>
  dplyr::left_join(popn_fy_projected, any_of(common_cols())) |>
  dplyr::mutate(
    projected_contacts = .data[["contacts"]] * .data[["growth_coeff"]]
  ) |>
  dplyr::mutate(
    total_contacts_by_year = sum(.data[["projected_contacts"]]),
    total_popn_by_year = sum(.data[["fin_year_popn"]]),
    pct_contacts_of_popn = (
      .data[["total_contacts_by_year"]] / .data[["total_popn_by_year"]]
    ),
    .by = c("lad18cd", "lad18nm", "fin_year")
  ) |>
  add_age_groups() |>
  dplyr::summarise(
    age_group_contacts_by_year = sum(.data[["projected_contacts"]]),
    age_group_popn_by_year = sum(.data[["fin_year_popn"]]),
    pct_age_group_contacts_of_popn = (
      .data[["age_group_contacts_by_year"]] / .data[["age_group_popn_by_year"]]
    ),
    .by = c(
      "lad18cd",
      "lad18nm",
      "fin_year",
      "total_contacts_by_year",
      "total_popn_by_year",
      "pct_contacts_of_popn",
      "age_group_cat"
    )
  )



# National data
contacts_fy_projected_national <- csds_contacts_2022_23_icb_summary |>
  refine_contacts_data() |>
  dplyr::summarise(across("contacts", sum), .by = all_of(common_cols())) |>
  dplyr::left_join(popn_fy_projected, any_of(common_cols())) |>
  dplyr::mutate(
    projected_contacts = .data[["contacts"]] * .data[["growth_coeff"]]
  ) |>
  dplyr::mutate(
    total_contacts_by_year = sum(.data[["projected_contacts"]]),
    total_popn_by_year = sum(.data[["fin_year_popn"]]),
    pct_contacts_of_popn = (
      .data[["total_contacts_by_year"]] / .data[["total_popn_by_year"]]
    ),
    .by = "fin_year"
  ) |>
  add_broad_age_groups() |>
  dplyr::mutate(
    broad_age_group_contacts_by_year = sum(.data[["projected_contacts"]]),
    broad_age_group_popn_by_year = sum(.data[["fin_year_popn"]]),
    pct_broad_age_group_contacts_of_popn = (
      .data[["age_group_contacts_by_year"]] / .data[["age_group_popn_by_year"]]
    ),
    .by = c(
      "fin_year",
      "total_contacts_by_year",
      "total_popn_by_year",
      "pct_contacts_of_popn",
      "broad_age_cat"
    )
  ) |>
  add_age_groups() |>
  dplyr::summarise(
    age_group_contacts_by_year = sum(.data[["projected_contacts"]]),
    age_group_popn_by_year = sum(.data[["fin_year_popn"]]),
    pct_age_group_contacts_of_popn = (
      .data[["age_group_contacts_by_year"]] / .data[["age_group_popn_by_year"]]
    ),
    .by = c(
      "fin_year",
      "total_contacts_by_year",
      "total_popn_by_year",
      "pct_contacts_of_popn",
      "broad_age_cat",
      "broad_age_group_contacts_by_year",
      "broad_age_group_popn_by_year",
      "pct_broad_age_group_contacts_of_popn",
      "age_group_cat"
    )
  )
