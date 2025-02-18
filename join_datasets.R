# See `build_datasets.R` for the creation of the initial data objects used below






# Data split by provider (probably not needed)
contacts_fy_projected_provider <- csds_contacts_2022_23_provider_summary |>
  # This filter results in:
  #   * 1,750,507 contacts with no LAD
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



# Data split by ICB (the one to be used - create extra data columns within the
# ggplot2 pipelines in the Quarto doc)
contacts_fy_projected_icb <- csds_contacts_2022_23_icb_summary |>
  refine_contacts_data() |>
  # Nesting creates a list-col "data", with a single tibble per row/provider
  tidyr::nest(.by = c("icb22cdh", "icb22nm")) |>
  dplyr::mutate(across("data", \(x) purrr::map(x, join_popn_proj_data))) |>
  # Example of creating a summary tibble for each ICB using custom fn.
  dplyr::mutate(
    total_contacts_by_year = summarise_total_contacts_by_year(.data[["data"]])
  )



# Data split by local authority (now not needed - create in Quarto from ICB)
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



# National data (now not needed - create in Quarto from ICB)
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
