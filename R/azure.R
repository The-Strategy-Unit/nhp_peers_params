flag_trust <- function(trust_data,trust_code) {
  trust_data <- trust_data |>
    mutate(trust=trust_code)
  return(trust_data)
}

get_container <- function() {
  t <- AzureAuth::get_azure_token(
    "https://storage.azure.com",
    tenant=Sys.getenv("AZ_TENANT_ID"),
    app=Sys.getenv("AZ_APP_ID"),
    password=Sys.getenv("AZ_APP_SECRET")
  )

  ep <- AzureStor::blob_endpoint(Sys.getenv("AZ_STORAGE_EP"), token = t)

  AzureStor::blob_container(ep, Sys.getenv("AZ_STORAGE_CONTAINER"))
}

get_result_sets <- function() {
  cont <- get_container()

  cont |>
    AzureStor::list_blobs("prod", info = "all", recursive = TRUE) |>
    dplyr::filter(!.data[["isdir"]]) |>
    purrr::pluck("name") |>
    purrr::set_names() |>
    purrr::map(\(name, ...) AzureStor::get_storage_metadata(cont, name)) |>
    dplyr::bind_rows(.id = "file") |>
    dplyr::mutate(
      dplyr::across("viewable", as.logical)
    )
}

get_results_from_azure <- function(filename) {
  cont <- get_container()
  tf <- withr::local_tempfile()
  AzureStor::download_blob(cont, filename, tf)

  readBin(tf, raw(), n = file.size(tf)) |>
    jsonlite::parse_gzjson_raw(simplifyVector = FALSE) |>
    parse_results()
}

parse_results <- function(r) {
  r$population_variants <- as.character(r$population_variants)

  r$results <- purrr::map(
    r$results,
    purrr::map_dfr,
    purrr::modify_at,
    c("model_runs", "time_profiles"),
    purrr::compose(list, as.numeric)
  )

  r
}

get_individual_trust_results <- function(result_sets,scenario_name) {
  #gets the latest dated run for a named scenario
  r_trust <- result_sets |>
    dplyr::filter(scenario == scenario_name) |>
    dplyr::arrange(desc(create_datetime)) |>
    _$file[[1]] |>
    get_results_from_azure()
  return(r_trust)
}

get_baseline_and_projections <- function(r_trust) {
  r_trust <- r_trust[["results"]][["default"]]
  trust <- r_trust |>
    group_by(measure, pod)|>
    summarise(baseline=sum(baseline),
              principal=sum(principal))
  return(trust)

}
