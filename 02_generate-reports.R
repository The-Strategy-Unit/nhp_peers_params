# Generate mitigator selection report

all_schemes <- readRDS("outputs/processed-data/all_peers.rds")

xfun::in_dir(file.path("outputs", "reports"), {
  purrr::walk(
    all_schemes,
    \(scheme_code) quarto::quarto_render(
      input = "activity_mitigators_template.qmd",
      execute_params = list("selected_peer" = scheme_code),
      output_file = glue::glue(
        "DRAFT_{Sys.Date()}_{scheme_code}_activity-mitigators-report.html"
      )
    )
  )
})


