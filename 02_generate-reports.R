# Generate mitigator selection report

all_schemes <- readRDS("data/all_peers.rds")

purrr::walk(
  all_schemes[1],
  \(scheme_code) {
    rmarkdown::render(
      input = file.path(
        "templates",
        "activity_mitigators_template.qmd"
      ),
      params = list("selected_peer" = scheme_code),
      output_file = file.path(
        "..",
        "outputs", 
        "reports",
        glue::glue(
          "DRAFT_{Sys.Date()}_{scheme_code}_activity-mitigators-report.html"
        )
      )
    )
  }
)
