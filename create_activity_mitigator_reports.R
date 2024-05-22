
all_schemes <- readRDS("secret/all_peers.rds")

purrr::walk(all_schemes,
            ~quarto::quarto_render(
              input = "activity_mitigators_template.qmd",
              execute_params = list("selected_peer" = .x),
              output_file = glue::glue("DRAFT_{.x}_activity_mitigators_report.html")
            ))
