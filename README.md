# nhp_peers_params

## Purpose

A Quarto report to compare mitigator values selected for modelling by schemes in the New Hospital Programme (NHP).

This report is superseded by [the mitigators comparison app](https://github.com/The-Strategy-Unit/nhp_inputs_report_app).

## Instructions

To run the report locally:

1. Put in the `data/` folder the following supporting files:
    * `all_params.json`
    * `all_peers.rds`
    * `mitigator_name_lookup.csv`
    * `mitigator groupings.xlsx`
    * `nee_table.rds`
    * `NHP_trust_code_lookup.xlsx`
    * `providers.json`
1. Create a `.Renviron` file in the project root and fill it with the variables in the provided `.Renviron.example` template.
1. Open and render `nhp-mitigators-report.qmd` to produce the HTML report.

Ask the Data Science team for these files and variables.
