# nhp_peers_params

## Purpose

Generate an automated report that presents visually each scheme's selected mitigator values for New Hospital Programme (NHP) modelling and compare them against their peers' selections and the National Elicitation Exercise (NEE). 

This report is deployed to Posit Connect: [URL TBC]

This is a static version of the report that acts as a stopgap until [the mitigator-comparison app](https://github.com/The-Strategy-Unit/nhp_inputs_report_app) is live.

## Instructions

To run the report locally:

1. Put in the `data/` folder the following supporting files:
    * `all_params.json`
    * `mitigators.json`
    * `nee_table.rds`
    * `mitigator_name_lookup.csv`
    * `mitigator groupings.xlsx`
    * `NHP_trust_code_lookup.xlsx`
1. Create a `.Renviron` file that contains the variables in `.Renviron.example`.
1. Open and render `nhp-mitigators-report.qmd` to produce the HTML report.
