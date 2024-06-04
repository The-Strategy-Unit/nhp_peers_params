# nhp_peers_params

## Purpose

Generate automated reports that present visually each scheme's selected mitigator values for New Hospital Programme (NHP) modelling and compare them against their peers' selections and the National Elicitation Exercise (NEE). 

## Instructions

Before you begin, put each scheme's json results file in `data/jsons/` (this manual process will be replaced later by [reading directly from Azure](https://github.com/The-Strategy-Unit/nhp_peers_params/issues/16)). The latest scenario runs are recorded on SharePoint (for now) in the 'current_runs_for_mitigator_comparison.xlsx' file (to be replaced later by tagging final jsons in Azure). The `data/` folder should also contain `all_params.json`, `mitigators.json` and `nee_table.rds`.

Then run:

1. `01_process-data.R` to create `all_peers.rds` and `plot_data.rds` in `outputs/processed-data/` (these files are required for producing reports and tables).
2. `02_generate-reports.R` to render peers' parameters reports to `outputs/reports/` for each scheme.
3. `03_generate-tables.R`  to create mitigator coverage tables (binary and certainty heatmap) in `outputs/tables/` for each scheme.
