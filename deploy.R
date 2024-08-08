# Run this script to deploy the app to Connect
# https://connect.strategyunitwm.nhs.uk/connect/#/apps/3bf39fd6-aa18-432c-b45c-4a3c639c827a

# Paths to files required for deployment
files <- c(
  "nhp-mitigators-report.qmd",
  file.path(
    "R",
    c(
      "azure.R",
      "heatmaps.R",
      "lookups.R",
      "plot.R",
      "read.R",
      "tabulate.R"
    )
  ),
  file.path(
    "data",
    c(
      "all_params.json",
      "all_peers.json",
      "mitigator groupings.xlsx",
      "mitigator_name_lookup.csv",
      "nee_table.rds",
      "NHP_trust_code_lookup.xlsx",
      "providers.json"
    )
  )
)

rsconnect::deployApp(
  appId = 290,  # 'Content ID' in Settings > Info panel of the app on Connect
  appFiles = files
)
