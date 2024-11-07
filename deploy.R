# Run this script to deploy the app to Connect
# https://connect.strategyunitwm.nhs.uk/connect/#/apps/3bf39fd6-aa18-432c-b45c-4a3c639c827a

# If you haven't deployed and don't have an rsconnect folder in your local root
# of this project, you can find the app ID under 'Content ID' in the
# Settings > Info panel of the app on Connect
app_id <- rsconnect::deployments(".")$appID

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
  appId = app_id,  
  appFiles = files
)
