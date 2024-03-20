1. funs_get_param.R has modified functions from nhp inputs (or outputs?) app to get values out of parameter list

2. activity_mitigators.R sources funs_get_param.R and reads model json files found in "secret/jsons/". the first 3 characters of json file is used to extract scheme code. 
   This file also creates all_peers.rds and plot_data.rds in secret folder.

3. plot_activity_mitigator_fun.R is function used to create plots.

4. activity_mitigators_template.qmd is template quarto file used in reports and has parameterised "selected_peer" . Sources plot_activity_mitigator_fun.R and reads in plot_data.rds from secret folder.

5. create_activity_mitigator_reports.R takes all codes from all_peers.rds and walks through them applying quarto render function.
