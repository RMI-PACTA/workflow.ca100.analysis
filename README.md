# workflow.ca100.analysis
This repository serves for CA100 analysis. We deliver once a year around September to CA100 indicators at company level. 
Indicators includes 
* aggregate scores for Power and Automotive sectors
* traffic lights for build out for each technology for Power and Automotive sectors
* emission intensity assessment for aviation, steel and cement

## How to use this workflow

You first need to create a .env text file with the following variable:

R_CONFIG_ACTIVE="2022Q2"
DATA_STORE_PATH="path/to/AI/data"
OUTPUT_DIR="path/where/we/want/to/save/results"
ETP_PATHWAY="path/to/collect/etp/hdv/scenarios"
TEMPLATE_DIR="path/to/raw/results/template"
LAST_FILE_USED="path/to/old/ca100/results"

LAST_FILE_USED allows us to reuse 2019 results for Aviation. It's deprecated with last release but still possible to do uncommenting corresponding lines in CA100_assemble.R

If you run it for a new timestamp, you need to add it in config.yml file.

Then you first need to run prepare_CA100_analysis_file.R
It will create the files you need to run the analysis from AI input.
The output will look like the one we have when running pacta.data.preparation but you need to run it from here because we consider Hybrid, HydroCap and NuclearCap as brown technology. This is because for some companies, it doesn't make sense to ask them to build out some hydro or nuclear capacities. (for example pure renewables player).

You then need to run CA100_aggregate_score.R, CA100_Trajectory.R and CA100_Emission_Intensity.R

Each of those file will create an output, that is a csv file with the raw results.

Finally we need to run CA100_assemble.R to assemble all the results in the xlsx file. 
