# Organization

This folder contains all scripts necessary to replicate these population projections.

`/000-Libraries.R` - is a simple R script containing the libraries used in all of the subsequent scripts.

`/001-fipscodes.R` - is a simple R script used to download a listing of FIPS codes.

`/002-basedataload.R` - contains the script necessary to generate `DATA/us.1969_2016.19ages.adjusted.txt`. It also contains all of the post-processing of the underlying population estimates for use in the out-of-sample validation.

`/003-proj_basedataload.R` - contains the script necessary to generate `DATA/us.1990_2016.19ages.adjusted.txt`. It also contains all of the post-processing of the underlying population estimates for use in the actual population projections.

`/004-GQ_gather.R` - contains the script necessary to generate `DATA-PROCESSED/gq_2000.csv` and `DATA-PROCESSED/gq_2010.csv`.

`/005-state_level_fert_proj.R` - contains the script necessary to generate `DATA-PROCESSED/state-level-fert-rates_20002015.csv` and `DATA-PROCESSED/state-level-fert-rates_20152100.csv`.

`/006-projections_20002015.R` - contains the script necessary to generate all of the data in `PROJECTIONS/EVAL/`. This script will generate the out-of-sample validation population projections for the period 2000-2015.

`/007-projections_2100.R` - contains the script necessary to generate all of the data in `PROJECTIONS/PROJECTIONS/`. This script will generate the population projections for the period 2015-2100.

`/091-readevalproj.R` - contains a script necessary to generate the `maintext.pdf`. This script will import the out-of-sample validation data produced from the script `/006-projections_20002015.R` .

`/092-readevalproj_fitted.R` - contains a script necessary to generate one of the figures in the `maintext.pdf`. This script will import the out-of-sample validation data produced from the script `/006-projections_20002015.R` and fit it to the national total in a similar manner as controlling the actual projections to the SSPs.

`/093-lexisplotexample.R` - contains a script necessary to generate one of the figures in the `maintext.pdf`. This script will generate a simple lexis diagram example of CCRs and CCDs.

`/094-PROJECTIONS_import.R` - contains a script necessary to generate the `maintext.pdf`. This script will import the final projections controlled to the SSPs.

`/999-SSP_rake_FINALPROJECTIONS.R` - contains the script necessary to rake the final projections, produced in script `/007-projections_2100.R`. This script will out put `/DATA-PROCESSED/SSP_asrc.csv`. This is the final projections.


# Correspondence
For any issues with the functionality of the script please create an issue.

## License
The data collected and presented is licensed under the [Creative Commons Attribution 3.0 license](http://creativecommons.org/licenses/by/3.0/us/deed.en_US), and the underlying code used to format, analyze and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
