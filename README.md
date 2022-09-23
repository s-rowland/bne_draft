# bne_draft

Robbie: I'm sure you will but just a reminder to add a README description here!
# Sebastian: Breif README added here. 
# There is a larger convetions doc that explains more of how to use the code. 


Code to generate results for manuscript. Many datasets of PM2.5 predictions and potential explanatory variables will needed to be downloaded; ways to access each dataset are described in the manuscript. Note that external validation results cannot be reproduced with this repo because that data is not publicly available, though interested readers can contact the research groups (listed in the Supplement). Overall, we 
1. Process prediction models and training data
2. Tune BNE hyperparameters
3. Fit final BNE models with selected hyperparameters 
4. Process potential explanatory variables 
5. Link potential explanatory variables and BNE-estimated predictive uncertainty 
6. Analyze explanatory power of each variable 
7. Present results

To reproduce the code: 
1) download and place the prediction models in the correct folders within inputs/pm25/base_models/annual
2) download the EPA AQS data and place in inputs/pm25/ground_truth/annual 
3) download US state shape file callled cb_2015_us_state_500k.shp at http://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html and place it in ancillary_data/raw/census
to create city-specific BNE results (not included in this manuscript), also download 
tl_2015_us_cbsa.shp from the same site and and place it in ancillary_data/raw/census

4) download the explanatory variables and place them in str_uncert_analysis/data/explanatory_variables/raw
5) The external validation data is not publicly available for sharing, so some results may not be fully reproducible without collecting that data. 
6) Run the code contained in the scripts folder 
7) Run the code contained in str_uncert_analysis/code

Key folders are: 
* inputs: contains prediction data used in BNE model
* scripts: code to process prediction models and tune and fit BNE model
* bne_ppd: contains results of BNE run
* str_uncert_analysis: data, code, and results to analyze potential expalantory variables, using uncertainty generated from scripts code.

Many details can be found in the conventions document, found in scripts/j_create_documentation