# bne_draft

Robbie: I'm sure you will but just a reminder to add a README description here!
# Sebastian: Brief README added here. 
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
* notes that d_01 scripts should NOT be run - these are to construct daily models, which is outside the scope of the current manuscript.
specifically, run, in this order: 
* scripts
* set up
* a_01_make_conus_outline.R
* STR_a_02_create_refGrids.R
* format base models
* STR_b_01_average_av_by_year.R
* STR_b_02_average_cmaq_by_year.R
* STR_b_03_retrieve_js.R
* STR_b_04_average_rk_by_year.R
* STR_b_05_me_truncation_guides.R
* STR_d_06_blend_merra.R
* format ground truth
* STR_c_01_curate_aqs_data
* create training prediction datasets
* STR_d_02_make_training_prediction_datasets_annual
* STR_d_03_identify_spatial_folds_annual_leader
* run BNE 
* a_grid_search_annual.m
* b_generate_ppd_annual_refGRid.m
* c_generate_ppd_annual_external_validation.m
* d_get_coverage_winner .m
* e_gerenate_preds_at_aqs.m
* f_get_cv_results.m
* str_uncert_analysis
* fit and examine BNE results
* b_01_compile_grid_results.R
* b_02_plot_bne_param.R (not required)
* b_03_compile_external_validation_annual.R
* b_04_make_annual_ev_data.R
* b_05_examine_ev_data.R (not required)
* b_06_assign_baseM_ev_data_annual.R
* b_07_conduct_external_validation_annual.R
* b_08_plot_ev_sites.R (not required)
* collect explanatory variables 
* c_01_collect_census_data.R
* c_02_collect_topography_data.rtf
* c_03a_get_era5_data_land.ipynb
* c_03b_get_era5_data_singleLayer-boundaryH.ipynb
* c_03c_get_era5_data_singleLayer.ipynb
* c_04_collect_PRISM.R
* harmonize data 
* d_01_format_era5_data
* d_02_calculate_annual_era5.R 
* d_03_calculate_annual_giovanni.R
* d_04_wrangle_topo.R
* d_05_make_keys.R 
* d_06_assign_expVar_annualBNE
* explanatory variable analysis
* g_01a_expVar_competition_ySD_gam.R
* g_01b_expVar_competition_ySD_gam_sensitivity15knots.R 
* g_02a_expVar_competition_ySDScaled_gam.R 
* g_02b_expVar_competition_ySDScaled_gam_sensitivity_15knots.R
* all scripts in j_generate_results_for_manuscript



* a_examine_bne_inputs was used in exploratory phase and not required for manuscript

7) Run the code contained in str_uncert_analysis/code

Key folders are: 
* inputs: contains prediction data used in BNE model
* scripts: code to process prediction models and tune and fit BNE model
* bne_ppd: contains results of BNE run
* str_uncert_analysis: data, code, and results to analyze potential expalantory variables, using uncertainty generated from scripts code.

Many details can be found in the conventions document, found in scripts/j_create_documentation

Notes 
* .keep files are added to directories to maintain the folder structure within git
* some packages, so far just nngeo, cannot be installed on the server where we ran the analysis; for nngeo please make sure install it. The need to install nngeo is also stated in a_00_import_packages_set_global_objects.R..