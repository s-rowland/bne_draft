
addpath("scripts/f_run_BNE/bne_v1/")

training_full = readtable(append('inputs/pm25/training_datasets/',... 
    'annual', '_combined/training_cvfolds.csv'));

[rmse, r2, coverage, me, slope] = cross_validate_BNE_v1(training_full, 7, ...
        2, 0.5, 2, 0.5, ...
        0.0498, 0.1353, 'year', 2, 1234)

