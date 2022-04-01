% 1: run bne with selected parameters
[W,w0,SigW,Z,piZ,Zt,MSE] = train_BNE_v1('daily', 5, 1, 50, 1, 15, 0.0498, 'dayOfYear', 1234, 2)

% 2: create folder to place predictions
mkdir outputs/pm25/daily/runs/external_validation

                              
% 3: Generate predictions
% 3a bring in the dataset
target =readtable('str_uncert_analysis/data/external_validation/inputs/ev_data_assigned_all.csv');
% 3b generate and save predictions
predict_BNE_v1(W,w0,SigW,Z,piZ,Zt,...
    target, 'extVal', ...
    5, 2,30,2,15, 'dayOfYear', ...
    'str_uncert_analysis/outputs/d_bne_results/external_validation', 'External_validation_winner')

