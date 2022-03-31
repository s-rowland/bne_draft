
% 1: run bne with selected parameters
[W,w0,SigW,Z,piZ,Zt,MSE] = train_BNE_v1('daily', 5, 2,15,2,30, 0.01, 'julianDay', 1234)

% 2: create folder to place predictions 
mkdir outputs/pm25/daily/runs/testing2

 
% 3: Generate predictions
% 3a bring in the dataset
target =readtable('inputs/pm25/prediction_datasets/daily_individual/preds_2007_101.csv');
% 3b generate and save predictions
predict_BNE_v1(W,w0,SigW,Z,piZ,Zt,...
    target, 'pred_timeSlice', ...
    5, 2,15,2,30, 'julianDay', ...
    'outputs/pm25/daily/runs/testing2', 'test');


