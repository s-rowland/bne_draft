
% run bne
[W,w0,SigW,Z,piZ,Zt,MSE] = train_BNE_v1('daily', 5, 2,15,2,30, 0.01, 'julianDay', 1234)

% create folder to place stuff 
mkdir outputs/pm25/daily/runs/testing2

% generate whatever predictions you want
  target =readtable('inputs/pm25/prediction_datasets/daily_individual/preds_2007_101.csv');
  targetYYYY = 2007; targetDoY = 101;
 

predict_BNE_v1(W,w0,SigW,Z,piZ,Zt,...
    target, targetYYYY, targetDoY, ...
    5, 2,15,2,30, 0.01, 'julianDay', 1234, ...
    'outputs/pm25/daily/runs/testing2')
