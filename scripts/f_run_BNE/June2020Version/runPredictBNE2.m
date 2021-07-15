%function [W] = runPredictBNE(inputset, Sscale, Tscale, foldMethod, fold)

inputset = 'AVGSCMJSCC';
Sscale = 6.5
Tscale = 0.5

foldMethod = 'all'
fold = 'all'

trainAqs = readtable(append('BNE_Inputs/c_04_parcels_temporal/annual/train_aqs_',  foldMethod, '_', fold, '.csv'));
trainLatlon = readtable(append('BNE_Inputs/c_04_parcels_temporal/annual/train_latlon_', foldMethod, '_', fold, '.csv'));
trainTime = readtable(append('BNE_Inputs/c_04_parcels_temporal/annual/train_time_', foldMethod, '_', fold, '.csv'));
trainPred = readtable(append('BNE_Inputs/c_04_parcels_temporal/annual/train_pred_', inputset, '_', foldMethod, '_', fold, '.csv'));

trainAqs = trainAqs{:,:};
trainLatlon = trainLatlon{:,:};
trainTime = trainTime{:,:};
trainPred = trainPred{:,:};

[W,w0,SigW,Z,piZ] = BNE_stochastic(trainAqs, trainLatlon, trainTime, trainPred, 500, Sscale, Tscale);

