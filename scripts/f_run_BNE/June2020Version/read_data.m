YYYY = '2010';
inputset = 'AVGSCM';
Kscale = 6.5
KscaleStr = '6.5'

trainAqs = readtable(append('train_aqs_', YYYY, '.csv'));
trainLatlon = readtable(append('train_latlon_', YYYY, '.csv'));
trainPred = readtable(append('train_pred_', YYYY, '_', inputset,  '.csv'));

trainAqs = trainAqs{:,:};
trainLatlon = trainLatlon{:,:};
trainPred = trainPred{:,:};



[W,w0,SigW,Z,piZ] = BNE(trainAqs, trainLatlon, trainPred, 500, Kscale)