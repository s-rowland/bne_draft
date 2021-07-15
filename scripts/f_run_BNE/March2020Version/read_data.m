YYYY = '2011';
inputset = 'AVGSCMJSCC';
Kscale = 3.5
KscaleStr = '3.5'

trainAqs = readtable(append('train_aqs_', YYYY, '_all_all.csv'));
trainLatlon = readtable(append('train_latlon_', YYYY, '_all_all.csv'));
trainPred = readtable(append('train_pred_', YYYY, '_', inputset,  '_all_all.csv'));

trainAqs = trainAqs{:,:};
trainLatlon = trainLatlon{:,:};
trainPred = trainPred{:,:};



[W,w0,SigW,Z,piZ] = BNE(trainAqs, trainLatlon, trainPred, 500, Kscale)