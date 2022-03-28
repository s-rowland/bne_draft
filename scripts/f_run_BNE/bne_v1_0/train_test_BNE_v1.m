function [partMSE] = train_predict_BNE_v1(window, num_models, fold, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)
% % 
% % === Inputs ===
% %  
% %     window: (string) whether annual or daily 
% %     num_mods: number of base models included 
% %     trainfold: (string) describes the locations used for training 
% %     predFold: (string) describes the locations/times we will predict 
% %     
% %     len_scale : (scalar) RBF kernel parameter (we've been using 3.5, but not optimized)
% %     penalty: (numeric) strength of the prior ; lambda
% %     bool_periodic: (string) whether julian day or day of year; only for daily data 
% %     seed: (numeric) the seed 
% % 
% %  === Outputs ===
% % 
% %     a written matrix of the mean values of the model parameters and
% %     their standard deviation. For predicted concentration, additional values
% %     like the 2.5th and 97.5th percentiles are reported. 

%%%% ------------------------------ %%%%
%%%% 0: Set Up Objects for Test Run %%%%
%%%% ------------------------------ %%%%

 % window = 'annual'; num_models = 7; trainFold = 'all'; predFold = 'refGridConus'; 
 % len_scale_space = 3.5'; len_scale_time = 1; len_scale_space_bias = 3.5';
 % len_scale_time_bias = 1; penalty = 0.1; time_metric = 'annual'; seed = 1234;

 % window = 'daily'; num_models = 5; fold=1;
 % len_scale_space = 3.5'; len_scale_time = 20; len_scale_space_bias = 3.5';
 % len_scale_time_bias = 20; penalty = 0.1; time_metric = 'julianDay'; seed = 1234;
 % yyyy_start = 2005; yyyy_end=2015; dir_out = 'test_run';
 
 %  time_metric = 'dayOfYear';
 
%%%% ------------ %%%%
%%%% 1: Train BNE %%%%
%%%% ------------ %%%%

% set seed
rng(seed)
% 1a additional features that are consistent across model runs
num_rand_feat = 500;

% 1b bring in training data
% 1b.i bring in the full training dataset
training_full = training_original;
% 1b.ii remove the time column you do not use 
if strcmp(time_metric, 'dayOfYear')
    training_full.julian_day = [];
    training_full.day_of_year = training_full.day_of_year ./ training_full.max_doy;
elseif strcmp(time_metric, 'julianDay')
    training_full.day_of_year = [];
end

% 1b.ii drop leave-out observations 
%foldNum = str2num(cell2mat(extractBetween(trainFold, 5, 6)))
training = training_full(training_full.fold ~=fold,:);
testing = training_full(training_full.fold ==fold,:);

% 1c break down the training data into its components
% note that column 3 is date, which we aren't using yet
trainLatLon = training{:,1:2};
trainTime = training{:,3};
trainAqs = training{:,4};
trainPreds = training{:,5:(4+num_models)};
    
% 1d train BNE
% note that the parameters will be estimated for those 500 random features,
% and we then use the information about the parameter values to get
% estimates at each point
% note also since this is a maximum a posteriori model, not MCMC or VI, we
% get the best-fit values and not whole distributions. Distributions are
% estimated in the prediction phase

[W,w0,Z,piZ,Zt,MSE] = BNE_v1_0_nosigw(trainAqs, trainLatLon, trainTime, ...
    trainPreds, num_rand_feat,len_scale_space,len_scale_time, ...
    len_scale_space_bias,len_scale_time_bias, penalty, time_metric);

%%%% ----------------------------------------- %%%%
%%%% 2: Determine Error at Left-out Sites %%%%
%%%% ----------------------------------------- %%%%


% 2a break down the testing data into its components
testLatLon = testing{:,1:2};
testTime = testing{:,3};
testAqs = testing{:,4};
testPreds = testing{:,5:(4+num_models)};
test_size = size(testLatLon, 1);

% 2b set up the Phi's to translate our testing points to the RFF grid
if strcmp(time_metric, 'dayOfYear')
    Phi = sqrt(2/num_rand_feat)*cos(Z*testLatLon'/len_scale_space + Zt*58.0916*[cos(2*pi*testTime)' ; sin(2*pi*testTime)']/len_scale_time + piZ*ones(1,test_size));
    Phi_bias = sqrt(2/num_rand_feat)*cos(Z*testLatLon'/len_scale_space_bias + Zt*58.0916*[cos(2*pi*testTime)' ; sin(2*pi*testTime)']/len_scale_time_bias + piZ*ones(1,test_size));
else
    Phi = sqrt(2/num_rand_feat)*cos(Z*testLatLon'/len_scale_space + Zt*testTime'/len_scale_time + piZ*ones(1,test_size));
    Phi_bias = sqrt(2/num_rand_feat)*cos(Z*testLatLon'/len_scale_space_bias + Zt*testTime'/len_scale_time_bias + piZ*ones(1,test_size));
end

% 2c generate predictions
dotWPhi = W'*Phi;
softmax = exp(dotWPhi);
softmax = softmax./repmat(sum(softmax,1),num_models,1);
model_avg = sum(softmax.*testPreds',1);
bias = w0'*Phi_bias;
preds = model_avg + bias;

% 2d determine error 
error = testAqs' - preds;

% 2e get mean square error
mse_fold = mean(error.^2);
 
% 2f get the partial MSE - make the mse proportional to the amount of data
% in the testing set.
partMSE = mse_fold * test_size / size(training_full,1);
     
end