function [W,w0,SigW,Z,piZ,Zt,MSE] = train_BNE_v1(window, num_models, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, penalty_bias, time_metric, seed, stage)
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

% 1a set seed
rng(seed)

% 1b additional features that are consistent across model runs
num_rand_feat = 500;

% 1c bring in the full training dataset
training = readtable(append('inputs/pm25/training_datasets/',window, '_combined/training_cvfolds.csv'));

% 1d remove the time column you do not use 
if strcmp(time_metric, 'dayOfYear')
    training.julian_day = [];
elseif strcmp(time_metric, 'julianDay')
    training.percent_of_year = [];
end

% 1e break down the training data into its components
trainLatLon = training{:,1:2};
trainTime = training{:,3};
trainAqs = training{:,4};
trainPreds = training{:,5:(4+num_models)};
    
% 1f train BNE
% note that the parameters will be estimated for those 500 random features,
% and we then use the information about the parameter values to get
% estimates at each point
% note also since this is a maximum a posteriori model, not MCMC or VI, we
% get the best-fit values and not whole distributions. Distributions are
% estimated in the prediction phase

[W,w0,SigW,Z,piZ,Zt,MSE] = BNE_v1_0(trainAqs, trainLatLon, trainTime, ...
    trainPreds, num_rand_feat,len_scale_space,len_scale_time, ...
    len_scale_space_bias,len_scale_time_bias, penalty,penalty_bias, time_metric, stage);
end