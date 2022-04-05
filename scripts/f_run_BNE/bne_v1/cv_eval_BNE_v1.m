function [partial_mse, partial_r2, partial_cover] = cv_eval_BNE_v1(training_full, fold,... 
    num_models, ...
    scale_space_w, scale_time_w, scale_space_rp, scale_time_rp, ...
    lambda_w, lambda_rp, time_metric, opt_stage, seed)
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

%%%% -------------------------------- %%%%
%%%%  0: Set Up Objects for Test Run  %%%%
%%%% -------------------------------- %%%%

 % window = 'annual'; num_models = 7; trainFold = 'all'; predFold = 'refGridConus'; 
 % len_scale_space = 3.5'; len_scale_time = 1; len_scale_space_bias = 3.5';
 % len_scale_time_bias = 1; penalty = 0.1; time_metric = 'annual'; seed = 1234;
 % training_full = readtable(append('inputs/pm25/training_datasets/','annual', '_combined/training_cvfolds.csv'));
% fold = 1

%%%% ----------------------------------------- %%%%
%%%%  1: Split Data into Training and Testing  %%%%
%%%% ----------------------------------------- %%%%

training = training_full(training_full.fold ~=fold,:);
testing = training_full(training_full.fold ==fold,:);

%%%% ------------------------------------- %%%%
%%%%  2: Train BNE on Left-In Observations %%%%
%%%% ------------------------------------- %%%%

% 2.a. extract components
[trainSpace, trainTime, trainPreds, trainAqs, num_points] =  ...
    extract_components(training, num_models, time_metric);

% 2.b. train BNE
[W,RP,sigW,Zs,Zt,piZ,mse] = BNE_v1(trainAqs, trainSpace, trainTime, trainPreds, ...
    scale_space_w, scale_time_w, scale_space_rp, scale_time_rp, ...
    lambda_w, lambda_rp, time_metric, opt_stage, seed, 'cv')


%%%% ----------------------------------------- %%%%
%%%%  3: Evalute BNE on Left-Out Observations  %%%%
%%%% ----------------------------------------- %%%%

 [partial_mse, partial_r2, partial_cover] = predict_BNE_v1(W,RP,sigW,Zs,Zt,piZ, ...
    testing, size(training_full,1), 'cv', num_models, ...
    scale_space_w, scale_time_w, scale_space_rp, scale_time_rp, time_metric, ...
    'nosave', 'nosave');


end