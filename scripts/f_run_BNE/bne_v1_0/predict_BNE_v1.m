function [W] = predict_BNE_v1(W,w0,SigW,Z,piZ,Zt,...
    target, targetYYYY, targetDoY, targetName, num_models, ...
    outPath, outName)
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
 % target =readtable('inputs/pm25/prediction_datasets/daily_individual/preds_2007_101.csv');
 % targetYYYY = 2007; targetDoY = 101;

%%%% ------------------------------- %%%%
%%%% 1: Prepare Data for Predictions %%%%
%%%% ------------------------------- %%%%

% 1a determine the max_doy
if targetYYYY == 2008 | targetYYYY == 2012 | targetYYYY == 2016
    max_doy = 366;
else max_doy = 365
end

% 1b determine target time variable
if strcmp(time_metric, 'dayOfYear')
    target.julian_day = [];
elseif strcmp(time_metric, 'julianDay')
    target.percent_of_year = [];
end

% 1c extract components of target 
% each target is a single time slice containing all relevant locations
X = table2array(target(:, [1,2]));
time = table2array(target(:, [3]));
f_all = table2array(target(:, [4:(3+num_models)]));

%%%% -------------------------------- %%%%
%%%% 2: Set Up Objects for Prediction %%%%
%%%% -------------------------------- %%%%

% 2a Create num_samp samples of W and w0 from a Gaussian distribution used
% for calculating empirical mean and standard deviations. More samples is
% slower but more accurate.
% Basically, we take samples of the parameter values 
num_rand_feat = 500;

% 2b muW is a vector of the mean values of the parameters of weights and
% offset term 
muW = [W(:) ; w0];

% 2c set the number of samples that we will take 
num_samp = 250;

% 2d take the samples. we generate random numbers based on gaussian
% distributions, where the means are the mean values and the variances are 
% stored in SigW, which we calculate in the second half of the BNE function
wsamp1 = mvnrnd(muW,SigW,num_samp)';

% 2e put samples in tidy format
w0samp = [];
wsamp = [];
for s = 1:num_samp
    w0samp = [w0samp wsamp1(end-num_rand_feat+1:end,s)];
    wsamp = [wsamp reshape(wsamp1(1:num_models*num_rand_feat,s),num_rand_feat,num_models)];
end

% 2f create empty vectors to fill
softmax_mean = zeros(size(X,1),num_models);
softmax_sd = zeros(size(X,1),num_models);
ens_mean = zeros(size(X,1),1);
ens_sd = zeros(size(X,1),1);
bias_mean = zeros(size(X,1),1);
bias_sd = zeros(size(X,1),1);
y_mean = zeros(size(X,1),1);
y_sd = zeros(size(X,1),1);
y_95CIl = zeros(size(X,1),1);
y_95CIu = zeros(size(X,1),1);
y_90CIl = zeros(size(X,1),1);
y_90CIu = zeros(size(X,1),1);
y_85CIl = zeros(size(X,1),1);
y_85CIu = zeros(size(X,1),1);
y_80CIl = zeros(size(X,1),1);
y_80CIu = zeros(size(X,1),1);
y_75CIl = zeros(size(X,1),1);
y_75CIu = zeros(size(X,1),1);
y_70CIl = zeros(size(X,1),1);
y_70CIu = zeros(size(X,1),1);
            
%%%% ----------------------- %%%%
%%%% 3: Generate Predictions %%%%
%%%% ----------------------- %%%%

% 3a begin loop over the individual points
for i = 1:size(X,1)
    
    % 3b set up the way to translate time to the RFF
    if strcmp(time_metric, 'dayOfYear') 
        Phi = sqrt(2/num_rand_feat)*cos(Z*X(i,:)'/len_scale_space + Zt*58.0916*[cos(2*pi*time(i,:) ; sin(2*pi*time(i,:))]/len_scale_time + piZ);
        Phi_bias = sqrt(2/num_rand_feat)*cos(Z*X(i,:)'/len_scale_space_bias + Zt*58.0916*[cos(2*pi*time(i,:)) ; sin(2*pi*time(i,:))]/len_scale_time_bias + piZ);
    else
        Phi = sqrt(2/num_rand_feat)*cos(Z*X(i,:)'/len_scale_space + Zt*time/len_scale_time + piZ);
        Phi_bias = sqrt(2/num_rand_feat)*cos(Z*X(i,:)'/len_scale_space_bias + Zt*time/len_scale_time_bias + piZ);
    end
    
    % 3c sample the weights
    softmax = Phi'*wsamp;
    softmax = reshape(softmax',num_models,num_samp)';
    softmax = exp(softmax);
    softmax = softmax./repmat(sum(softmax,2),1,num_models);
    ens = softmax*f_all(i,:)';
    bias = Phi_bias'*w0samp;
    y = softmax*f_all(i,:)' + bias';
    
    % 3d fill in those empty arrays
    softmax_mean(i,:) = mean(softmax,1);
    softmax_sd(i,:) = std(softmax,1);
    ens_mean(i) = mean(ens);
    ens_std(i) = std(ens);
    bias_mean(i) = mean(bias);
    bias_sd(i) = std(bias);
    y_mean(i) = mean(y);
    y_sd(i) = std(y);
    y_95CIl(i) = quantile(y, 0.025);
    y_95CIu(i) = quantile(y, 0.975);
    y_90CIl(i) = quantile(y, 0.05);
    y_90CIu(i) = quantile(y, 0.95);
    y_85CIl(i) = quantile(y, 0.075);
    y_85CIu(i) = quantile(y, 0.925);
    y_80CIl(i) = quantile(y, 0.10);
    y_80CIu(i) = quantile(y, 0.90);
    y_75CIl(i) = quantile(y, 0.125);
    y_75CIu(i) = quantile(y, 0.875);
    y_70CIl(i) = quantile(y, 0.15);
    y_70CIu(i) = quantile(y, 0.85);
    
    % 3e progress message
    if mod(i,1000) == 0
        disp([num2str(time) '/' num2str(temp+365) ' ::: ' num2str(i/size(X,1))]);
    end
            
% 3f finish loop
end

%%%% ------------------- %%%%
%%%% 5: Save Predictions %%%%
%%%% ------------------- %%%%

% 5a nicely label the weight columns
if num_models == 5
    w_mean_av = softmax_mean(:,1);
    w_sd_av = softmax_std(:,1);
    w_mean_cm = softmax_mean(:,2);
    w_sd_cm = softmax_std(:,2);
    w_mean_js = softmax_mean(:,3);
    w_sd_js = softmax_std(:,3);
    w_mean_me = softmax_mean(:,4);
    w_sd_me = softmax_std(:,4);
    w_mean_rk = softmax_mean(:,5);
    w_sd_rk = softmax_std(:,5);
    weights = table(w_mean_av, w_sd_av, w_mean_cm, w_sd_cm, w_mean_js, ...
    w_sd_js, w_mean_me, w_sd_me, w_mean_rk, w_sd_rk, 'VariableNames', ...
    {'w_mean_av', 'w_sd_av', 'w_mean_cm', 'w_sd_cm', 'w_mean_js', ...
    'w_sd_js', 'w_mean_me', 'w_sd_me', 'w_mean_rk', 'w_sd_rk'});
    
elseif num_models == 6
        w_mean_av = softmax_mean(:,1);
        w_sd_av = softmax_std(:,1);
        w_mean_cb = softmax_mean(:,2);
        w_sd_cb = softmax_std(:,2);
        w_mean_cm = softmax_mean(:,3);
        w_sd_cm = softmax_std(:,3);
        w_mean_js = softmax_mean(:,4);
        w_sd_js = softmax_std(:,4);
        w_mean_me = softmax_mean(:,5);
        w_sd_me = softmax_std(:,5);
        w_mean_rk = softmax_mean(:,6);
        w_sd_rk = softmax_std(:,6);
        weights = table(w_mean_av, w_sd_av, w_mean_cb, w_sd_cb, w_mean_cm, w_sd_cm, w_mean_js, ...
            w_sd_js, w_mean_me, w_sd_me, w_mean_rk, w_sd_rk, 'VariableNames', ...
            {'w_mean_av', 'w_sd_av', 'w_mean_cb', 'w_sd_cb', 'w_mean_cm', 'w_sd_cm', 'w_mean_js', ...
            'w_sd_js', 'w_mean_me', 'w_sd_me', 'w_mean_rk', 'w_sd_rk'});
end

% 5b combine other parameters
lat = X(:,1);
lon = X(:,2);
otherparam = table(lat, lon, time, ens_mean,ens_sd, bias_mean, bias_sd, y_mean, y_sd, ... 
    y_95CIl, y_95CIu, y_90CIl, y_90CIu,y_85CIl, y_85CIu,y_80CIl, y_80CIu,...
    y_75CIl, y_75CIu,y_70CIl, y_70CIu,...
    'VariableNames', ... 
    {'lat', 'lon', 'ens_mean', 'ens_sd', 'bias_mean',  'bias_sd', 'y_mean', 'y_sd', ...
    'y_95CIl', 'y_95CIu', 'y_90CIl', 'y_90CIu', 'y_85CIl', 'y_85CIu', 'y_80CIl', 'y_80CIu', ...
    'y_75CIl', 'y_75CIu', 'y_70CIl', 'y_70CIu'});
   
% 5c combine all parameters 
results = [weights otherparam];

% 5d add observations if doing external validation
if strcomp(targetName, 'extVal')
   obs = target.obs; 
   obstab = table(obs, 'VariableNames', {'obs'})
   results = [results obstab]
end

% 5e save as csv
writetable(results, append(outPath, '/', outName,'.csv')) 

 

 % end function
end