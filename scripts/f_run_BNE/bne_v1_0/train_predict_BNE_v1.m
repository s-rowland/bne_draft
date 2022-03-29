function [partMSE] = train_predict_BNE_v1(window, num_models, pred_target, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out)
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
training = readtable(append('inputs/pm25/training_datasets/',window, '_combined/training_cvfolds.csv'));
% 1b.ii remove the time column you do not use 
if strcmp(time_metric, 'dayOfYear')
    training.julian_day = [];
    training.day_of_year = training.day_of_year ./ training.max_doy;
elseif strcmp(time_metric, 'julianDay')
    training.day_of_year = [];
end

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

[W,w0,SigW,Z,piZ,Zt,MSE] = BNE_v1_0(trainAqs, trainLatLon, trainTime, ...
    trainPreds, num_rand_feat,len_scale_space,len_scale_time, ...
    len_scale_space_bias,len_scale_time_bias, penalty, time_metric);

%%%% -------------------------- %%%%
%%%% 2: Prepare for Predictions %%%%
%%%% -------------------------- %%%%

% Create num_samp samples of W and w0 from a Gaussian distribution used
% for calculating empirical mean and standard deviations. More samples is
% slower but more accurate.
% Basically, we take samples of the parameter values 

% 2a muW is a vector of the mean values of the parameters of weights and
% offset term 
muW = [W(:) ; w0];
% 2b set the number of samples that we will take 
num_samp = 250;
% 2c take the samples. we generate random numbers based on gaussian
% distributions, where the means are the mean values and the variances are 
% stored in SigW, which we calculate in the second half of the BNE function
wsamp1 = mvnrnd(muW,SigW,num_samp)';
% 2d put samples in tidy format
w0samp = [];
wsamp = [];
for s = 1:num_samp
    w0samp = [w0samp wsamp1(end-num_rand_feat+1:end,s)];
    wsamp = [wsamp reshape(wsamp1(1:num_models*num_rand_feat,s),num_rand_feat,num_models)];
end

% str silenced
%temp = datenum('2016-01-01')-datenum('2000-01-01');

%%%% ------------------------------- %%%%
%%%% 3: Prepare for Predictions Loop %%%%
%%%% ------------------------------- %%%%

% 1 loop over the years
for YYYY = [2005, 2006, 2007, 2008, 2009, 2010, 2011,2012, 2013, 2014, 2015, 2016]
    
    % 1a determine the maximum number of days the in current year
    if YYYY == 2004 | YYYY == 2008 | YYYY == 2012 | YYYY == 2016
        maxDoY = 366
    else maxDoY = 365
    end
        
    % 1b loop over days within that year
    for DoY =  1:maxDoY 
        
        if strcmp(time_metric, 'annual')
            time = time;
        elseif strcmp(time_metric, 'dayOfYear') 
            time = DoY;
        elseif strcmp(time_metric, 'julianDay') 
            time = DoY + floor(365.25 * (YYYY-2005));
        end
        
        % extract preds for this time
        if strcomp(pred_target, 'refGridDaily') 
            targetPath = append('inputs/pm25/prediction_datasets/daily_individual/', ...
                'preds_', YYYY, '_', pad(num2str(DoY), 3, 'left', '0'), '.csv');
        elseif strcomp(pred_target, 'externalValidationDaily')
                targetPath = append('inputs/pm25/prediction_datasets/daily_individual/', ...
                'preds_', YYYY, '_', pad(num2str(DoY), 3, 'left', '0'), '.csv') ;
        end
        target = readtable(targetPath);
        
        % emp
        y_mean = zeros(size(X,1),1);
        y_std = zeros(size(X,1),1);
        softmax_mean = zeros(size(X,1),num_models);
        softmax_std = zeros(size(X,1),num_models);
        ens_mean = zeros(size(X,1),1);
        ens_std = zeros(size(X,1),1);
        bias_mean = zeros(size(X,1),1);
        bias_std = zeros(size(X,1),1);
        y_mean = zeros(size(X,1),1);
        y_std = zeros(size(X,1),1);
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
            
        % extract components
        X = target(:, [1,2]);
        f_all = target(:, [4:(3+num_models)]);

        % loop over the individual points
        for i = 1:size(X,1)
            if strcmp(time_metric, 'dayOfYear') 
                Phi = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space + Zt*58.0916*[cos(2*pi*time/maxDoY)' ; sin(2*pi*time/maxDoY)']/len_scale_time + piZ);
                Phi_bias = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space_bias + Zt*58.0916*[cos(2*pi*time/maxDoY)' ; sin(2*pi*time/maxDoY)']/len_scale_time_bias + piZ);
            else
                Phi = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space + Zt*time'/len_scale_time + piZ);
                Phi_bias = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space_bias + Zt*time'/len_scale_time_bias + piZ);
            end
            softmax = Phi'*wsamp;
            softmax = reshape(softmax',num_models,num_samp)';
            softmax = exp(softmax);
            softmax = softmax./repmat(sum(softmax,2),1,num_models);
            ens = softmax*f_all{i,:}';
            bias = Phi_bias'*w0samp;
            y = softmax*f_all{i,:}' + bias';
                    softmax_mean(i,:) = mean(softmax,1);
        softmax_std(i,:) = std(softmax,1);
        ens_mean(i) = mean(ens);
        ens_std(i) = std(ens);
        bias_mean(i) = mean(bias);
        bias_std(i) = std(bias);
        y_mean(i) = mean(y);
        y_std(i) = std(y);
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
            %if mod(i,1000) == 0
             %   disp([num2str(time) '/' num2str(temp+365) ' ::: ' num2str(i/size(X,1))]);
            %end
            
                % 4b save as csv
    writetable(results, append('outputs/pm25/daily/runs/BNE_',...
        string(len_scale_space),'_', string(len_scale_time), '_', ...
        string(len_scale_space_bias),'_', string(len_scale_time_bias), '_', ...
        string(penalty), '_', time_metric, '_', string(seed), '_', ...
        YYYY, '_', DoY,  '.csv')) 
    % end doy loop
        end 
 
% end yyyy loop
    end

 % end function
end