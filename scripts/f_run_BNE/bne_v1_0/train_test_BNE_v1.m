function [partMSE] = train_predict_BNE_v1(window, num_models, fold, ...
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
training_full = readtable(append('inputs/pm25/training_datasets/',window, '_combined/training_cvfolds.csv'));
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

partMSE = [0, 0]

% 1 loop over the years
for YYYY = [2005, 2006, 2007, 2009, 2010, 2011, 2013, 2014, 2015]
    
    % 1a determine the maximum number of days the in current year
    if YYYY == 2004 | YYYY == 2008 | YYYY == 2012 | YYYY == 2016
        maxDoY = 366
    else maxDoY = 365
    end
        
    % nasty hack to deal with missing leap years
    YYYY2 = YYYY
    if YYYY == 2009 | YYYY == 2010 | YYYY == 2011
        YYYY2 = YYYY-1
    elseif YYYY == 2013 | YYYY == 2014 | YYYY == 2015 
        YYYY2 = YYYY -2 
    end
        
    % 1b loop over days within that year
    for DoY =  1:maxDoY 
        
        if strcmp(time_metric, 'annual')
            time = time;
        elseif strcmp(time_metric, 'dayOfYear') 
            time = DoY;
        elseif strcmp(time_metric, 'julianDay') 
            time = DoY + floor(365.25 * (YYYY2-2005));
        end
        
        % extract preds for this time
        testing_day = testing(testing{:,3} ==time,:); 
        
        % extract components
        X = testing_day(:, [1,2]);
        f_all = testing_day(:, [5:(4+num_models)]);
        obs = testing_day.obs;
        % extract the predictions

        % generate the empty arrays to fill
        y_mean = zeros(size(X,1),1);

        % loop over the individual points
        for i = 1:size(X,1)
            if strcmp(time_metric, 'dayOfYear') 
                Phi = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space + Zt*58.0916*[cos(2*pi*time/maxDoY)' ; sin(2*pi*time/maxDoY)']/len_scale_time + piZ);
                Phi_bias = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space_bias + Zt*58.0916*[cos(2*pi*time/maxDoY)' ; sin(2*pi*time/maxDoY)']/len_scale_time_bias + piZ);
            else
                Phi = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space + Zt*time'/len_scale_time + piZ);
                Phi_bias = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space_bias + Zt*time'/len_scale_time_bias + piZ);
            end
            %Phi = sqrt(2/num_rand_feat)*cos(Z*X(i,:)'/len_scale_space + Zt*time/len_scale_time + piZ);
            % Phi for weights
            %Phi = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space + Zt*time/len_scale_time + piZ);
            %Phi = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space + Zt*time/len_scale_time + piZ);
            % Phi for bias 
            %Phi_bias = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space_bias + Zt*time/len_scale_time_bias + piZ);        
            bias = Phi_bias'*w0samp;
            softmax = Phi'*wsamp;
            softmax = reshape(softmax',num_models,num_samp)';
            softmax = exp(softmax);
            softmax = softmax./repmat(sum(softmax,2),1,num_models);
            ens = softmax*f_all{i,:}';
            y = softmax*f_all{i,:}' + bias';
            y_mean(i) = mean(y);

            %if mod(i,1000) == 0
             %   disp([num2str(time) '/' num2str(temp+365) ' ::: ' num2str(i/size(X,1))]);
            %end
        end
        
    %%%% ----------------------- %%%%
    %%%% 4: Generate Predictions %%%%
    %%%% ----------------------- %%%%

        error_day = y_mean - testing_day.obs;

        mse_day = mean(error_day.^2);
        
        if size(testing_day,1) == 0
            mse_day =0;
        end
        
        partMSE = [partMSE mse_day * size(X,1) / size(training_full,1)];
        

        end
    % 4b save as csv
    end
  partMSE = sum(partMSE)  
end