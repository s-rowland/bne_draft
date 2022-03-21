function [W] = train_predict_BNE_v1(window, num_models, trainFold, predFold, ...
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

 % window = 'daily'; num_models = 5; trainFold = 'fold01'; predFold = 'fold01'; 
 % len_scale_space = 3.5'; len_scale_time = 20; len_scale_space_bias = 3.5';
 % len_scale_time_bias = 20; penalty = 0.1; time_metric = 'dayOfYear'; seed = 1234;
 % yyyy_start = 2005; yyyy_end=2005; dir_out = 'test_run';
 
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
% 1b.ii drop leave-out observations if we are doing gridsearch
foldName = extractBetween(trainFold, 1, 4);
if [foldName{:}] ==  'fold'
    foldNum = str2num(cell2mat(extractBetween(trainFold, 5, 6)))
    training = training_full(training_full.fold ~=foldNum,:);
else training = training_full;
end

% 1c break down the training data into its components
% note that column 3 is date, which we aren't using yet
trainLatLon = training{:,1:2};
if strcmp(window, 'annual')
    trainTime = training{:,3};
    trainAqs = training{:,4};
    trainPreds = training{:,5:(4+num_models)};
elseif strcmp(time_metric, 'dayOfYear')
    trainTime = training{:,3};
    trainAqs = training{:,5};
    trainPreds = training{:,6:(5+num_models)};
elseif strcmp(time_metric, 'julianDay')
    trainTime = training{:,4};
    trainAqs = training{:,5};
    trainPreds = training{:,6:(5+num_models)};
end

% 1d train BNE
% note that the parameters will be estimated for those 500 random features,
% and we then use the information about the parameter values to get
% estimates at each point
% note also since this is a maximum a posteriori model, not MCMC or VI, we
% get the best-fit values and not whole distributions. Distributions are
% estimated in the prediction phase

[W,w0,SigW,Z,piZ,Zt,MSE] = BNE_v1_0(trainAqs, trainLatLon, trainTime, ...
    trainPreds, num_rand_feat,len_scale_space,len_scale_time, ...
    len_scale_space_bias,len_scale_time_bias, penalty);

%%%% -------------------------- %%%%
%%%% 2: Prepare for Predictions %%%%
%%%% -------------------------- %%%%

% get the number of random features
% this is not required when we use the function version
%num_rand_feat = size(Z,1);

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

% 1 loop over the years
for YYYY = yyyy_start:yyyy_end
    
    % 1a determine the maximum number of days the in current year
    if YYYY == 2004 | YYYY == 2008 | YYYY == 2012 | YYYY == 2016
        maxDoY = 366
    else maxDoY = 365
    end
        
    % 1b loop over days within that year
    for DoY =  1:2 %maxDoY 
        
        % bring in the prediction dataset 
        predName = extractBetween(predFold, 1, 4);
        if strcmp(predFold,'refGridConus')
            pred = readtable(append('inputs/pm25/prediction_datasets/',window, ...
            '_individual/preds_2005_', pad(num2str(DoY), 3, 'left', '0'), '.csv'));
        elseif strcmp(predFold, 'externalValidation') 
            pred = readtable(append('external_validation_data/pm25/formatted/external_', ...
                window, '.csv'));
        elseif [predName{:}] ==  'fold'
            foldNum = str2num(cell2mat(extractBetween(trainFold, 5, 6)))
            pred = training_full(training_full.fold ==foldNum, :);
            pred(5,:) = []
        end

        if strcmp(time_metric, 'annual')
            time = time;
        elseif strcmp(time_metric, 'dayOfYear') 
            time = 100*DoY / maxDoY;
        elseif strcmp(time_metric, 'julianDay') 
            time = DoY + floor(365.25 * (YYYY-2005));
        end
            
    % randomly select preds 
        %randrows = randperm(size(pred, 1));
        %pred = pred(randrows(1:size(pred, 1)/20), :);
        
        % extract components
        X = pred(:, [1,2]);
        f_all = pred(:, [4:8]);
        % extract the predictions
        if strcmp(window, 'annual')
            f_all = training{:,4:(3+num_models)};
        elseif strcmp(time_metric, 'dayOfYear')
            f_all = training{:,5:(4+num_models)};
        elseif strcmp(time_metric, 'julianDay')
            f_all = training{:,5:(4+num_models)};
        end

        % generate the empty arrays to fill
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
        y_68CIl = zeros(size(X,1),1);
        y_68CIu = zeros(size(X,1),1);
        y_min = zeros(size(X,1),1);
        y_max = zeros(size(X,1),1);
        y_median = zeros(size(X,1),1);
        y_skew = zeros(size(X,1),1);    
        y_kurtosis = zeros(size(X,1),1);

        % loop over the individual points
        for i = 1:size(X,1)
            %Phi = sqrt(2/num_rand_feat)*cos(Z*X(i,:)'/len_scale_space + Zt*time/len_scale_time + piZ);
            % Phi for weights
            Phi = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space + Zt*time/len_scale_time + piZ);
            % Phi for bias 
            Phi_bias = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space_bias + Zt*time/len_scale_time_bias + piZ);        
            bias = Phi_bias'*w0samp;
            softmax = Phi'*wsamp;
            softmax = reshape(softmax',num_models,num_samp)';
            softmax = exp(softmax);
            softmax = softmax./repmat(sum(softmax,2),1,num_models);
            ens = softmax*f_all(i,:)';
            y = softmax*f_all(i,:)' + bias';
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
            y_68CIl(i) = quantile(y, 0.16);
            y_68CIu(i) = quantile(y, 0.84);
            y_min(i) = min(y);
            y_max(i) = max(y);
            y_median(i) = median(y);
            y_skew(i) = skewness(y, 0); % sample skew
            y_kurtosis(i) = kurtosis(y, 0); % sample kurtosis

            %if mod(i,1000) == 0
             %   disp([num2str(time) '/' num2str(temp+365) ' ::: ' num2str(i/size(X,1))]);
            %end
        end
        %%%% ----------------------- %%%%
    %%%% 4: Generate Predictions %%%%
    %%%% ----------------------- %%%%

    % 4a combine summary metrics into a dataframe

        lat = X{:,1};
        lon = X{:,2};
        if num_models == 5
            w_mean_av = softmax_mean(:,1);
            w_mean_cm = softmax_mean(:,2);
            w_mean_js = softmax_mean(:,3);
            w_mean_me = softmax_mean(:,4);
            w_mean_rk = softmax_mean(:,5);
            w_std_av = softmax_std(:,1);
            w_std_cm = softmax_std(:,2);
            w_std_js = softmax_std(:,3);
            w_std_me = softmax_std(:,4);
            w_std_rk = softmax_std(:,5);
            wtable = table(w_mean_av, w_mean_cm, ...
                 w_mean_js, w_mean_me, w_mean_rk, ...
                w_std_av, w_std_cm, ...
                w_std_js, w_std_me, w_std_rk);
        elseif num_models == 6
            w_mean_av = softmax_mean(:,1);
            w_mean_cb = softmax_mean(:,2);
            w_mean_cm = softmax_mean(:,3);
            w_mean_js = softmax_mean(:,4);
            w_mean_me = softmax_mean(:,5);
            w_mean_rk = softmax_mean(:,6);
            w_std_av = softmax_std(:,1);
            w_std_cb = softmax_std(:,2);
            w_std_cm = softmax_std(:,3);
            w_std_js = softmax_std(:,4);
            w_std_me = softmax_std(:,5);
            w_std_rk = softmax_std(:,6);
            wtable = table(w_mean_av, w_mean_cb, w_mean_cm, ...
                 w_mean_js, w_mean_me, w_mean_rk, ...
                w_std_av, w_std_cb, w_std_cm, ...
                w_std_js, w_std_me, w_std_rk);
        elseif num_models == 7
            w_mean_av = softmax_mean(:,1);
            w_mean_cc = softmax_mean(:,2);
            w_mean_cm = softmax_mean(:,3);
            w_mean_gs = softmax_mean(:,4);
            w_mean_js = softmax_mean(:,5);
            w_mean_me = softmax_mean(:,6);
            w_mean_rk = softmax_mean(:,7);
            w_std_av = softmax_std(:,1);
            w_std_cc = softmax_std(:,2);
            w_std_cm = softmax_std(:,3);
            w_std_gs = softmax_std(:,4);
            w_std_js = softmax_std(:,5);
            w_std_me = softmax_std(:,6);
            w_std_rk = softmax_std(:,7);
            wtable = table(w_mean_av,  w_mean_cc, w_mean_cm, ...
                w_mean_gs, w_mean_js, w_mean_me, w_mean_rk, ...
                w_std_av,  w_std_cc, w_std_cm, ...
                w_std_gs, w_std_js, w_std_me, w_std_rk);
        elseif num_models == 8
            w_mean_av = softmax_mean(:,1);
            w_mean_cb = softmax_mean(:,2);
            w_mean_cc = softmax_mean(:,4);
            w_mean_cm = softmax_mean(:,5);
            w_mean_gs = softmax_mean(:,6);
            w_mean_js = softmax_mean(:,7);
            w_mean_me = softmax_mean(:,8);
            w_mean_rk = softmax_mean(:,9);
            w_std_av = softmax_std(:,1);
            w_std_cb = softmax_std(:,2);
            w_std_cc = softmax_std(:,3);
            w_std_cm = softmax_std(:,4);
            w_std_gs = softmax_std(:,5);
            w_std_js = softmax_std(:,6);
            w_std_me = softmax_std(:,7);
            w_std_rk = softmax_std(:,8);
            wtable = table(w_mean_av, w_mean_cb, w_mean_cc, w_mean_cm, ...
                w_mean_gs, w_mean_js, w_mean_me, w_mean_rk, ...
                w_std_av, w_std_cb, w_std_cc, w_std_cm, ...
                w_std_gs, w_std_js, w_std_me, w_std_rk);
        end
        % compile
        results = [table(lat, lon), ...
            wtable, ...
            table(ens_mean, ens_std, bias_mean, bias_std, y_mean, y_std, ...
            y_95CIl, y_95CIu, y_68CIl, y_68CIu)];

    % 4b save as csv
        writetable(results, append('outputs/pm25/', dir_out, ...
            '/bne_', predFold, '_', num2str(YYYY), '_', num2str(DoY),...
            num2str(len_scale_space), '_', num2str(len_scale_time), '_', ...
            num2str(len_scale_space_bias), ...
            num2str(len_scale_time_bias), '_', ...
        num2str(seed), '.csv'))

    end
end