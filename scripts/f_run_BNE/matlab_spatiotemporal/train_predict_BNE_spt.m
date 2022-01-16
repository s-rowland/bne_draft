function [W] = train_predict_BNE_spt(inputset,len_scale_space,len_scale_time, fold, resid)
% % 
% % === Inputs ===
% %  
% %     inputset : (string) describes which input models are included;
% %          default is 'avgscmjscc' for the 5 main annual models
% %     len_scale : (scalar) RBF kernel parameter (we've been using 3.5, but not optimized)
% %     fold : (string) describes which prediction dataset to use 
% %          'all' is the full dataset; 'fold01' is the first fold, etc
% % 
% %  === Outputs ===
% % 
% %     a written matrix of the mean values of the model parameters and
% %     their standard deviation. For predicted concentration, additional values
% %     like the 2.5th and 97.5th percentiles are reported. 

%%%% ------------------------------ %%%%
%%%% 0: Set Up Objects for Test Run %%%%
%%%% ------------------------------ %%%%

 
%inputset = 'avgscmjscc';
 %len_scale_space = 3.5';
 %len_scale_time = 0.1;
 %fold = 'all';
 %num_rand_feat = 500;

%%%% ------------ %%%%
%%%% 1: Train BNE %%%%
%%%% ------------ %%%%

% 1a additional features that are consistent across model runs
num_rand_feat = 500;
%num_models = 5;

% 1b bring in training data
if strcmp(fold, 'NYS')
    trainFold = 'all'
elseif strcmp(fold, 'cities')
    trainFold = 'all'
elseif strcmp(fold, 'monitors')
    trainFold = 'all'
else 
    trainFold = fold
end

training = readtable(append('BNE_inputs/training_datasets/combined_annual/training_', inputset, '_', trainFold, '.csv'));

% 1c break down the training data into its components
% note that column 3 is date, which we aren't using yet
trainLatLon = training{:,1:2};
trainTime = training{:,3};
trainAqs = training{:,4};
trainPreds = training{:,5:9};
num_models = size(trainPreds,2);

% 1d train BNE
% note that the parameters will be estimated for those 500 random features,
% and we then use the information about the parameter values to get
% estimates at each point
% note also since this is a maximum a posteriori model, not MCMC or VI, we
% get the best-fit values and not whole distributions. Distributions are
% estimated in the prediction phase
[W,w0,SigW,Z,piZ, Zt] = BNE_spt(trainAqs,trainLatLon,trainTime, ...
    trainPreds,num_rand_feat,len_scale_space,len_scale_time, resid)

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

switch resid
    case 'noResid'
        w0samp = zeros(size(w0samp,1), size(w0samp, 2))
end

% str silenced
%temp = datenum('2016-01-01')-datenum('2000-01-01');

for time =  1:1 %temp:30:temp+365
    YYYY = 2009 + time;
    pred = readtable(append('BNE_inputs/prediction_datasets/individual_annual/predictions_', ...
     inputset, '_', num2str(YYYY), '_', fold, '.csv'));
 
    X = pred(:, [1,2]);
    f_all = pred(:, [4:8]);
 
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
     
    for i = 1:size(X,1)
        %Phi = sqrt(2/num_rand_feat)*cos(Z*X(i,:)'/len_scale_space + Zt*time/len_scale_time + piZ);
        
        Phi = sqrt(2/num_rand_feat)*cos(Z*X{i,:}'/len_scale_space + Zt*time/len_scale_time + piZ);
        bias = Phi'*w0samp;
        softmax = Phi'*wsamp;
        softmax = reshape(softmax',num_models,num_samp)';
        softmax = exp(softmax);
        softmax = softmax./repmat(sum(softmax,2),1,num_models);
        ens = softmax*f_all{i,:}';
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
    results = [X, array2table(softmax_mean), array2table(softmax_std), ...
        array2table(ens_mean), array2table(ens_std), ...
        array2table(bias_mean), array2table(bias_std), ...
        array2table(y_mean), array2table(y_std), ...
        array2table(y_95CIl), array2table(y_95CIu), ...
        array2table(y_68CIl), array2table(y_68CIu), ...
        array2table(y_min), array2table(y_max), ...
        array2table(y_median), array2table(y_skew), array2table(y_kurtosis)];

%lambda0txt = num2str(lambda0)
switch resid
    case 'resid'
        last = '.csv'
    case 'noResid'
        last = '_noResid.csv'
end
% 4b save as csv
    writetable(results, append('outputs/spatiotemp_annual/BNE_',...
        inputset, '_', string(len_scale_space),'_', string(len_scale_time),...
        '_',num2str(YYYY), '_', fold, last))

end