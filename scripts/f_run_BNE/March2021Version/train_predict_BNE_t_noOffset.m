function [W] = train_predict_BNE_t_noOffset(YYYY,inputset,len_scale, fold)
% % 
% % === Inputs ===
% % 
% %     YYYY : (scalar) the year for which we are training and predicting 
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

% YYYY = '2010';
% inputset = 'avgscmjscc';
% len_scale = 3.5';
% fold = 'fold01';

%%%% ------------ %%%%
%%%% 1: Train BNE %%%%
%%%% ------------ %%%%

% 1a additional features that are consistent across model runs
num_rand_feat = 500;
num_models = 5;

% 1b bring in training data
if strcmp(fold, 'cities')
    trainFold = 'all';
else 
    trainFold = fold;
end
training = readtable(append('BNE_inputs/training_datasets/individual_annual/training_', inputset, '_', YYYY, '_', trainFold, '.csv'));

% 1c break down the training data into its components
% note that column 3 is date, which we aren't using yet
trainLatlon = training{:,1:2};
trainAqs = training{:,4};
trainPred = training{:,5:9};

% 1d train BNE
% note that the parameters will be estimated for those 500 random features,
% and we then use the information about the parameter values to get
% estimates at each point
% note also since this is a maximum a posteriori model, not MCMC or VI, we
% get the best-fit values and not whole distributions. Distributions are
% estimated in the prediction phase
[W,w0,SigW,Z,piZ] = BNE_t_noOffset(trainAqs, trainLatlon, trainPred, num_rand_feat, len_scale)

%%%% -------------------------- %%%%
%%%% 2: Prepare for Predictions %%%%
%%%% -------------------------- %%%%

% 2a create an id for reading in data line by line.
fid = fopen(append('BNE_inputs/prediction_datasets/individual_annual/predictions_', ...
     inputset, '_', YYYY, '_', fold, '.csv'),'r');
 
% 2b makes an object we can use from the fid
line = str2num(fgetl(fid));

% 2c get the number of observations in the prediction dataset
predCount = readtable(append('BNE_inputs/prediction_datasets/individual_annual/predCount_',...
     inputset, '_', YYYY, '_', fold, '.csv'));

% 2d set seed 
rng(20);

% 2e create num_samp samples of W and w0 from a Gaussian distribution used
% for calculating empirical mean and standard deviations. More samples is
% slower but more accurate.
% Basically, we take samples of the parameter values 

% 2e.i  muW is a vector of the mean values of the parameters of weights and
% offset term 
muW = [W(:) ; w0];
% 2e.ii set the number of samples that we will take 
num_samp = 250; %variances % I silenced 'variances'
% 2e.iii take the samples. we generate random numbers based on gaussian
% distributions, where the means are the mean values and the variances are 
% stored in SigW, which we calculate in the second half of the BNE function
wsamp1 = mvnrnd(muW,SigW,num_samp)';
% 2e.iv put samples in tidy format
w0samp = [];
wsamp = [];
for s = 1:num_samp
    w0samp = [w0samp wsamp1(end-num_rand_feat+1:end,s)];
    wsamp = [wsamp reshape(wsamp1(1:num_models*num_rand_feat,s),num_rand_feat,num_models)];
end

% 2f create empty vectors of the PPD-summary characteristics that we are
% interested in 
X = [];
f_all = [];
softmax_mean = [];
softmax_std = [];
bias_mean = [];
bias_std = [];
y_mean = [];
y_std = [];
y_95CIl = [];
y_95CIu = [];
y_min = [];
y_max = [];
y_median = [];

%%%% ----------------------- %%%%
%%%% 3: Generate Predictions %%%%
%%%% ----------------------- %%%%

% 3a I don't know what this line does
start_plot = 0;
% 3b loop over the points in the prediction dataset
for i = 1:predCount{1,1} 
    % 3c pull in a point 
    line = fgetl(fid); 
    % 3d wasrandomly select 1% of points - no longer needed
    if rand < 1 
        % 3e convert point to numeric
        line = str2num(line);
        % another unncessary conditional statement, but I don't want to
        % remove it and potentially mess up the spacing
        if 2010 == 2010 % Define the year to predict for. Currently very slow for more current years.
            % not sure what this line does 
            start_plot = 1;
            % 3g define the location (lat and lon)
            X = [X ; [line(1) line(2)]];
            % 3h define the input model predictions
            f = line([4, 5, 6, 7, 8]); %% Select 5 main models
            f_all = [f_all ; f];
 
            % 3i translate the points' location from lat-lon to random
            % features
            Phi = sqrt(2/num_rand_feat)*cos(Z*X(end,:)'/len_scale + piZ);
            % 3j create the bias term... 250 samples
            bias = zeros(1, 250);
            % 3k calculate the weights 
            % and rescale them to sum to one
            softmax = Phi'*wsamp;
            softmax = reshape(softmax',num_models,num_samp)';
            softmax = exp(softmax);
            softmax = softmax./repmat(sum(softmax,2),1,num_models);
            % 3l vector of y by taking weighted mean of models and the
            % bias. 
            y = softmax*f' + bias';
            % 3m summary statistics of parameters
            bias_mean = [bias_mean ; mean(bias)];
            bias_std = [bias_std ; std(bias)];
            softmax_mean = [softmax_mean ; mean(softmax,1)];
            softmax_std = [softmax_std ; std(softmax,1)];
            y_mean = [y_mean ; mean(y)];
            y_std = [y_std ; std(y)];
            y_95CIl = [y_95CIl ; quantile(y, 0.025)];
            y_95CIu = [y_95CIu ; quantile(y, 0.975)];
            y_min = [y_min ; min(y)];
            y_max = [y_max ; max(y)];
            y_median = [y_median ; median(y)];

        end
    end
%     % Below is just for visualization purposes to get a sense of how much
%     more time is needed.
%     if mod(i,10000) == 0
%         if start_plot
%             scatter(X(:,2),X(:,1),[],softmax_mean(:,1),'filled');
%             colorbar
%             axis equal
%             title([num2str(i) ' ::: ' num2str(length(y_mean))]);
%             pause(.1);
%         else
%             [num2str(i) ' ::: ' line]
%         end
%     end
end
% 3n finish with that id object
fclose(fid);

%%%% ----------------------- %%%%
%%%% 4: Generate Predictions %%%%
%%%% ----------------------- %%%%

% 4a combine summary metrics into a dataframe
results = [X, softmax_mean, softmax_std,bias_mean, bias_std, y_mean, y_std, y_95CIl, y_95CIu, y_min, y_max, y_median];

% 4b save as csv
writematrix(results, append('BNE_outputs/temp_annual_noOffset/BNE_',...
    inputset, '_', string(len_scale),'_', YYYY, '_', fold, '.csv'))
