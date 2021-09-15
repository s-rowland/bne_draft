function [W] = runPredictBNEnoOffset(YYYY,inputset,len_scale, fold)
%YYYY = '2015';
%inputset = 'avgscmjscc';
%len_scale = 6.5';
%fold = 'all';

training = readtable(append('BNE_inputs/training_data/combined/Training_annual_', YYYY, '_avgscmjscc_', fold, '.csv'));

trainAqs = training{:,4};
trainLatlon = training{:,1:2};
trainPred = training{:,5:9};

[W,w0,SigW,Z,piZ] = BNE_noOffset(trainAqs, trainLatlon, trainPred, 500, len_scale)


% Initialize the file of predictions we will walk through
fid = fopen(append('BNE_inputs/input_models/combined/annual/Predictions_', YYYY, '_avgscmjscc_', fold, '.csv'),'r'); % Creates id for reading in data line by line. A lot of the code below is 
line = str2num(fgetl(fid));

% determine the number of predictions 
% used to end the loop
predCount = readtable(append('BNE_inputs/input_models/combined/annual/PredCount_', YYYY, '_avgscmjscc_', fold, '.csv'));

% additional features for model
num_rand_feat = 500;
num_models = size(trainPred,2);
% set seed 
rng(20);

% Creates num_samp samples of W and w0 from a Gaussian distribution used
% for calculating empirical mean and standard deviations. More samples is
% slower but more accurate.
muW = [W(:) ; w0];
num_samp = 250;%variances % I silenced this
wsamp1 = mvnrnd(muW,SigW,num_samp)';
w0samp = [];
wsamp = [];

for s = 1:num_samp
    w0samp = [w0samp wsamp1(end-num_rand_feat+1:end,s)];
    wsamp = [wsamp reshape(wsamp1(1:num_models*num_rand_feat,s),num_rand_feat,num_models)];
end

X = [];
f_all = [];
y_mean = [];
y_std = [];
y_05CI = [];
y_95CI = [];
y_min = [];
y_max = [];
y_median = [];
softmax_mean = [];
softmax_std = [];
bias_mean = [];
bias_std = [];
year = 2010;

start_plot = 0;
for i = 1:predCount{1,1} 
    line = fgetl(fid);
    if rand < 1 % keep all predicted - we random selected earlier
        line = str2num(line);
        if 2010 == 2010 % Define the year to predict for. Currently very slow for more current years.
            start_plot = 1;
            X = [X ; [line(2) line(1)]];
            f = line([4, 5, 6, 7, 8]); %% Select 5 main models
            f_all = [f_all ; f];
            Phi = sqrt(2/num_rand_feat)*cos(Z*X(end,:)'/len_scale + piZ);
            bias = 0;
            softmax = Phi'*wsamp;
            softmax = reshape(softmax',num_models,num_samp)';
            softmax = exp(softmax);
            softmax = softmax./repmat(sum(softmax,2),1,num_models);
            y = softmax*f' + bias';
            y_mean = [y_mean ; mean(y)];
            y_std = [y_std ; std(y)];
            bias_mean = [bias_mean ; mean(bias)];
            bias_std = [bias_std ; std(bias)];
            softmax_mean = [softmax_mean ; mean(softmax,1)];
            softmax_std = [softmax_std ; std(softmax,1)];
            y_05CI = [y_05CI ; quantile(y, 0.05)];
            y_95CI = [y_95CI ; quantile(y, 0.95)];
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
fclose(fid);


results = [X, softmax_mean, softmax_std,bias_mean, bias_std, y_mean, y_std, y_05CI, y_95CI, y_min, y_max, y_median];

writematrix(results, append('BNE_outputs/noOffset/', YYYY, '_avgscmjscc_', string(len_scale),'_',  fold, '.csv'))
