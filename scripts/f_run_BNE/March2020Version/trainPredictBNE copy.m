function [W] = runPredictBNE(YYYY,inputset,Kscale, fold)
%YYYY = '2015';
%inputset = 'AVGSCMJSCC';
%Kscale = 6.5
%KscaleStr = '6.5'
trainAqs = readtable(append('BNE_Inputs/c_03_parcels/annual/train_aqs_', YYYY, '_', foldMethod, '_', fold, '.csv'));
trainLatlon = readtable(append('BNE_Inputs/c_03_parcels/annual/train_latlon_', YYYY, '_', foldMethod, '_', fold, '.csv'));
trainPred = readtable(append('BNE_Inputs/c_03_parcels/annual/train_pred_', YYYY, '_', inputset, '_', foldMethod, '_', fold, '.csv'));

trainAqs = trainAqs{:,:};
trainLatlon = trainLatlon{:,:};
trainPred = trainPred{:,:};

[W,w0,SigW,Z,piZ] = BNE(trainAqs, trainLatlon, trainPred, 500, Kscale)


% A lot of this is specific to the data file we're working with

fid = fopen(append('BNE_Inputs/c_03_parcels/annual/Predictions_', YYYY, '_AVGSCMJSCC_1percent.csv'),'r'); % Creates id for reading in data line by line. A lot of the code below is 
line = str2num(fgetl(fid));

predCount = readtable(append('BNE_Inputs/c_03_parcels/annual/PredCount_', YYYY, '_AVGSCMJSCC_1percent.csv'));


% added things 
num_rand_feat = 500;
num_models = 5;
len_scale = Kscale;
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
            f = line([3, 4, 5, 6, 7]); %% Select 3 models and ignore SC
            f_all = [f_all ; f];
            Phi = sqrt(2/num_rand_feat)*cos(Z*X(end,:)'/len_scale + piZ);
            bias = Phi'*w0samp;
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


results = [X, softmax_mean, softmax_std,bias_mean, bias_std, y_mean, y_std];

writematrix(results, append('BNE_Outputs/annual/', YYYY, '_', inputset, '_', string(Kscale),'_', foldMethod, '_', fold, '.csv'))
