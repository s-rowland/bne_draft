Xall = readtable(append('BNE_Inputs/c_04_parcels_temporal/annual/Predictions_', '_AVGSCMJSCCM14M20CANNBGRD_1percent.csv'));

X = Xall(:,1:2);
f_all = Xall(:,3:5); % this line needs to change to accomodate num of inputs
num_models = size(f_all,2);
num_rand_feat = size(Z,1);

muW = [W(:) ; w0];
num_samp = 250;
wsamp1 = mvnrnd(muW,SigW,num_samp)';
w0samp = [];
wsamp = [];
for s = 1:num_samp
    w0samp = [w0samp wsamp1(end-num_rand_feat+1:end,s)];
    wsamp = [wsamp reshape(wsamp1(1:num_models*num_rand_feat,s),num_rand_feat,num_models)];
end

temp = datenum('2016-01-01')-datenum('2000-01-01');
% temp is just used to determine the number of days to go through 
% temp is a scalar
% as we start the day, we initialize some empty (zero) matrices of the
% right dimensions
for time = temp:30:temp+365 % this is a loop over the days 
    % we go from temp to temp + 365 (one year), in increments of 30 
    % so this is just 12 slices from 1 year. 
    % okay 
    y_mean = zeros(size(X,1),1);
    y_std = zeros(size(X,1),1);
    softmax_mean = zeros(size(X,1),num_models);
    softmax_std = zeros(size(X,1),num_models);
    bias_mean = zeros(size(X,1),1);
    bias_std = zeros(size(X,1),1);
    for i = 1:size(X,1) % this is a loop over all the latlon points of a single day 
        % so here the zt gets mulitplied by time so that we get the right
        % weights for location+time combo
        % remember X is location. 
        Phi = sqrt(2/num_rand_feat)*cos(Z*X(i,:)'/len_scale_space + Zt*time/len_scale_time + piZ);
        bias = Phi'*w0samp;
        softmax = Phi'*wsamp;
        softmax = reshape(softmax',num_models,num_samp)';
        softmax = exp(softmax);
        softmax = softmax./repmat(sum(softmax,2),1,num_models);
        y = softmax*f_all(i,:)' + bias';
        y_mean(i) = mean(y);
        y_std(i) = std(y);
        bias_mean(i) = mean(bias);
        bias_std(i) = std(bias);
        softmax_mean(i,:) = mean(softmax,1);
        softmax_std(i,:) = std(softmax,1);
        if mod(i,1000) == 0
            disp([num2str(time) '/' num2str(temp+365) ' ::: ' num2str(i/size(X,1))]);
        end
    end
    save(['2016_' num2str(time) '.mat'],'y_mean','y_std','bias_mean','bias_std','softmax_mean','softmax_std','X','f_all');
end