function [W,w0,Z,piZ,Zt,MSE] = BNE_v1_0_nosigw(y,X,time,models,num_rand_feat,...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric)
% % Implements a stochastic optimization (MAP inference) version of BNE.
% %
% % === Inputs ===
% % 
% %     y : vector or measurements, N x 1 
% %     X : geographic locations for measurements in y, N x 2 (we use lat-long)
% %     time : time stamp for measurements in y, N x 1 ( we use 1 unit = 1 day)
% %     models : model predictions for each measurement, N x (number of models)
% %     num_rand_feat : dimensionality of random features (we've found 500 to be plenty)
% %     len_scale_space : RBF spatial kernel parameter (we've been using 6.5, but not optimized)
% %     len_scale_time : RBF temporal kernel parameter (we've been using 17.5, but not optimized)
% %     bool_periodic : indicates that the time-varying portion of the kernel should repeat each year
% % 
% %  === Outputs ===
% % 
% %     W : num_rand_feat x num_models parameters for each model
% %     w0 : num_rand_feat x 1 parameters for bias term
% %     SigW : Covariance of W and w0 for Gaussian approximation. 
% %             Comment: This is for all parameters in W and w0 combined by vectorizing
% %             the columns of W first and then appending w0 to the end, so it is large.
% %             I check to see if it is positive semidefinite as required. I've noticed sometimes 
% %             it isn't, which is strange and I need to look more into it. If not, I
% %             currently set any negative eigenvalues to zero and recalculate.
% %     Z & piZ : The random variables used to calculate the random
% %                features (Phi in code). Need to use the same ones for prediction.
% models = trainPreds; y = trainAqs; X = trainLatLon; time = trainTime;

[num_obs,num_models] = size(models);
dimX = size(X,2);

W = zeros(num_rand_feat,num_models);
w0 = zeros(num_rand_feat,1);

Z = randn(num_rand_feat,dimX);
if strcmp(time_metric, 'dayOfYear')
    Zt = randn(num_rand_feat,2); % 2D time for year invariance, but seasonal variation
else
    Zt = randn(num_rand_feat,1); % One dimensional time
end
piZ = 2*pi*rand(num_rand_feat,1);

noise = var(y)/8; %% Set SNR to 8. This can be changed.
lambda = penalty;
lambda0 = penalty;
batch_size = 2000; %% Number of data points to randomly sample per model parameter update

err = 100;
MSE = 0;
% %  === OPTIMIZE W AND w0 ===
for iter = 1: 1000

    % Subsample batch_size number of points and construct "random"
    % features. (The randomness happens once at the beginning)
    [~,idx] = sort(rand(1,num_obs));
    idx = idx(1:batch_size);
    if strcmp(time_metric, 'dayOfYear')
        Phi = sqrt(2/num_rand_feat)*cos(Z*X(idx,:)'/len_scale_space + Zt*58.0916*[cos(2*pi*time(idx))' ; sin(2*pi*time(idx))']/len_scale_time + piZ*ones(1,batch_size));
        Phi_bias = sqrt(2/num_rand_feat)*cos(Z*X(idx,:)'/len_scale_space_bias + Zt*58.0916*[cos(2*pi*time(idx))' ; sin(2*pi*time(idx))']/len_scale_time_bias + piZ*ones(1,batch_size));
    else
        Phi = sqrt(2/num_rand_feat)*cos(Z*X(idx,:)'/len_scale_space + Zt*time(idx)'/len_scale_time + piZ*ones(1,batch_size));
        Phi_bias = sqrt(2/num_rand_feat)*cos(Z*X(idx,:)'/len_scale_space_bias + Zt*time(idx)'/len_scale_time_bias + piZ*ones(1,batch_size));
    end

    % Calculate stochastic gradient and update model GP vectors
    dotWPhi = W'*Phi;
    softmax = exp(dotWPhi);
    softmax = softmax./repmat(sum(softmax,1),num_models,1);
    model_avg = sum(softmax.*models(idx,:)',1);
    bias = w0'*Phi_bias;
    error = y(idx)' - model_avg - bias;
    grad = Phi*((1/noise)*repmat(error,num_models,1).*(models(idx,:)' - repmat(model_avg,num_models,1)).*softmax)' - lambda*W;
    W = W + grad/sqrt(iter);

    % Calculate stochastic gradient and update bias vector
    dotWPhi = W'*Phi;
    softmax = exp(dotWPhi);
    softmax = softmax./repmat(sum(softmax,1),num_models,1);
    model_avg = sum(softmax.*models(idx,:)',1);
    residual = y(idx) - model_avg';
    w0tmp = inv(lambda0*noise*eye(num_rand_feat) + Phi_bias*Phi_bias')*(Phi_bias*residual);
    w0 = w0tmp/sqrt(iter) + (1-1/sqrt(iter))*w0;
    
    % Display progress of algorithm
    error = y(idx)' - model_avg - bias;
    MSE = (iter-1)*MSE/iter + mean(error(:).^2)/iter;  % Roughly approximates the training MSE
    if mod(iter, 10)==0
        display(['Iteration ' num2str(iter) ' ::: MSE ' num2str(MSE)]);
    end
end
end
