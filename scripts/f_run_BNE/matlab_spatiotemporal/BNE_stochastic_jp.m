function [W,w0,SigW,Z,piZ] = BNE_stochastic(y,X,time,models,num_rand_feat,len_scale_space,len_scale_time)
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

[num_obs,num_models] = size(models);
dimX = size(X,2);

W = zeros(num_rand_feat,num_models);
w0 = zeros(num_rand_feat,1);

Z = randn(num_rand_feat,dimX);
Zt = randn(num_rand_feat,1);
piZ = 2*pi*rand(num_rand_feat,1);

noise = var(y)/8; %% Set SNR to 8. This can be changed.
lambda = .1;
lambda0 = .1;
batch_size = 2000; %% Number of data points to randomly sample per model parameter update

err = 100;
% %  === OPTIMIZE W AND w0 ===
for iter = 1:20000
    step_size = 1/sqrt(batch_size);

    % Subsample batch_size number of points and construct "random"
    % features. (The randomness happens once at the beginning)
    [~,idx] = sort(rand(1,num_obs));
    idx = idx(1:batch_size);
    Phi = sqrt(2/num_rand_feat)*cos(Z*X(idx,:)'/len_scale_space + Zt*time(idx)'/len_scale_time + piZ*ones(1,batch_size));
    
    % Calculate stochastic gradient and update model GP vectors
    dotWPhi = W'*Phi;
    softmax = exp(dotWPhi);
    softmax = softmax./repmat(sum(softmax,1),num_models,1);
    model_avg = sum(softmax.*models(idx,:)',1);
    bias = w0'*Phi;
    error = y(idx)' - model_avg - bias;
    grad = Phi*((1/noise)*repmat(error,num_models,1).*(models(idx,:)' - repmat(model_avg,num_models,1)).*softmax)' - lambda*W;
    W = W + step_size*grad/sqrt(iter);

    % Calculate stochastic gradient and update bias vector
    dotWPhi = W'*Phi;
    softmax = exp(dotWPhi);
    softmax = softmax./repmat(sum(softmax,1),num_models,1);
    model_avg = sum(softmax.*models(idx,:)',1);
    residual = y(idx) - model_avg';
    w0tmp = inv(lambda0*noise*eye(num_rand_feat) + Phi*Phi')*(Phi*residual);
    w0 = w0tmp/sqrt(iter) + (1-1/sqrt(iter))*w0;
    
    % Display progress of algorithm
    error = y(idx)' - model_avg - bias;
    err = mean(error(:).^2)/10 + 9*err/10;
    display(['Iteration ' num2str(iter) ' ::: MSE ' num2str(err)]);
    if mod(iter,100) == 0
        subplot(4,1,1); stem(W(:,1));
        subplot(4,1,2); stem(W(:,2));
        subplot(4,1,3); stem(W(:,3));
        subplot(4,1,4); stem(w0);
        pause(.1)
    end
end


% % === CALCULATE THE COVARIANCE ===

bool_global_cov = 1; % If 1, this calculates cross correlations across model/bias vectors. If 0, it still calculates correlations within paramter vectors of each model & bias
SigW = zeros(num_rand_feat*(num_models+1));
for iter = 1:floor(num_obs/batch_size)-1
    Phi = sqrt(2/num_rand_feat)*cos(Z*X(iter*batch_size+1:iter*batch_size+batch_size,:)'/len_scale_space + Zt*time(iter*batch_size+1:iter*batch_size+batch_size)'/len_scale_time + piZ*ones(1,batch_size));
    dotWPhi = W'*Phi;
    softmax = exp(dotWPhi);
    softmax = softmax./repmat(sum(softmax,1),num_models,1);
    model_avg = sum(softmax.*models(iter*batch_size+1:iter*batch_size+batch_size,:)',1);
    bias = w0'*Phi;
    error = y(iter*batch_size+1:iter*batch_size+batch_size)' - model_avg - bias;
    t1 = -((models(iter*batch_size+1:iter*batch_size+batch_size,:)' - repmat(model_avg,num_models,1)).*softmax).^2;
    t2 = repmat(error,num_models,1).*(models(iter*batch_size+1:iter*batch_size+batch_size,:)' - repmat(model_avg,num_models,1)).*softmax.*(1-2*softmax);
    for i = 1:num_models
        for j = i:num_models
            if i == j
                SigW((i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat,(i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat) = ...
                    SigW((i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat,(i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat) ...
                        -lambda*eye(num_rand_feat)/floor(num_obs/batch_size) + (1/noise)*((repmat(t1(i,:) + t2(i,:),num_rand_feat,1).*Phi)*Phi');
            elseif bool_global_cov
                tij1 = -(models(iter*batch_size+1:iter*batch_size+batch_size,i)'-model_avg).*(models(iter*batch_size+1:iter*batch_size+batch_size,j)'-model_avg).*softmax(i,:).*softmax(j,:);
                tij2 = -error.*(models(iter*batch_size+1:iter*batch_size+batch_size,j)'-model_avg).*softmax(i,:).*softmax(j,:);
                tij3 = -error.*(models(iter*batch_size+1:iter*batch_size+batch_size,i)'-model_avg).*softmax(i,:).*softmax(j,:);
                SigW((i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat,(j-1)*num_rand_feat+1:(j-1)*num_rand_feat+num_rand_feat) = ...
                    SigW((i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat,(j-1)*num_rand_feat+1:(j-1)*num_rand_feat+num_rand_feat) + ...
                    (repmat((1/noise)*(tij1+tij2+tij3),num_rand_feat,1).*Phi)*Phi';
                SigW((j-1)*num_rand_feat+1:(j-1)*num_rand_feat+num_rand_feat,(i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat) = ...
                    SigW((j-1)*num_rand_feat+1:(j-1)*num_rand_feat+num_rand_feat,(i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat) + ...
                    (repmat((1/noise)*(tij1+tij2+tij3),num_rand_feat,1).*Phi)*Phi';
            end
        end
    end
    SigW(num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat,num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat) = ...
        SigW(num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat,num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat) ...
        -lambda*eye(num_rand_feat)/floor(num_obs/batch_size) - (1/noise)*Phi*Phi';
    if bool_global_cov
        for k = 1:num_models
           SigW(num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat,(k-1)*num_rand_feat+1:(k-1)*num_rand_feat+num_rand_feat) = ...
               SigW(num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat,(k-1)*num_rand_feat+1:(k-1)*num_rand_feat+num_rand_feat) ...
               -(1/noise)*(repmat((models(iter*batch_size+1:iter*batch_size+batch_size,k)'-model_avg).*softmax(k,:),num_rand_feat,1).*Phi)*Phi';
           SigW((k-1)*num_rand_feat+1:(k-1)*num_rand_feat+num_rand_feat,num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat) = ...
               SigW((k-1)*num_rand_feat+1:(k-1)*num_rand_feat+num_rand_feat,num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat) ...
               -(1/noise)*(repmat((models(iter*batch_size+1:iter*batch_size+batch_size,k)'-model_avg).*softmax(k,:),num_rand_feat,1).*Phi)*Phi';
        end
    end
%     imagesc(inv(-(SigW+SigW')/2)); colorbar; axis image; pause(.1)
    iter/floor(num_obs/batch_size)
end
SigW = (SigW + SigW')/2;
SigW = inv(-SigW);
SigW = (SigW + SigW')/2;
[Q,L] = eig(SigW);
L(L < 0) = 0;
SigW = Q*L*Q';
