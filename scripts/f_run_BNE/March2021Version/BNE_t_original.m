function [W,w0,SigW,Z,piZ] = BNE_t_original(y,X,models,num_rand_feat,len_scale)
% % 
% % === Inputs ===
% % 
% %     y : vector or measurements, N x 1 
% %     X : geographic locations for measurements in y, N x 2
% %     models : model predictions for each measurement, N x (number of models)
% %     num_rand_feat : dimensionality of random features (we've found 500 to be plenty)
% %     len_scale : RBF kernel parameter (we've been using 6.5, but not optimized)
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
piZ = 2*pi*rand(num_rand_feat,1);

Phi = sqrt(2/num_rand_feat)*cos(Z*X'/len_scale + piZ*ones(1,num_obs));

noise = var(y)/8; %% Set SNR to 8. This can be changed.
lambda = .1;
lambda0 = .1;

% %  === OPTIMIZE W AND w0 ===
for iter = 1:1000
    step_size = .1/sqrt(num_obs);
    
    for step = 1:5
        dotWPhi = W'*Phi;
        softmax = exp(dotWPhi);
        softmax = softmax./repmat(sum(softmax,1),num_models,1);
        model_avg = sum(softmax.*models',1);
        bias = w0'*Phi;
        error = y' - model_avg - bias;
        grad = Phi*((1/noise)*repmat(error,num_models,1).*(models' - repmat(model_avg,num_models,1)).*softmax)' - lambda*W;
        W = W + step_size*grad;
    end
    dotWPhi = W'*Phi;
    softmax = exp(dotWPhi);
    softmax = softmax./repmat(sum(softmax,1),num_models,1);
    model_avg = sum(softmax.*models',1);
    residual = y - model_avg';
    w0 = inv(lambda0*noise*eye(num_rand_feat) + Phi*Phi')*(Phi*residual);
    error = y' - model_avg - bias;
%     if iter > 100
%         noise = mean(error(:).^2);
%     end
    display(['Iteration ' num2str(iter) ' ::: MSE ' num2str(mean(error(:).^2))]);
end


% % === CALCULATE THE COVARIANCE ===

bool_global_cov = 1;
dotWPhi = W'*Phi;
softmax = exp(dotWPhi);
softmax = softmax./repmat(sum(softmax,1),num_models,1);
model_avg = sum(softmax.*models',1);
bias = w0'*Phi;
error = y' - model_avg - bias;
SigW = zeros(num_rand_feat*(num_models+1));
t1 = -((models' - repmat(model_avg,num_models,1)).*softmax).^2;
t2 = repmat(error,num_models,1).*(models' - repmat(model_avg,num_models,1)).*softmax.*(1-2*softmax);
for i = 1:num_models
    for j = i:num_models
        if i == j
            SigW((i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat,(i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat) = ...
                    -lambda*eye(num_rand_feat) + (1/noise)*((repmat(t1(i,:) + t2(i,:),num_rand_feat,1).*Phi)*Phi');
        elseif bool_global_cov
            tij1 = -(models(:,i)'-model_avg).*(models(:,j)'-model_avg).*softmax(i,:).*softmax(j,:);
            tij2 = -error.*(models(:,j)'-model_avg).*softmax(i,:).*softmax(j,:);
            tij3 = -error.*(models(:,i)'-model_avg).*softmax(i,:).*softmax(j,:);
            SigW((i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat,(j-1)*num_rand_feat+1:(j-1)*num_rand_feat+num_rand_feat) = ...
                (repmat((1/noise)*(tij1+tij2+tij3),num_rand_feat,1).*Phi)*Phi';
            SigW((j-1)*num_rand_feat+1:(j-1)*num_rand_feat+num_rand_feat,(i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat) = ...
                (repmat((1/noise)*(tij1+tij2+tij3),num_rand_feat,1).*Phi)*Phi';
        end
    end
end
SigW(num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat,num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat) = ...
    -lambda*eye(num_rand_feat) - (1/noise)*Phi*Phi';
if bool_global_cov
    for k = 1:num_models
       SigW(num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat,(k-1)*num_rand_feat+1:(k-1)*num_rand_feat+num_rand_feat) = ...
           -(1/noise)*(repmat((models(:,k)'-model_avg).*softmax(k,:),num_rand_feat,1).*Phi)*Phi';
       SigW((k-1)*num_rand_feat+1:(k-1)*num_rand_feat+num_rand_feat,num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat) = ...
           -(1/noise)*(repmat((models(:,k)'-model_avg).*softmax(k,:),num_rand_feat,1).*Phi)*Phi';
    end
end
SigW = (SigW + SigW')/2;
SigW = inv(-SigW);
SigW = (SigW + SigW')/2;
[Q,L] = eig(SigW);
L(L < 0) = 0;
SigW = Q*L*Q';
