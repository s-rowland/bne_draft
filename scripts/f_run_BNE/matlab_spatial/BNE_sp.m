function [W,w0,SigW,Z,piZ] = BNE_sp(y,X,models,num_rand_feat,len_scale, resid)
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

%%%% ------------------------------ %%%%
%%%% 0: Set Up Objects for Test Run %%%%
%%%% ------------------------------ %%%%

% Set some initial parameteres
% YYYY = '2015';
% inputset = 'avgscmjscc';
% len_scale = 0.5';
% fold = 'all';
% resid = 'resid'
% read data
% training = readtable(append('BNE_inputs/training_datasets/individual_annual/training_', 'avgscmjscc_', YYYY, '_', fold, '.csv'));
% break up training data into its components 
% third column is year 
% y = training{:,4};
% X = training{:,1:2};
% models = training{:,5:9};
% num_rand_feat = 500;

%%%% --------- %%%%
%%%% Begin BNE %%%%
%%%% --------- %%%%

%%%% ------------------------- %%%%
%%%% 1: Create Initial Objects %%%%
%%%% ------------------------- %%%%

% 1a extract the number of training points and the number of input models 
[num_obs,num_models] = size(models);

% 1b number of space dimensions
dimX = size(X,2);

% 1c an all-zero matrix that will be filled with weights 
% dimensions are 500 rows(one per random feature) and 
% 5 columns (one per input model) 
% remember, with this random features approach, each point in spacetime has
% a particular value for each of the 500 features (sorta similar to natural
% spline). 
W = zeros(num_rand_feat,num_models);

% 1d an all-zero vector for the offset term 
w0 = zeros(num_rand_feat,1);

% 1e create the random features
% 1e.i intermediate step
Z = randn(num_rand_feat,dimX);
% 1e.ii intermediate step - the 'key' to translating between latlon & RFF
piZ = 2*pi*rand(num_rand_feat,1);
% 1e.iii locations of training points expressed as the random features
Phi = sqrt(2/num_rand_feat)*cos(Z*X'/len_scale + piZ*ones(1,num_obs));

% 1f set three hyperparameters. 
% lambda relates to the prior of the weights
% and lambda0 relates to the prior of the offset 
% the lambdas act as penalties
noise = var(y)/8; %% Set SNR to 8. This can be changed.
lambda = 0.1;
lambda0 = 0.1;

%%%% -------------------- %%%%
%%%% 2: OPTIMIZE W AND w0 %%%%
%%%% -------------------- %%%%

for iter = 1:5000
    
    % determine the size of the steps we will take in the direction of the
    % gradient 
    step_size = .1/sqrt(num_obs);
    
    % inner loop
    for step = 1:5
        % 2b calculate log of weight of each model, at each training point
        dotWPhi = W'*Phi;
        % 2c exponentiate to get weight
        softmax = exp(dotWPhi);
        % 2d constrain the weights to be between 0 and 1
        softmax = softmax./repmat(sum(softmax,1),num_models,1);
        % 2e weighted average of models- 'base ensemble'
        model_avg = sum(softmax.*models',1);
        % 2f calculate bias term
        bias = w0'*Phi;
        % 2g error with current parameters
        error = y' - model_avg - bias;
        % 2h determine gradient (direction to step towards to reduce error)
        grad = Phi*((1/noise)*repmat(error,num_models,1).*(models' - repmat(model_avg,num_models,1)).*softmax)' - lambda*W;
        % 2g take a step in the direction of the gradient
        W = W + step_size*grad;
    end
    
    % now we update the offset, after tuning the others for a bit
    % 3a calculate log of weight of each model, at each training point
    dotWPhi = W'*Phi;
    % 3b exponentiate to get weight
    softmax = exp(dotWPhi);
    % 3c constrain the weights to be between 0 and 1
    softmax = softmax./repmat(sum(softmax,1),num_models,1);
    % 3d weighted average of models- 'base ensemble'
    model_avg = sum(softmax.*models',1);
    % 3e difference of 'base ensemble' and obs
    residual = y - model_avg';
    % 3f offset term
    % start with the last term 
    % we translate the residuals from latlon to the 500 RFF 
    % then we add up the amount of residual for each feature with the
    % matrix multiplication 
    % the inversion and the lambda0*noise is about penalizing the offset
    switch resid
        case 'resid' 
            w0 = inv(lambda0*noise*eye(num_rand_feat) + Phi*Phi')*(Phi*residual);
        case 'noResid'
            w0 = zeros(num_rand_feat,1);
    end 
    %end
    % 3g error (but we don't use it) 
    % do we update the noise?
    error = y' - model_avg - bias;

    %if iter > 100
     %    noise = mean(error(:).^2);
     %end
    display(['Iteration ' num2str(iter) ' ::: MSE ' num2str(mean(error(:).^2))]);
end


% % === CALCULATE THE COVARIANCE ===
% this is used to generate the samples in the prediction step

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
