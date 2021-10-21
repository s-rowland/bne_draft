function [W,w0,SigW,Z,piZ, Zt] = BNE_spt(y,X,time,models,num_rand_feat,len_scale_space,len_scale_time)
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

%%%% ------------------------------ %%%%
%%%% 0: Set Up Objects for Test Run %%%%
%%%% ------------------------------ %%%%

% 0a Set some initial parameteres
 %inputset = 'avgscmjscc';
 %len_scale_space = 3.5';
 %len_scale_time = 2;
 %fold = 'all';
% 0b read data 
%training = readtable(append('BNE_inputs/training_datasets/combined_annual/training_', inputset, '_', 'all', '.csv'));
% 0c break up training data into its components 
% third column is year 
% y = training{:,4};
% X = training{:,1:2};
% time = training{:,3};
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
% 1e.i intermediate step for spatial 
Z = randn(num_rand_feat,dimX);
% 1e.i intermediate step for spatial 
Zt = randn(num_rand_feat,1);
% 1e.ii intermediate step - the 'key' to translating between latlon & RFF
% not exactly
piZ = 2*pi*rand(num_rand_feat,1);

% 1f set three coefficients. 
% lambda relates to the prior of the weights
% and lambda0 relates to the prior of the offset 
% the lambdas act as penalties
noise = var(y)/8; %% Set SNR to 8. This can be changed.
lambda = .1;
lambda0 = .1;

% 1g Number of data points to randomly sample per model parameter update
% this line is not in the spatial model
% the idea here is to increase computational efficiency by update the model
% based on some observations, rather than using all of them every single
% time. 
% This component makes it 'stochastic gradient descent'
% may not be as necesary for the annual component
batch_size = 500; 

% 1h not sure
err = 100;

%%%% -------------------- %%%%
%%%% 2: OPTIMIZE W AND w0 %%%%
%%%% -------------------- %%%%

for iter = 1:2000 % should be 20k
    
    % 2a determine step size for MAP optimization
    step_size = 1/sqrt(batch_size);

    % 2b Subsample batch_size number of points and construct "random"
    % features. (The randomness happens once at the beginning)
    % 2b.i first we take the index of the observations, and randomly
    % arrange them
    [~,idx] = sort(rand(1,num_obs));
    % 2b.ii next we keep a subset of the indicies 
    % where the size of the subset is the batch_size) 
    % e.g., if batch_size is 10, you pick the first 10 random indicies
    idx = idx(1:batch_size);
    % 2b.iii determine the space-time location of the points, 
    % translating from lat, lon and date to random features
    Phi = sqrt(2/num_rand_feat)*cos(Z*X(idx,:)'/len_scale_space + Zt*time(idx)'/len_scale_time + piZ*ones(1,batch_size));
    
    % 2c Calculate stochastic gradient and update model GP (weight) vectors
    % 2c.i calculate log of weight of each model, at each training point
    dotWPhi = W'*Phi;
    % 2c.ii exponentiate to get weight
    softmax = exp(dotWPhi);
    % 2c.iii constrain the weights to be between 0 and 1
    softmax = softmax./repmat(sum(softmax,1),num_models,1);
    % 2c.iv weighted average of models- 'base ensemble'
    model_avg = sum(softmax.*models(idx,:)',1);
    % 2c.v calculate bias term
    bias = w0'*Phi;
    % 2c.vi error with current parameters
    error = y(idx)' - model_avg - bias;
    % 2c.vii determine gradient (direction to step towards to reduce error)
    grad = Phi*((1/noise)*repmat(error,num_models,1).*(models(idx,:)' - repmat(model_avg,num_models,1)).*softmax)' - lambda*W;
    % 2c.viii take a step in the direction of the gradient
    % note that the step size actually shrinks, because we divide by the
    % iter
    W = W + step_size*grad/sqrt(iter);

    % 2d Calculate stochastic gradient and update bias vector
    % 2d.i calculate log of weight of each model, at each training point
    dotWPhi = W'*Phi;
    % 2d.ii exponentiate to get weight
    softmax = exp(dotWPhi);
    % 2d.iii constrain the weights to be between 0 and 1
    softmax = softmax./repmat(sum(softmax,1),num_models,1);
    % 2d.iv weighted average of models- 'base ensemble'
    model_avg = sum(softmax.*models(idx,:)',1);
    % 2d.v difference of 'base ensemble' and observation
    residual = y(idx) - model_avg';
    % 2d.vii offset term
    % start with the last term 
    % we translate the residuals from latlon to the 500 RFF 
    % then we add up the amount of residual for each feature with the
    % matrix multiplication 
    % the inversion and the lambda0*noise is about penalizing the offset
    w0tmp = inv(lambda0*noise*eye(num_rand_feat) + Phi*Phi')*(Phi*residual);
    % 2d.viii not sure 
    w0 = w0tmp/sqrt(iter) + (1-1/sqrt(iter))*w0;
    
    % 2e Display progress of algorithm
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
