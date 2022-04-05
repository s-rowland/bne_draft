function [W,RP,sigW,Zs,Zt,piZ,mse] = BNE_v1(y, space, time, models, ...
    scale_space_w, scale_time_w, scale_space_rp, scale_time_rp, ...
    lambda_w, lambda_rp, time_metric, opt_stage, seed, bne_mode)
% % Implements a stochastic optimization (MAP inference) version of BNE.
% %
% % === Inputs ===
% % 
% %     y : vector or measurements, N space 1 
% %     space : geographic locations for measurements in y, N space 2 (we use lat-long)
% %     time : time stamp for measurements in y, N space 1 ( we use 1 unit = 1 day)
% %     models : model predictions for each measurement, N space (number of models)
% %     num_rand_feat : dimensionality of random features (we've found 500 to be plenty)
% %     scale_space_w : RBF spatial kernel parameter (we've been using 6.5, but not optimized)
% %     scale_time_w : RBF temporal kernel parameter (we've been using 17.5, but not optimized)
% %     scale_space_rp : RBF spatial kernel parameter (we've been using 6.5, but not optimized)
% %     scale_time_rp : RBF temporal kernel parameter (we've been using 17.5, but not optimized)
% %     bool_periodic : indicates that the time-varying portion of the kernel should repeat each year
% % 
% %  === Outputs ===
% % 
% %     W : num_rand_feat space num_models parameters for each model
% %     rp : num_rand_feat space 1 parameters for bias term
% %     sigW : Covariance of W and rp for Gaussian approximation. 
% %             Comment: This is for all parameters in W and rp combined by vectorizing
% %             the columns of W first and then appending rp to the end, so it is large.
% %             I check to see if it is positive semidefinite as required. I've noticed sometimes 
% %             it isn't, which is strange and I need to look more into it. If not, I
% %             currently set any negative eigenvalues to zero and recalculate.
% %     Z & piZ : The random variables used to calculate the random
% %                features (Phi in code). Need to use the same ones for prediction.

%%%% ------------------------------ %%%%
%%%% 0: Set Up Objects for Test Run %%%%
%%%% ------------------------------ %%%%

% models = trainPreds; y = trainAqs; space = trainSpace; time = trainTime;


%%%% ---------------------------------- %%%%
%%%% 1: Set Up Objects for Optimization %%%%
%%%% ---------------------------------- %%%%

% 1.a. set the number of random features 
% 500 is generally sufficient
num_rand_feat = 500

% 1.b. determine the dimensions we are working with
[num_obs,num_models] = size(models);
dimspace = size(space,2);

% 1.c. intitialize the parameter values at zero 
W = zeros(num_rand_feat,num_models);
RP = zeros(num_rand_feat,1);

% 1.d. create mapping for RFF random features
% 1.d.i. spatial dimensions
Zs = randn(num_rand_feat,dimspace);
% 1.d.ii temporal dimenion
if strcmp(time_metric, 'percentOfYear')
    % 2D time for year invariance, but seasonal variation
    Zt = randn(num_rand_feat,2); 
else
    % One dimensional time - julian date since start of study period
    Zt = randn(num_rand_feat,1); 
end
% 1.d.iii. another component
piZ = 2*pi*rand(num_rand_feat,1);

% 1.e. Set SNR to 8. This can be changed.
% we can just keep at 8 because we choose the penalties, which impacts
% optimization in the same way as the signal:noise aka noise: true variance
noise = var(y)/8; 

% 1.f. determin the number of data points to randomly sample per model parameter update
if num_obs < 10000    
    batch_size = 500;   
else
    batch_size = 2000; 
end

% 1.g. initialize the error 
err = 100;
mse = 0;

% 1.h. set the seed 
rng(seed);

%%%% ---------------------- %%%%
%%%% 2: Optimize Parameters %%%%
%%%% ---------------------- %%%%

% 2.0. begin loop 

if strcmp(bne_mode, 'testBNE')
    max_iter = 200; 
else 
    max_iter = 2000
end
    
for iter = 1:max_iter

    % 2.a. Subsample batch_size number of points 
    [~,idx] = sort(rand(1,num_obs));
    idx = idx(1:batch_size);
    
    % 2.b. Construct "random" features for those points.
    % (The randomness happens once at the beginning of loop)
    if strcmp(time_metric, 'percentOfYear')
        phi_w = sqrt(2/num_rand_feat)*cos(Zs*space(idx,:)'/scale_space_w + ...
            Zt*58.0916*[cos(2*pi*time(idx))' ; sin(2*pi*time(idx))']/scale_time_w + piZ*ones(1,batch_size));
        phi_rp = sqrt(2/num_rand_feat)*cos(Zs*space(idx,:)'/scale_space_rp + ...
             Zt*58.0916*[cos(2*pi*time(idx))' ; sin(2*pi*time(idx))']/scale_time_rp + piZ*ones(1,batch_size));
    else
        phi_w = sqrt(2/num_rand_feat)*cos(Zs*space(idx,:)'/scale_space_w + ...
            Zt*time(idx)'/scale_time_w + piZ*ones(1,batch_size));
        phi_rp = sqrt(2/num_rand_feat)*cos(Zs*space(idx,:)'/scale_space_rp + ...
                Zt*time(idx)'/scale_time_rp + piZ*ones(1,batch_size));
    end
        
    % 2.c Calculate stochastic gradient and update model GP vectors
    dotWPhi = W'*phi_w;
    softmax = exp(dotWPhi);
    softmax = softmax./repmat(sum(softmax,1),num_models,1);
    model_avg = sum(softmax.*models(idx,:)',1);
    dotRPPhi = RP'*phi_rp;
    error = y(idx)' - model_avg - dotRPPhi;
    grad = phi_w*((1/noise)*repmat(error,num_models,1).*(models(idx,:)' - ...
        repmat(model_avg,num_models,1)).*softmax)' - lambda_w*W;
    W = W + grad/sqrt(iter);
    
    % 2.d Update the residual process vector, if we are doing the one-stage
    % optimization
    if opt_stage == 1
        dotWPhi = W'*phi_w;
        softmax = exp(dotWPhi);
        softmax = softmax./repmat(sum(softmax,1),num_models,1);
        model_avg = sum(softmax.*models(idx,:)',1);
        dotRPPhi = RP'*phi_rp;
        residual = y(idx) - model_avg' - dotRPPhi;
        RPtmp = inv(lambda_rp*noise*eye(num_rand_feat) + phi_rp*phi_rp')*(phi_rp*residual);
        RP = rptmp/sqrt(iter) + (1-1/sqrt(iter))*RP;
    end
    
    
    % 2.e. Display progress of algorithm
    error = y(idx)' - model_avg -dotRPPhi;
    mse = (iter-1)*mse/iter + mean(error(:).^2)/iter;  % Roughly approximates the training mse
    display(['Weights Iteration ' num2str(iter) ' ::: mse ' num2str(mse)]);
end

%%%% ----------------------------------- %%%%
%%%% 3: Optimize Residual Process Vector %%%%
%%%% ----------------------------------- %%%%

% 3.a. generate vector to cover all values
% although it looks dumb, we use this vector so that the construction of
% Phi can have the exact form each time. 
idx = 1:1:num_obs;

% 3.b. create the random features of all points
if strcmp(time_metric, 'percentOfYear')
    phi_w = sqrt(2/num_rand_feat)*cos(Zs*space(idx,:)'/scale_space_w + ...
        Zt*58.0916*[cos(2*pi*time(idx))' ; sin(2*pi*time(idx))']/scale_time_w + piZ*ones(1,num_obs));
    phi_rp = sqrt(2/num_rand_feat)*cos(Zs*space(idx,:)'/scale_space_rp + ...
        Zt*58.0916*[cos(2*pi*time(idx))' ; sin(2*pi*time(idx))']/scale_time_rp + piZ*ones(1,num_obs));
else
    phi_w = sqrt(2/num_rand_feat)*cos(Zs*space(idx,:)'/scale_space_rp + ...
        Zt*time(idx)'/scale_time_rp + piZ*ones(1,num_obs));
    phi_rp = sqrt(2/num_rand_feat)*cos(Zs*space(idx,:)'/scale_space_rp + ...
        Zt*time(idx)'/scale_time_rp + piZ*ones(1,num_obs));
end

% 3.c. determine residual error after using optimal weights
dotWPhi = W'*phi_w;
% 3.c.ii. catch for really big values 
dotWPhi(dotWPhi>100)=90;
dotWPhi(dotWPhi<-100)=-90;
% 3.c.iii continue
softmax = exp(dotWPhi);
softmax = softmax./repmat(sum(softmax,1),num_models,1);
model_avg = sum(softmax.*models(idx,:)',1);
residual = y(idx) - model_avg';

% 3.d calculate optimal rp with closed-form equation
RP = inv(lambda_rp*noise*eye(num_rand_feat) + phi_rp*phi_rp')*(phi_rp*residual);
%rp = rptmp/sqrt(iter) + (1-1/sqrt(iter))*rp;
dotRPPhi = RP'*phi_rp;
    
% Display progress of algorithm
error = y(idx)' - model_avg - dotRPPhi;
mse = (iter-1)*mse/iter + mean(error(:).^2)/iter;  % Roughly approximates the training mse
display(['final mse ' ' ::: mse ' num2str(mse)]);


%%%% ---------------------- %%%%
%%%% 2: Optimize Parameters %%%%
%%%% ---------------------- %%%%

if strcmp(bne_mode, 'testMeanPredOnly')
    sigW = 0; 
else 
    
% % === CALCULATE THE COVARIANCE ===
    bool_global_cov = 0; % If 1, this calculates cross correlations across model/bias vectors. 
    % If 0, it still calculates correlations within paramter vectors of each model & bias
    % we do correlation within parameters as an approspaceimation to avoid getting 
    % a matrispace that is not positive definite
    sigW = zeros(num_rand_feat*(num_models+1));
    for iter = 1:floor(num_obs/batch_size)-1
        if strcmp(time_metric, 'percentOfYear')
            phi_w = sqrt(2/num_rand_feat)*cos(Zs*space(iter*batch_size+1:iter*batch_size+batch_size,:)'/scale_space_w + Zt*58.0916*[cos(2*pi*time(iter*batch_size+1:iter*batch_size+batch_size))' ; sin(2*pi*time(iter*batch_size+1:iter*batch_size+batch_size))']/scale_time_w + piZ*ones(1,batch_size));
            phi_rp = sqrt(2/num_rand_feat)*cos(Zs*space(iter*batch_size+1:iter*batch_size+batch_size,:)'/scale_space_rp + Zt*58.0916*[cos(2*pi*time(iter*batch_size+1:iter*batch_size+batch_size))' ; sin(2*pi*time(iter*batch_size+1:iter*batch_size+batch_size))']/scale_time_rp + piZ*ones(1,batch_size));
        else
            phi_w = sqrt(2/num_rand_feat)*cos(Zs*space(iter*batch_size+1:iter*batch_size+batch_size,:)'/scale_space_w + Zt*time(iter*batch_size+1:iter*batch_size+batch_size)'/scale_time_w + piZ*ones(1,batch_size));
            phi_rp = sqrt(2/num_rand_feat)*cos(Zs*space(iter*batch_size+1:iter*batch_size+batch_size,:)'/scale_space_rp + Zt*time(iter*batch_size+1:iter*batch_size+batch_size)'/scale_time_rp + piZ*ones(1,batch_size));    
        end
        dotWPhi = W'*phi_w; 
        % catch for really big values 
        dotWPhi(dotWPhi>100)=90;
        dotWPhi(dotWPhi<-100)=-90;
        softmax = exp(dotWPhi);
        softmax = softmax./repmat(sum(softmax,1),num_models,1);
        model_avg = sum(softmax.*models(iter*batch_size+1:iter*batch_size+batch_size,:)',1);
        dotRPPhi = RP'*phi_rp;
        error = y(iter*batch_size+1:iter*batch_size+batch_size)' - model_avg - dotRPPhi;
        t1 = -((models(iter*batch_size+1:iter*batch_size+batch_size,:)' - repmat(model_avg,num_models,1)).*softmax).^2;
        t2 = repmat(error,num_models,1).*(models(iter*batch_size+1:iter*batch_size+batch_size,:)' - repmat(model_avg,num_models,1)).*softmax.*(1-2*softmax);
        for i = 1:num_models
            for j = i:num_models
                if i == j
                    sigW((i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat,(i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat) = ...
                        sigW((i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat,(i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat) ...
                            -lambda_w*eye(num_rand_feat)/floor(num_obs/batch_size) + (1/noise)*((repmat(t1(i,:) + t2(i,:),num_rand_feat,1).*phi_w)*phi_w');
                elseif bool_global_cov
                    tij1 = -(models(iter*batch_size+1:iter*batch_size+batch_size,i)'-model_avg).*(models(iter*batch_size+1:iter*batch_size+batch_size,j)'-model_avg).*softmax(i,:).*softmax(j,:);
                    tij2 = -error.*(models(iter*batch_size+1:iter*batch_size+batch_size,j)'-model_avg).*softmax(i,:).*softmax(j,:);
                    tij3 = -error.*(models(iter*batch_size+1:iter*batch_size+batch_size,i)'-model_avg).*softmax(i,:).*softmax(j,:);
                    sigW((i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat,(j-1)*num_rand_feat+1:(j-1)*num_rand_feat+num_rand_feat) = ...
                        sigW((i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat,(j-1)*num_rand_feat+1:(j-1)*num_rand_feat+num_rand_feat) + ...
                        (repmat((1/noise)*(tij1+tij2+tij3),num_rand_feat,1).*phi_w)*phi_w';
                    sigW((j-1)*num_rand_feat+1:(j-1)*num_rand_feat+num_rand_feat,(i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat) = ...
                        sigW((j-1)*num_rand_feat+1:(j-1)*num_rand_feat+num_rand_feat,(i-1)*num_rand_feat+1:(i-1)*num_rand_feat+num_rand_feat) + ...
                        (repmat((1/noise)*(tij1+tij2+tij3),num_rand_feat,1).*phi_w)*phi_w';
                end
            end
        end
        sigW(num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat,num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat) = ...
            sigW(num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat,num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat) ...
            -lambda_rp*eye(num_rand_feat)/floor(num_obs/batch_size) - (1/noise)*phi_rp*phi_rp';
        if bool_global_cov
            for k = 1:num_models
               sigW(num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat,(k-1)*num_rand_feat+1:(k-1)*num_rand_feat+num_rand_feat) = ...
                   sigW(num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat,(k-1)*num_rand_feat+1:(k-1)*num_rand_feat+num_rand_feat) ...
                   -(1/noise)*(repmat((models(iter*batch_size+1:iter*batch_size+batch_size,k)'-model_avg).*softmax(k,:),num_rand_feat,1).*phi_w)*phi_rp';
               sigW((k-1)*num_rand_feat+1:(k-1)*num_rand_feat+num_rand_feat,num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat) = ...
                   sigW((k-1)*num_rand_feat+1:(k-1)*num_rand_feat+num_rand_feat,num_models*num_rand_feat+1:num_models*num_rand_feat+num_rand_feat) ...
                   -(1/noise)*(repmat((models(iter*batch_size+1:iter*batch_size+batch_size,k)'-model_avg).*softmax(k,:),num_rand_feat,1).*phi_rp)*phi_w';
            end
        end
    end
    % we keep only the diagonal values - within-parameter variance as an approspaceimation to avoid getting 
    % a matrispace that is not positive definite
    % by not borrowing information from other parameters, our approspaceimation has
    % slightly higher uncertainty than the true uncertainty. 
    sigW = diag(abs(1./diag(sigW)));
end
