
% 1: run bne with selected parameters
[W,w0,SigW,Z,piZ,Zt,MSE] = train_BNE_v1('daily', 5, 2, 30, 2, 15, 0.5,0.0001, ...
    'dayOfYear', 1234, 2)

% 2: create folder to place predictions 
mkdir outputs/pm25/daily/runs/testing2

 
% 3: Generate predictions
% 3a bring in the dataset

addpath("scripts/f_run_BNE/bne_v1_0/")
% 1 make grid search table
% 1a set parameter values we will consider
yyyy_list = 2005 %:2016;
doy_list = 1;

% 1b actually make the table
% 1b.i get all the combinations
grid_mat = combvec(yyyy_list, doy_list).';
% 1b.ii put them in a nice labeled table
grid = table;
grid.yyyy = grid_mat(:,1);
grid.doy = grid_mat(:,2);
grid.ran = transpose(repelem(0, size(grid,1)));


%[~,idx] = sort(rand(1,num_obs));
    %idx = idx(1:20000);

for i = 1:size(grid, 1)
    
target =readtable(append('inputs/pm25/prediction_datasets/daily_individual/preds_', ...
    num2str(grid.yyyy(i)), '_', ...
    pad(num2str(grid.doy(i)), 3, 'left', '0'), '.csv'));

% 3b generate and save predictions

predict_BNE_v1(W,w0,SigW,Z,piZ,Zt,...
    target, 'pred_timeSlice', ...
    5, 2,30,2,15, 'dayOfYear', ...
    'str_uncert_analysis/outputs/d_bne_results/preds', ...
    append('smPenaltyBiaspreds_', num2str(grid.yyyy(i)), '_', ...
    pad(num2str(grid.doy(i)), 3, 'left', '0')))

end
