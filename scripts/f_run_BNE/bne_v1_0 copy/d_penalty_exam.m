
% 3: Generate predictions
% 3a bring in the dataset

addpath("scripts/f_run_BNE/bne_v1_0/")
% 1 make grid search table
% 1a set parameter values we will consider
yyyy_list = 2005 %:2016;
doy_list = [1, 150, 250];

% 1b actually make the table
% 1b.i get all the combinations
grid_mat = combvec(yyyy_list, doy_list).';
% 1b.ii put them in a nice labeled table
gridtimes = table;
gridtimes.yyyy = grid_mat(:,1);
gridtimes.doy = grid_mat(:,2);
gridtimes.ran = transpose(repelem(0, size(gridtimes,1)));


% 1 make grid search table
% 1a set parameter values we will consider
len_scale_space_w_list = [2];
len_scale_time_w_list = [30];
len_scale_space_bias_list = [2];
len_scale_time_bias_list = [15];
penalty_list = [0.9, 0.1, 0.01, 0.001, 0.0001];
penalty_bias_list = [0.9, 0.1, 0.01, 0.001, 0.0001];
time_metric_list = [2,1]; %['julianDay', 'dayOfYear'];
seed_list = [1234];
stage_list = [1, 2];

% 1b actually make the table
% 1b.i get all the combinations
grid_mat = combvec(len_scale_space_w_list, len_scale_time_w_list, ...
    len_scale_space_bias_list, len_scale_time_bias_list, penalty_list, ...
    penalty_bias_list, time_metric_list, seed_list, stage_list).';
% 1b.ii put them in a nice labeled table
grid = table;
grid.len_scale_space = grid_mat(:,1);
grid.len_scale_time = grid_mat(:,2);
grid.len_scale_space_bias = grid_mat(:,3);
grid.len_scale_time_bias = grid_mat(:,4);
grid.penalty = grid_mat(:,5);
grid.penalty_bias = grid_mat(:,6);
grid.time_metric = grid_mat(:,7);
grid.seed = grid_mat(:,8);
grid.stage = grid_mat(:,9);
grid.mse = transpose(repelem(0, size(grid,1)));

grid.row_number = transpose(1:100);

target =readtable(append('inputs/pm25/prediction_datasets/daily_individual/preds_', ...
            num2str(2005), '_', ...
            pad(num2str(1), 3, 'left', '0'), '.csv'));
num_obs = size(target, 1);
[~,idx] = sort(rand(1,num_obs));
idx = idx(1:30000);

for i = 75:100 %size(grid,1)
    
        if grid.time_metric(i) == 1
            time_metric_act = 'julianDay';
        else time_metric_act = 'dayOfYear';
        end
        [W,w0,SigW,Z,piZ,Zt,MSE] = train_BNE_v1('daily', 5, ...
            grid.len_scale_space(i), grid.len_scale_time(i), ...
            grid.len_scale_space_bias(i), grid.len_scale_time_bias(i), ...
            grid.penalty(i), grid.penalty_bias(i), time_metric_act, ...
            grid.stage(i), grid.seed(i));
        
        
        
        
    for j = 1:3
        target =readtable(append('inputs/pm25/prediction_datasets/daily_individual/preds_', ...
            num2str(gridtimes.yyyy(j)), '_', ...
            pad(num2str(gridtimes.doy(j)), 3, 'left', '0'), '.csv'));
        target = target(idx, :);

            % features. (The randomness happens once at the beginning)

    % 3b generate and save predictions

        predict_BNE_v1(W,w0,SigW,Z,piZ,Zt,...
            target, 'pred_timeSlice', ...
            5, 2,30,2,15, time_metric_act, ...
            'bne_preds_penalty_test/preds', ...
            append('preds_', num2str(gridtimes.yyyy(j)), '_', ...
            pad(num2str(gridtimes.doy(j)), 3, 'left', '0'),'_', ...
            strrep(num2str(grid.penalty(i)), '.', '-'), '_', ...
            strrep(num2str(grid.penalty_bias(i)), '.', '-'), '_',...
            time_metric_act, '_', num2str(grid.stage(i))))

    end

    display(num2str(i))
    
end


