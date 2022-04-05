
addpath("scripts/f_run_BNE/bne_v1_0/")
% 1 make grid search table
% 1a set parameter values we will consider
len_scale_space_w_list = [2, 1, 0.5];
len_scale_time_w_list = [1, 2];
len_scale_space_bias_list = [2, 1, 0.5];
len_scale_time_bias_list = [1, 2];
penalty_list = [0.3679,  0.0498, 0.0183];
%penalty_bias_list = [0.3679,  0.0498, 0.0183];
time_metric_list = [1]; %['julianDay', 'dayOfYear'];
seed_list = [1234];
stage_list = [2];

% 1b actually make the table
% 1b.i get all the combinations
grid_mat = combvec(len_scale_space_w_list, len_scale_time_w_list, ...
    len_scale_space_bias_list, len_scale_time_bias_list, penalty_list, ... %penalty_bias_list,
     time_metric_list, seed_list, stage_list).';
% 1b.ii put them in a nice labeled table
grid = table;
grid.len_scale_space = grid_mat(:,1);
grid.len_scale_time = grid_mat(:,2);
grid.len_scale_space_bias = grid_mat(:,3);
grid.len_scale_time_bias = grid_mat(:,4);
grid.penalty = grid_mat(:,5);
grid.penalty_bias = grid_mat(:,5);
grid.time_metric = grid_mat(:,6);
grid.seed = grid_mat(:,7);
grid.stage = grid_mat(:,8);
grid.mse = transpose(repelem(0, size(grid,1)));

%grid.row_number = transpose(1:108);

% bring in the training dataset
training_original = readtable(append('inputs/pm25/training_datasets/',... 
    'annual', '_combined/training_cvfolds.csv'));

%done = [3:6:105 148:6:214] ;

for i = 1:3:size(grid,1)
    
    if  i ==i %grid.time_metric(i) ~= 1 %any(i == done)
        
        time_metric_act = 'julianDay';

        grid.mse(i) = cross_validate_BNE_v1('annual', 7, ...
            grid.len_scale_space(i), grid.len_scale_time(i), ...
            grid.len_scale_space_bias(i), grid.len_scale_time_bias(i), ...
            grid.penalty(i),grid.penalty_bias(i), time_metric_act, grid.seed(i), ...
            2, training_original);
        display(num2str(i))
    end
    writetable(grid, 'grids/grid_annual_3')
end