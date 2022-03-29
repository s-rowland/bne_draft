
addpath("scripts/f_run_BNE/bne_v1_0/")
% 1 make grid search table
% 1a set parameter values we will consider
len_scale_space_w_list = [2, 1, 0.5];
len_scale_time_w_list = [15, 30];
len_scale_space_bias_list = [2, 1, 0.5];
len_scale_time_bias_list = [15, 30];
penalty_list = [0.3679,  0.0498, 0.0183];
time_metric_list = [2,1]; %['julianDay', 'dayOfYear'];
seed_list = [1234];

% 1b actually make the table
% 1b.i get all the combinations
grid_mat = combvec(len_scale_space_w_list, len_scale_time_w_list, ...
    len_scale_space_bias_list, len_scale_time_bias_list, penalty_list, ...
    time_metric_list, seed_list).';
% 1b.ii put them in a nice labeled table
grid = table;
grid.len_scale_space = grid_mat(:,1);
grid.len_scale_time = grid_mat(:,2);
grid.len_scale_space_bias = grid_mat(:,3);
grid.len_scale_time_bias = grid_mat(:,4);
grid.penalty = grid_mat(:,5);
grid.time_metric = grid_mat(:,6);
grid.seed = grid_mat(:,7);
grid.mse = transpose(repelem(0, size(grid,1)));

done = [1, 2, 3, 26, 27, 28, 51, 52, 53, 75, 76, 77, 100, 101, 102, 125, ...
    126, 150, 151, 152, 175, 176, 200, 201, 203, 204, 205, 206, 207, 208, ...
    209, 210, 211, 212];
grid.row_number = transpose(1:216);

% bring in the training dataset
training_original = readtable(append('inputs/pm25/training_datasets/','daily', '_combined/training_cvfolds.csv'));


for i = 1:size(grid,1)
    
    if i ==i %any(i == i)
        if grid.time_metric[i] == 1
            time_metric_act = 'julianDay';
        else time_metric_act = 'dayOfYear';
        end
        grid.mse(i) = cross_validate_BNE_v1('daily', 5, ...
            grid.len_scale_space(i), grid.len_scale_time(i), ...
            grid.len_scale_space_bias(i), grid.len_scale_time_bias(i), ...
            grid.penalty(i), time_metric_act, grid.seed(i), ...
            2005, 2016, 'full_grid_search', training_original);
        display(num2str(i))
    end
    writetable(grid, 'grid_redo')
end