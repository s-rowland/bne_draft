
addpath("scripts/f_run_BNE/bne_v1_0/")
% 1 make grid search table
% 1a set parameter values we will consider
len_scale_space_w_list = [3, 2, 1, 0.5];
len_scale_time_w_list = [10, 20, 30];
len_scale_space_bias_list = [3, 2, 1, 0.5];
len_scale_time_bias_list = [10, 20, 30];
penalty_list = [0.3679, 0.1353, 0.0498, 0.0183];
time_metric_list = [1, 2]; %['julianDay', 'dayOfYear'];
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
grid.time_metric = grid_mat(:,5);
grid.penalty = grid_mat(:,6);
grid.seed = grid_mat(:,7);
grid.mse = transpose(repelem(0, size(grid,1)));

writetable(grid)