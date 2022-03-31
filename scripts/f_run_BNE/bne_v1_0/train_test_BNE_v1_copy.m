
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

grid.row_number = transpose(1:216); 
  
i = 3

window = 'daily'; num_models = 5; fold=2;
  len_scale_space = grid.len_scale_space(i); len_scale_time = grid.len_scale_time(i); 
  len_scale_space_bias = grid.len_scale_space_bias(i);
  len_scale_time_bias = grid.len_scale_time_bias(i); penalty = grid.penalty(i); 
  time_metric = time_metric_act; seed = 1234;

  
  
  
  mse = [0 0];
mse = [mse train_test_BNE_v1(window, num_models, 1, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, training_original)];
display('fold 01 done')
mse = [mse train_test_BNE_v1(window, num_models, 2, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, training_original)];
display('fold 02 done')
mse = [mse train_test_BNE_v1(window, num_models, 3, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, training_original)];
display('fold 03 done')
mse = [mse train_test_BNE_v1(window, num_models, 4, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, training_original)];
display('fold 04 done')
mse = [mse train_test_BNE_v1(window, num_models, 5, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, training_original)];
display('fold 05 done')
mse = [mse train_test_BNE_v1(window, num_models, 6, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, training_original)];
display('fold 06 done')
mse = [mse train_test_BNE_v1(window, num_models, 7, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, training_original)];
display('fold 07 done')
mse = [mse train_test_BNE_v1(window, num_models, 8, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, training_original)];
display('fold 08 done')
mse = [mse train_test_BNE_v1(window, num_models, 9, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, training_original)];
display('fold 09 done')
mse = [mse train_test_BNE_v1(window, num_models, 10, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, training_original)];
display('fold 10 done')