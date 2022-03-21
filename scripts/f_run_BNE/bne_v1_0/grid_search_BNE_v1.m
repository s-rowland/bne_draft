% 1 make grid search table
% 1a set parameter values we will consider
len_scale_space_w_list = [3, 2];
len_scale_time_w_list = [20, 30];
len_scale_space_bias_list = [3, 2];
len_scale_time_bias_list = [20, 30];
seed_list = [1234, 5555, 9999];
% 1b actually make the table
% 1b.i get all the combinations
grid_mat = combvec(len_scale_space_w_list, len_scale_time_w_list, ...
    len_scale_space_bias_list, len_scale_time_bias_list, seed_list).';
% 1b.ii put them in a nice labeled table
grid = table;
grid.len_scale_space = grid_mat(:,1);
grid.len_scale_time = grid_mat(:,2);
grid.len_scale_space_bias = grid_mat(:,3);
grid.len_scale_time_bias = grid_mat(:,4);
grid.seed = grid_mat(:,5);
grid.mse = transpose(repelem(0, size(grid,1)));

for i = 1:2%size(grid,1)
    grid.mse(i) = cross_validate_BNE_v1('daily', 5, ...
        grid.len_scale_space(i), grid.len_scale_time(i), ...
        grid.len_scale_space_bias(i), grid.len_scale_time_bias(i), ...
        0.1, 'julianDay', grid.seed(i), 2005, 2006, 'mini_grid_search');
    display(num2str(i))

end