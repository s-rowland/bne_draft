


%%%% ---------------------- %%%%
%%%% 1: Optimize Parameters %%%%
%%%% ---------------------- %%%%

addpath("scripts/f_run_BNE/bne_v1/")

% 1 make grid search table
% 1a set parameter values we will consider
scale_space_w_list = [2, 1, 0.5];
scale_time_w_list = [2,1, 0.5];
scale_space_rp_list = [2, 1, 0.5];
scale_time_rp_list = [2,1,0.5];
lambda_list = [0.3679,  0.0498, 0.0183];
lambda_rp_list = [0.3679,  0.0498, 0.0183];
opt_stage_list = [2];
seed_list = [1234];


% 1b actually make the table
% 1b.i get all the combinations
grid_mat = combvec(scale_space_w_list, scale_time_w_list, ...
    scale_space_rp_list, scale_time_rp_list, lambda_list, lambda_rp_list, ...
     opt_stage_list, seed_list).';
% 1b.ii put them in a nice labeled table
grid = table;
grid.scale_space_w = grid_mat(:,1);
grid.scale_time_w = grid_mat(:,2);
grid.scale_space_rp = grid_mat(:,3);
grid.scale_time_rp = grid_mat(:,4);
grid.lambda_w = grid_mat(:,5);
grid.lambda_rp = grid_mat(:,6);
grid.opt_stage = grid_mat(:,7);
grid.seed = grid_mat(:,8);
grid.rmse = transpose(repelem(0, size(grid,1)));
grid.r2 = transpose(repelem(0, size(grid,1)));
grid.cover = transpose(repelem(0, size(grid,1)));

% temporary restriction
%idx = grid.lambda_w == 0.1353 | grid.lambda_rp == 0.1353; %find what rows this is true
%grid = grid(idx,:); 

%grid.row_number = transpose(1:108);

% bring in the training dataset
training_full = readtable(append('inputs/pm25/training_datasets/',... 
    'annual', '_combined/training_cvfolds_nome.csv'));

%done = [3:6:105 148:6:214] ;

num_models = 6


%%%% ---------------------- %%%%
%%%% 2: Optimize Parameters %%%%
%%%% ---------------------- %%%%

for i = 600:size(grid,1)
    time_metric = 'year';
    
    [rmse r2 cover] = cross_validate_BNE_v1(training_full, num_models, ...
        grid.scale_space_w(i), grid.scale_time_w(i),  ...
        grid.scale_space_rp(i), grid.scale_time_rp(i), ...
        grid.lambda_w(i), grid.lambda_rp(i), time_metric, ...
        grid.opt_stage(i), grid.seed(i));
    grid.rmse(i) = rmse; 
    grid.r2(i) = r2; 
    grid.cover(i) = cover; 
    
        display(num2str(i))
        % update results table
    writetable(grid, 'str_uncert_analysis/outputs/b_description_bne_grid_search/annual_grid_search_nome_600')
end