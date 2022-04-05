addpath("scripts/f_run_BNE/bne_v1_0/")


len_scale_space_w_list = [2];
len_scale_time_w_list = [2];
len_scale_space_bias_list = [2];
len_scale_time_bias_list = [2, 1];
penalty_list = [0.3679];
penalty_bias_list = [0.3679];
time_metric_list = [1]; %['julianDay', 'dayOfYear'];
seed_list = [1234];
stage_list = [2];

% 1b actually make the table
% 1b.i get all the combinations
grid_mat = combvec(penalty_list, penalty_bias_list, ...
     len_scale_space_w_list, len_scale_time_w_list, ...
    len_scale_space_bias_list, len_scale_time_bias_list, ...
    time_metric_list, seed_list, stage_list).';
% 1b.ii put them in a nice labeled table
grid = table;
grid.penalty = transpose([0.3679, 0.498]);
grid.penalty_bias = transpose([0.3679, 0.498]);
grid.len_scale_space = grid_mat(:,3);
grid.len_scale_time = grid_mat(:,4);
grid.len_scale_space_bias = grid_mat(:,5);
grid.len_scale_time_bias = grid_mat(:,6);
grid.time_metric = grid_mat(:,7);
grid.seed = grid_mat(:,8);
grid.stage = grid_mat(:,9);
grid.mse = transpose(repelem(0, size(grid,1)));

%grid.row_number = transpose(1:30);

% 3a bring in the dataset
%targetday = readtable(append('inputs/pm25/training_datasets/','daily', '_combined/training_cvfolds.csv'));

target2 = readtable(append('inputs/pm25/training_datasets/','annual', '_combined/training_cvfolds.csv'));

%target2.day_of_year = transpose(repelem(0, size(target2,1)));

i = 1;
time_metric_act = 'julianDay';


[W,w0,SigW,Z,piZ,Zt,MSE] = train_BNE_v1('annual', 7, ...
            grid.len_scale_space(i), grid.len_scale_time(i), ...
            grid.len_scale_space_bias(i), grid.len_scale_time_bias(i), ...
            grid.penalty(i), grid.penalty_bias(i), time_metric_act, ...
            grid.stage(i), grid.seed(i));
       

predict_BNE_v1(W,w0,SigW,Z,piZ,Zt,...
                target2, 'extVal', ...
                7, grid.len_scale_space(i), 30, grid.len_scale_space_bias(i), 15, time_metric_act, ...
                'coverage_test/annual',...
                append('EV_', 'aqs', '_', ...
                strrep(num2str(grid.penalty(i)), '.', '-'), '_', ...
                strrep(num2str(grid.penalty_bias(i)), '.', '-'), '_',...
                time_metric_act, '_', num2str(grid.stage(i))))

