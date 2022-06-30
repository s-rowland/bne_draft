

%%%% ---------------------- %%%%
%%%% 1: Set up for Loop  %%%%
%%%% ---------------------- %%%%

% 1.a. set path for the bne functions
addpath("scripts/f_run_BNE/bne_v1/")

% 1.a make table of hyperparameter combinations
% 1a set parameter values we will consider
scale_space_w_list = [2];
scale_time_w_list = [0.5];
scale_space_rp_list = [2];
scale_time_rp_list = [0.5];
lambda_w_list = [0.0498];
lambda_rp_list = [0.1353];
time_metric_list = [1]; %['julianDay', 'dayOfYear'];
seed_list = [1234];
opt_stage_list = [2];

% 1.b. actually make the table
% 1b.i get all the combinations
grid_mat = combvec(scale_space_w_list, scale_time_w_list, ...
    scale_space_rp_list, scale_time_rp_list, lambda_w_list, lambda_rp_list,...
   time_metric_list, seed_list, opt_stage_list).';
% 1b.ii put them in a nice labeled table
grid = table;
grid.scale_space_w = grid_mat(:,1);
grid.scale_time_w = grid_mat(:,2);
grid.scale_space_rp = grid_mat(:,3);
grid.scale_time_rp = grid_mat(:,4);
grid.lambda_w = grid_mat(:,5);
grid.lambda_rp =grid_mat(:,6);
grid.time_metric = grid_mat(:,7);
grid.seed = grid_mat(:,8);
grid.opt_stage = grid_mat(:,9);

% 1.c. set time metric
time_metric = 'year';
    
% 1.d bring in the training dataset
training = readtable(append('inputs/pm25/training_datasets/',... 
    'annual', '_combined/training_cvfolds.csv'));


% 1.e. extract components
[trainSpace, trainTime, trainPreds, trainAqs, num_points] =  ...
    extract_components(training, 7, time_metric);

%%%% -------------------------------------------- %%%%
%%%% 2: Generate PPD's; loop over models and years %%%%
%%%% --------------------------------------------- %%%%

for i = 1:size(grid,1)
    
    % 2.a. generate model
    [W,RP,sigW,Zs,Zt,piZ,mse] = BNE_v1(trainAqs, trainSpace, trainTime, trainPreds, ...
        grid.scale_space_w(i), grid.scale_time_w(i), grid.scale_space_rp(i), grid.scale_time_rp(i), ...
        grid.lambda_w(i), grid.lambda_rp(i), time_metric, grid.opt_stage(i), grid.seed(i), 'cv');
        
         % 2.b.loop to generate ppd for each year
  target = training;
        
        % 2.c. generate and write ppd summary
        predict_BNE_v1(W,RP,sigW,Zs,Zt,piZ, ...
            target, 10, 'compare obs', 7, ...
            grid.scale_space_w(i), grid.scale_time_w(i), grid.scale_space_rp(i), ...
            grid.scale_time_rp(i), time_metric, ...
            'str_uncert_analysis/bne_ppd/annual', ....
            append('predatAQS_', ...
                strrep(num2str(grid.scale_space_w(i)), '.', '-'), '_', ...
                strrep(num2str(grid.scale_time_w(i)), '.', '-'), '_', ...
                strrep(num2str(grid.scale_space_rp(i)), '.', '-'), '_', ...
                strrep(num2str(grid.scale_time_rp(i)), '.', '-'), '_', ...
                strrep(num2str(grid.scale_time_rp(i)), '.', '-'), '_', ...
                strrep(num2str(grid.lambda_w(i)), '.', '-'), '_', ...
                strrep(num2str(grid.lambda_rp(i)), '.', '-')))


    
end


