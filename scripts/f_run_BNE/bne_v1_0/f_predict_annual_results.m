
% 3: Generate predictions
% 3a bring in the dataset

addpath("scripts/f_run_BNE/bne_v1_0/")
% 1 make grid search table
% 1a set parameter values we will consider
yyyy_list = [2010, 2011, 2012, 2013, 2014, 2015]; %:2016;

% 1 make grid search table
% 1a set parameter values we will consider
len_scale_space_w_list = [2];
len_scale_time_w_list = [2];
len_scale_space_bias_list = [2];
len_scale_time_bias_list = [1, 2];
penalty_list = [0.3679,  0.0498];
time_metric_list = [1]; %['julianDay', 'dayOfYear'];
seed_list = [1234];
stage_list = [2];

% 1b actually make the table
% 1b.i get all the combinations
grid_mat = combvec(len_scale_space_w_list, len_scale_time_w_list, ...
    len_scale_space_bias_list, len_scale_time_bias_list, penalty_list, ...
   time_metric_list, seed_list, stage_list).';
% 1b.ii put them in a nice labeled table
grid = table;
grid.len_scale_space = grid_mat(:,1);
grid.len_scale_time = grid_mat(:,2);
grid.len_scale_space_bias = grid_mat(:,3);
grid.len_scale_time_bias = grid_mat(:,4);
grid.penalty = grid_mat(:,5);
grid.penalty_bias =grid_mat(:,5);
grid.time_metric = grid_mat(:,6);
grid.seed = grid_mat(:,7);
grid.stage = grid_mat(:,8);
grid.mse = transpose(repelem(0, size(grid,1)));


target2 = readtable(append('inputs/pm25/training_datasets/','annual', '_combined/training_cvfolds.csv'));
target2.day_of_year = transpose(repelem(0, size(target2,1)));

for i = 1:size(grid,1)
    
        if grid.time_metric(i) == 1
            time_metric_act = 'julianDay';
        else time_metric_act = 'dayOfYear';
        end
        [W,w0,SigW,Z,piZ,Zt,MSE] = train_BNE_v1('annual', 7, ...
            grid.len_scale_space(i), grid.len_scale_time(i), ...
            grid.len_scale_space_bias(i), grid.len_scale_time_bias(i), ...
            grid.penalty(i), grid.penalty_bias(i), time_metric_act, ...
            grid.stage(i), grid.seed(i));
        
        
        
        
    for yyyy = 2010:2015
        target =readtable(append('inputs/pm25/prediction_datasets/annual_individual/preds_annual_', ...
            num2str(yyyy), '.csv'));
       % target = target(idx, :);

            % features. (The randomness happens once at the beginning)

    % 3b generate and save predictions
        target = target(1:1000,:);

        predict_BNE_v1(W,w0,SigW,Z,piZ,Zt,...
            target, 'pred_timeSlice', 7, ...
            grid.len_scale_space(i), grid.len_scale_time(i),...
            grid.len_scale_space_bias(i), grid.len_scale_time_bias(i), time_metric_act, ...
            'str_uncert_analysis/BNE_ppd/annual/preds', ...
            append('Annualpreds_', num2str(yyyy), '_', ...
            strrep(num2str(grid.len_scale_time_bias(i)), '.', '-'), '_', ...
            strrep(num2str(grid.penalty(i)), '.', '-')))

    end

            predict_BNE_v1(W,w0,SigW,Z,piZ,Zt,...
            target2, 'extVal', 7, ...
            grid.len_scale_space(i), grid.len_scale_time(i),...
            grid.len_scale_space_bias(i), grid.len_scale_time_bias(i), time_metric_act, ...
            'str_uncert_analysis/BNE_ppd/annual/coverage_test', ...
            append('AnnualpredsCover_', num2str(yyyy), '_', ...
            strrep(num2str(grid.len_scale_time_bias(i)), '.', '-'), '_', ...
            strrep(num2str(grid.penalty(i)), '.', '-')))
    %display(num2str(i))
    
end


