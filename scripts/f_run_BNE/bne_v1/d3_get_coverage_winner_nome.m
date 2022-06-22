
%%%% -------------------------------- %%%%
%%%%  0: Set winning hyperparameters  %%%%
%%%% -------------------------------- %%%%
    
num_models = 6; 
scale_space_w = 2; 
scale_time_w = 1; 
scale_space_rp = 2; 
scale_time_rp = 1; 
lambda_w = 0.0498; 
lambda_rp = 0.3679; 
time_metric = 'year'; 
opt_stage = 2; 
seed = 1234;
    
%%%% ------------------- %%%%
%%%%  1: Set fold table  %%%%
%%%% ------------------- %%%%

% 1.a. make initial vectors
fold = transpose([1:10]); 
pc_95 = transpose(repelem(0, 10));
pc_90 = transpose(repelem(0, 10));
pc_85 = transpose(repelem(0, 10));
pc_80 = transpose(repelem(0, 10));
pc_75 = transpose(repelem(0, 10));
pc_70 = transpose(repelem(0, 10));

% 1.b combine into a table
fold_table = table(fold, pc_95, pc_90, pc_85, pc_80, pc_75, pc_70, ...
    'VariableNames', ... 
    {'fold', 'pc_95', 'pc_90', 'pc_85', 'pc_80', 'pc_75', 'pc_70'});

%%%% ------------------------------------ %%%%
%%%%  2: Calculate coverage in each fold  %%%%
%%%% ------------------------------------ %%%%

% 2.a bring in training data 
training_full = readtable(append('inputs/pm25/training_datasets/',... 
    'annual', '_combined/training_cvfolds_nome.csv'));

for i = 1:10 
    
    [partial_mse, partial_r2, partial_cover, pc_95, pc_90, pc_85, pc_80, pc_75, pc_70] = cv_eval_BNE_v1(training_full,...
        fold_table.fold(i),... 
        num_models, ...
        scale_space_w, scale_time_w, scale_space_rp, scale_time_rp, ...
        lambda_w, lambda_rp, time_metric, opt_stage, seed);

    fold_table.mse(i) = partial_mse ; 
    fold_table.pc_95(i) = pc_95 ; 
    fold_table.pc_90(i) = pc_90 ; 
    fold_table.pc_85(i) = pc_85 ; 
    fold_table.pc_80(i) = pc_80 ; 
    fold_table.pc_75(i) = pc_75 ; 
    fold_table.pc_70(i) = pc_70 ; 
    
end

%%%% ------------------------ %%%%
%%%%  1: aggregate  %%%%
%%%% ------------------------ %%%%
cover_95 = sum(fold_table.pc_95);
cover_90 = sum(fold_table.pc_90);
cover_85 = sum(fold_table.pc_85);
cover_80 = sum(fold_table.pc_80);
cover_75 = sum(fold_table.pc_75);
cover_70 = sum(fold_table.pc_70);

