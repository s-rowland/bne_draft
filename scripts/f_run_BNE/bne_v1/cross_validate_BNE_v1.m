function [rmse, r2, coverage] = cross_validate_BNE_v1(training_full, num_models, ...
        scale_space_w, scale_time_w, scale_space_rp, scale_time_rp, ...
        lambda_w, lambda_rp, time_metric, opt_stage, seed)

%%%% ------------------------ %%%%
%%%%  1: Set fold fold table  %%%%
%%%% ------------------------ %%%%

fold = transpose([1:10]); 
mse = transpose(repelem(0, 10));
r2 = transpose(repelem(0, 10));
cover = transpose(repelem(0, 10));

fold_table = table(fold, mse, r2, cover, ...
    'VariableNames', ... 
    {'fold', 'mse', 'r2', 'cover'});

%%%% ------------------------ %%%%
%%%%  1: Calcualte Metrics in each fold  %%%%
%%%% ------------------------ %%%%

for i = 1:10 
    
    [partial_mse partial_r2 partial_cover] = cv_eval_BNE_v1(training_full,...
        fold_table.fold(i),... 
        num_models, ...
        scale_space_w, scale_time_w, scale_space_rp, scale_time_rp, ...
        lambda_w, lambda_rp, time_metric, opt_stage, seed);

    fold_table.mse(i) = partial_mse ; 
    fold_table.r2(i) = partial_r2 ; 
    fold_table.cover(i) = partial_cover ; 
end

%%%% ------------------------ %%%%
%%%%  1: aggregate  %%%%
%%%% ------------------------ %%%%
   rmse = sqrt(sum(fold_table.mse));
   r2 = sum(fold_table.r2);
   coverage = sum(fold_table.cover);
end
