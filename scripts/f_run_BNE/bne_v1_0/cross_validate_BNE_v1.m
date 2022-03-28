function [MSE] = cross_validate_BNE_v1(window, num_models, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)


 % window = 'daily'; num_models = 5; 
 % len_scale_space = 3.5'; len_scale_time = 20; len_scale_space_bias = 3.5';
 % len_scale_time_bias = 20; penalty = 0.1; time_metric = 'julianDay'; seed = 1234;
 % yyyy_start = 2005; yyyy_end=2006; dir_out = 'test_run';

mse = [0 0];
mse = [mse train_test_BNE_v1(window, num_models, 1, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)];
display('fold 01 done')
mse = [mse train_test_BNE_v1(window, num_models, 2, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)];
display('fold 02 done')
mse = [mse train_test_BNE_v1(window, num_models, 3, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)];
display('fold 03 done')
mse = [mse train_test_BNE_v1(window, num_models, 4, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)];
display('fold 04 done')
mse = [mse train_test_BNE_v1(window, num_models, 5, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)];
display('fold 05 done')
mse = [mse train_test_BNE_v1(window, num_models, 6, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)];
display('fold 06 done')
mse = [mse train_test_BNE_v1(window, num_models, 7, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)];
display('fold 07 done')
mse = [mse train_test_BNE_v1(window, num_models, 8, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)];
display('fold 08 done')
mse = [mse train_test_BNE_v1(window, num_models, 9, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)];
display('fold 09 done')
mse = [mse train_test_BNE_v1(window, num_models, 10, ...
    len_scale_space,len_scale_time,len_scale_space_bias,len_scale_time_bias, ...
    penalty, time_metric, seed, yyyy_start, yyyy_end, dir_out, training_original)];
display('fold 10 done')
MSE = sqrt(sum(mse))
