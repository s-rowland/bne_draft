mkdir(inputset);
writematrix(softmax_mean, append(inputset, '/w_mean_', YYYY, '_',inputset, '_', KscaleStr,'.csv'))
writematrix(softmax_std, append(inputset, '/w_std_', YYYY, '_',inputset, '_', KscaleStr,'.csv'))
writematrix(bias_mean, append(inputset, '/bias_mean_', YYYY, '_',inputset, '_', KscaleStr,'.csv'))
writematrix(bias_std, append(inputset, '/bias_std_', YYYY, '_',inputset, '_', KscaleStr,'.csv'))
writematrix(y_mean, append(inputset, '/y_mean_', YYYY, '_',inputset, '_', KscaleStr,'.csv'))
writematrix(y_std, append(inputset, '/y_std_', YYYY, '_', inputset, '_', KscaleStr, '.csv'))