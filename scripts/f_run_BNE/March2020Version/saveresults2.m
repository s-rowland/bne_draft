 
results = [X, softmax_mean, softmax_std,bias_mean, bias_std, y_mean, y_std];

writematrix(results, append(YYYY, '_', inputset, '_', string(Kscale),'_', 'all', '_', 'all', '.csv'))
