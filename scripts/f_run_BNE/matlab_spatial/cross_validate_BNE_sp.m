
%[W] = train_predict_BNE_sp('2015','avgscmjscc',3.5, 'all')
year_list = {'2010', '2011', '2012', '2013', '2014', '2015'};
fold_list = {'fold01', 'fold02', 'fold03', 'fold04', 'fold05' ...
    'fold06', 'fold07', 'fold08', 'fold09', 'fold10'}

for j =1 %4:length(year_list)
    for i = 1:length(fold_list)
        [W] = train_predict_BNE_sp(year_list{j},'avgscmjsccme',1.5, fold_list{i}, 'resid')
    end
end
%[W] = train_predict_BNE_sp('2012','avgscmjscc',3.5, 'cities')

