%[W] = cross_validate_BNE_spt(3.5, 0.008)

len_scale_space_list = {10, 3.5, 0.5, 0.1};
len_scale_time_list = {50, 10, 1, 0.1, 0.01, 0.001, 0.0001, 0.00001};

for j = 1:length(len_scale_space_list)
    for i= 1:length(len_scale_time_list)
        [W] = cross_validate_BNE_spt(len_scale_space_list{j}, len_scale_time_list{i})
    end
end