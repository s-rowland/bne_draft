[W] = cross_validate_BNE_spt(3.5, 0.008)


for len_scale_space = 4:-0.5:3
    for len_scale_time = 0.009:-0.001:0.007
        [W] = cross_validate_BNE_spt(len_scale_space, len_scale_time)
    end
end