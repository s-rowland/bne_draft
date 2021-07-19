

if(!exists("Ran_a_00")){
  here::i_am("README.rtf")
  source(here::here('scripts', 'a_set_up', "a_00_setUp_env.R"))
}

conus <- st_read(here::here('data_ancillary', 'formatted', 'spatial_outlines', 'conus.shp'), 
                 crs = projString) %>% 
  st_transform(crs= st_crs('epsg:4326'))


conus$geometry

xmin <- -124.7631
xmax <- -66.9499
ymin <- 24.5231
ymax <- 49.38436

xrange <- seq(xmin, xmax, length.out = 10)
yrange <- seq(ymin, ymax, length.out = 10)



# make a bunch of points
x1 <- rbind(c(xrange[1],ymin), c(xrange[1],ymin), c(xrange[1],ymax))
x2 <- rbind(c(xrange[2],ymin), c(xrange[2],ymin), c(xrange[2],ymax))
x3 <- rbind(c(xrange[3],ymin), c(xrange[3],ymin), c(xrange[3],ymax))
x4 <- rbind(c(xrange[4],ymin), c(xrange[4],ymin), c(xrange[4],ymax))
x5 <- rbind(c(xrange[5],ymin), c(xrange[5],ymin), c(xrange[5],ymax))
x6 <- rbind(c(xrange[6],ymin), c(xrange[6],ymin), c(xrange[6],ymax))
x7 <- rbind(c(xrange[7],ymin), c(xrange[7],ymin), c(xrange[7],ymax))
x8 <- rbind(c(xrange[8],ymin), c(xrange[8],ymin), c(xrange[8],ymax))
x9 <- rbind(c(xrange[9],ymin), c(xrange[9],ymin), c(xrange[9],ymax))
x10 <- rbind(c(xrange[10],ymin), c(xrange[10],ymin), c(xrange[10],ymax))

y1 <- rbind(c(xmin,yrange[1]), c(xmin,yrange[1]), c(xmax,yrange[1]))
y2 <- rbind(c(xmin,yrange[2]), c(xmin,yrange[2]), c(xmax,yrange[2]))
y3 <- rbind(c(xmin,yrange[3]), c(xmin,yrange[3]), c(xmax,yrange[3]))
y4 <- rbind(c(xmin,yrange[4]), c(xmin,yrange[4]), c(xmax,yrange[4]))
y5 <- rbind(c(xmin,yrange[5]), c(xmin,yrange[5]), c(xmax,yrange[5]))
y6 <- rbind(c(xmin,yrange[6]), c(xmin,yrange[6]), c(xmax,yrange[6]))
y7 <- rbind(c(xmin,yrange[7]), c(xmin,yrange[7]), c(xmax,yrange[7]))
y8 <- rbind(c(xmin,yrange[8]), c(xmin,yrange[8]), c(xmax,yrange[8]))
y9 <- rbind(c(xmin,yrange[9]), c(xmin,yrange[9]), c(xmax,yrange[9]))
y10 <- rbind(c(xmin,yrange[10]), c(xmin,yrange[10]), c(xmax,yrange[10]))

s11 <- rbind(c(xrange[2],ymin), c(xrange[2],ymin))

# LINESTRING sf object
conusGrid <- sapply(list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                    y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, s11), function(x) st_linestring(x)) %>%
  st_sfc() %>%
  st_sf('geom' = .)

st_crs(conusGrid) = 4326
conusGrid0 <- conusGrid %>% 
    st_intersection(conus) %>% 
  filter(!is.na(g)) %>% 
  dplyr::select(-g, -m) %>% 
  mutate(X = 1)

png('~/Desktop/conusGrid_refGrid.png')
ggplot(conus) + 
  geom_sf(fill = 'grey95')  + 
  geom_sf(data = conusGrid0, aes(fill= X), color = 'black') + 
  theme(legend.position = 'none') +
  theme_void() 
dev.off() 




# for input models 
xrange <- seq(xmin, xmax, length.out = 20)
yrange <- seq(ymin, ymax, length.out = 20)

# make a bunch of points
x1 <- rbind(c(xrange[1],ymin), c(xrange[1],ymin), c(xrange[3],ymax))
x2 <- rbind(c(xrange[2],ymin), c(xrange[2],ymin), c(xrange[4],ymax))
x3 <- rbind(c(xrange[3],ymin), c(xrange[3],ymin), c(xrange[5],ymax))
x4 <- rbind(c(xrange[4],ymin), c(xrange[4],ymin), c(xrange[6],ymax))
x5 <- rbind(c(xrange[5],ymin), c(xrange[5],ymin), c(xrange[7],ymax))
x6 <- rbind(c(xrange[6],ymin), c(xrange[6],ymin), c(xrange[8],ymax))
x7 <- rbind(c(xrange[7],ymin), c(xrange[7],ymin), c(xrange[9],ymax))
x8 <- rbind(c(xrange[8],ymin), c(xrange[8],ymin), c(xrange[10],ymax))
x9 <- rbind(c(xrange[9],ymin), c(xrange[9],ymin), c(xrange[11],ymax))
x10 <- rbind(c(xrange[10],ymin), c(xrange[10],ymin), c(xrange[12],ymax))
x11 <- rbind(c(xrange[11],ymin), c(xrange[11],ymin), c(xrange[13],ymax))
x12 <- rbind(c(xrange[12],ymin), c(xrange[12],ymin), c(xrange[14],ymax))
x13 <- rbind(c(xrange[13],ymin), c(xrange[13],ymin), c(xrange[15],ymax))
x14 <- rbind(c(xrange[14],ymin), c(xrange[14],ymin), c(xrange[16],ymax))
x15 <- rbind(c(xrange[15],ymin), c(xrange[15],ymin), c(xrange[17],ymax))
x16 <- rbind(c(xrange[16],ymin), c(xrange[16],ymin), c(xrange[18],ymax))
x17 <- rbind(c(xrange[17],ymin), c(xrange[17],ymin), c(xrange[19],ymax))
x18 <- rbind(c(xrange[18],ymin), c(xrange[18],ymin), c(xrange[20],ymax))
x19 <- rbind(c(xrange[19],ymin), c(xrange[19],ymin), c(xrange[20],ymax))

y1 <- rbind(c(xmin,yrange[1]), c(xmin,yrange[1]), c(xmax,yrange[2]))
y2 <- rbind(c(xmin,yrange[2]), c(xmin,yrange[2]), c(xmax,yrange[3]))
y3 <- rbind(c(xmin,yrange[3]), c(xmin,yrange[3]), c(xmax,yrange[4]))
y4 <- rbind(c(xmin,yrange[4]), c(xmin,yrange[4]), c(xmax,yrange[5]))
y5 <- rbind(c(xmin,yrange[5]), c(xmin,yrange[5]), c(xmax,yrange[6]))
y6 <- rbind(c(xmin,yrange[6]), c(xmin,yrange[6]), c(xmax,yrange[7]))
y7 <- rbind(c(xmin,yrange[7]), c(xmin,yrange[7]), c(xmax,yrange[8]))
y8 <- rbind(c(xmin,yrange[8]), c(xmin,yrange[8]), c(xmax,yrange[9]))
y9 <- rbind(c(xmin,yrange[9]), c(xmin,yrange[9]), c(xmax,yrange[10]))
y10 <- rbind(c(xmin,yrange[10]), c(xmin,yrange[10]), c(xmax,yrange[11]))
y11 <- rbind(c(xmin,yrange[11]), c(xmin,yrange[11]), c(xmax,yrange[12]))
y12 <- rbind(c(xmin,yrange[12]), c(xmin,yrange[12]), c(xmax,yrange[13]))
y13 <- rbind(c(xmin,yrange[13]), c(xmin,yrange[13]), c(xmax,yrange[14]))
y14 <- rbind(c(xmin,yrange[14]), c(xmin,yrange[14]), c(xmax,yrange[15]))
y15 <- rbind(c(xmin,yrange[15]), c(xmin,yrange[15]), c(xmax,yrange[16]))
y16 <- rbind(c(xmin,yrange[16]), c(xmin,yrange[16]), c(xmax,yrange[17]))
y17 <- rbind(c(xmin,yrange[17]), c(xmin,yrange[17]), c(xmax,yrange[18]))
y18 <- rbind(c(xmin,yrange[18]), c(xmin,yrange[18]), c(xmax,yrange[19]))
y19 <- rbind(c(xmin,yrange[19]), c(xmin,yrange[19]), c(xmax,yrange[20]))


s11 <- rbind(c(xrange[2],ymin), c(xrange[2],ymin))

# LINESTRING sf object
conusGrid <- sapply(list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, 
                         x11, x12, x13, x14, x15, x16, x17, x18, 
                         y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, 
                         y11, y12, y13, y14, y15, y16, y17, y18, y19, s11),
                    function(x) st_linestring(x)) %>%
  st_sfc() %>%
  st_sf('geom' = .)

st_crs(conusGrid) = 4326
conusGridInput <- conusGrid %>% 
  st_intersection(conus) %>% 
  filter(!is.na(g)) %>% 
  dplyr::select(-g, -m) %>% 
  mutate(X = 1)

png('~/Desktop/conusGrid_inputmodel1.png')
ggplot(conus) + 
  geom_sf(fill = 'grey95')  + 
  geom_sf(data = conusGridInput, aes(fill= X), color = 'black') + 
  theme(legend.position = 'none') +
  theme_void() 
dev.off() 




# now for the fianl image 
pred <- read_csv(here::here('data_input_models', 'combined', 'annual',
                     paste0('Predictions_', 2010, '_' , 'avgscmjscc', '_all.csv')))


pred <- pred %>% 
 st_as_sf(coords = c("lon", "lat"), crs=st_crs("epsg:4326"))

fillScheme <- scale_fill_scico(
  direction = -1, palette = 'hawaii',
  breaks = c(0, round(max(pred$AV)*0.25,2), 
             round(max(pred$AV)*0.5,2), 
             round(max(pred$AV)*0.75,2), 
             round(max(pred$AV),2)),
  limits = c(0, round(max(pred$AV),2)))
colorScheme <- scale_color_scico(
  direction = -1, palette = 'hawaii',
  breaks = c(0, round(max(pred$AV)*0.25,2), 
             round(max(pred$AV)*0.5,2), 
             round(max(pred$AV)*0.75,2), 
             round(max(pred$AV),2)),
  limits = c(0, round(max(pred$AV),2)))

png('~/Desktop/conusGrid_inputmodel1_pred.png')
ggplot(conus) + 
  geom_sf()  + 
  geom_sf(data = pred, aes(fill= AV, color = AV), size = 0.5) + 
  geom_sf(data = conusGridInput, aes(fill= X), color = 'black') + 
  fillScheme + colorScheme +
  theme(legend.position = 'none') +
  theme_void() 
dev.off()





# make a bunch of points
x1 <- rbind(c(xrange[1],ymin), c(xrange[1],ymin), c(xrange[3],ymax))
x2 <- rbind(c(xrange[2],ymin), c(xrange[2],ymin), c(xrange[2],ymax))
x3 <- rbind(c(xrange[3],ymin), c(xrange[3],ymin), c(xrange[1],ymax))
x4 <- rbind(c(xrange[4],ymin), c(xrange[4],ymin), c(xrange[2],ymax))
x5 <- rbind(c(xrange[5],ymin), c(xrange[5],ymin), c(xrange[3],ymax))
x6 <- rbind(c(xrange[6],ymin), c(xrange[6],ymin), c(xrange[4],ymax))
x7 <- rbind(c(xrange[7],ymin), c(xrange[7],ymin), c(xrange[5],ymax))
x8 <- rbind(c(xrange[8],ymin), c(xrange[8],ymin), c(xrange[6],ymax))
x9 <- rbind(c(xrange[9],ymin), c(xrange[9],ymin), c(xrange[7],ymax))
x10 <- rbind(c(xrange[10],ymin), c(xrange[10],ymin), c(xrange[8],ymax))
x11 <- rbind(c(xrange[11],ymin), c(xrange[11],ymin), c(xrange[9],ymax))
x12 <- rbind(c(xrange[12],ymin), c(xrange[12],ymin), c(xrange[10],ymax))
x13 <- rbind(c(xrange[13],ymin), c(xrange[13],ymin), c(xrange[11],ymax))
x14 <- rbind(c(xrange[14],ymin), c(xrange[14],ymin), c(xrange[12],ymax))
x15 <- rbind(c(xrange[15],ymin), c(xrange[15],ymin), c(xrange[13],ymax))
x16 <- rbind(c(xrange[16],ymin), c(xrange[16],ymin), c(xrange[14],ymax))
x17 <- rbind(c(xrange[17],ymin), c(xrange[17],ymin), c(xrange[15],ymax))
x18 <- rbind(c(xrange[18],ymin), c(xrange[18],ymin), c(xrange[16],ymax))
x19 <- rbind(c(xrange[19],ymin), c(xrange[19],ymin), c(xrange[17],ymax))

y1 <- rbind(c(xmin,yrange[1]), c(xmin,yrange[1]), c(xmax,yrange[3]))
y2 <- rbind(c(xmin,yrange[2]), c(xmin,yrange[2]), c(xmax,yrange[4]))
y3 <- rbind(c(xmin,yrange[3]), c(xmin,yrange[3]), c(xmax,yrange[5]))
y4 <- rbind(c(xmin,yrange[4]), c(xmin,yrange[4]), c(xmax,yrange[6]))
y5 <- rbind(c(xmin,yrange[5]), c(xmin,yrange[5]), c(xmax,yrange[7]))
y6 <- rbind(c(xmin,yrange[6]), c(xmin,yrange[6]), c(xmax,yrange[8]))
y7 <- rbind(c(xmin,yrange[7]), c(xmin,yrange[7]), c(xmax,yrange[9]))
y8 <- rbind(c(xmin,yrange[8]), c(xmin,yrange[8]), c(xmax,yrange[10]))
y9 <- rbind(c(xmin,yrange[9]), c(xmin,yrange[9]), c(xmax,yrange[11]))
y10 <- rbind(c(xmin,yrange[10]), c(xmin,yrange[10]), c(xmax,yrange[12]))
y11 <- rbind(c(xmin,yrange[11]), c(xmin,yrange[11]), c(xmax,yrange[13]))
y12 <- rbind(c(xmin,yrange[12]), c(xmin,yrange[12]), c(xmax,yrange[14]))
y13 <- rbind(c(xmin,yrange[13]), c(xmin,yrange[13]), c(xmax,yrange[15]))
y14 <- rbind(c(xmin,yrange[14]), c(xmin,yrange[14]), c(xmax,yrange[16]))
y15 <- rbind(c(xmin,yrange[15]), c(xmin,yrange[15]), c(xmax,yrange[17]))
y16 <- rbind(c(xmin,yrange[16]), c(xmin,yrange[16]), c(xmax,yrange[18]))
y17 <- rbind(c(xmin,yrange[17]), c(xmin,yrange[17]), c(xmax,yrange[19]))
y18 <- rbind(c(xmin,yrange[18]), c(xmin,yrange[18]), c(xmax,yrange[20]))
y19 <- rbind(c(xmin,yrange[19]), c(xmin,yrange[19]), c(xmax,yrange[20]))


s11 <- rbind(c(xrange[2],ymin), c(xrange[2],ymin))

# LINESTRING sf object
conusGrid <- sapply(list(x3, x4, x5, x6, x7, x8, x9, x10, 
                         x11, x12, x13, x14, x15, x16, x17, x18, 
                         y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, 
                         y11, y12, y13, y14, y15, y16, y17, y18,  s11),
                    function(x) st_linestring(x)) %>%
  st_sfc() %>%
  st_sf('geom' = .)

st_crs(conusGrid) = 4326
conusGridInput <- conusGrid %>% 
  st_intersection(conus) %>% 
  filter(!is.na(g)) %>% 
  dplyr::select(-g, -m) %>% 
  mutate(X = 1)

png('~/Desktop/conusGrid_inputmodel2.png')
ggplot(conus) + 
  geom_sf(fill = 'grey95')  + 
  geom_sf(data = conusGridInput, aes(fill= X), color = 'black') + 
  theme(legend.position = 'none') +
  theme_void() 
dev.off() 




# now for the fianl image 
pred <- read_csv(here::here('data_input_models', 'combined', 'annual',
                            paste0('Predictions_', 2010, '_' , 'avgscmjscc', '_all.csv')))


pred <- pred %>% 
  st_as_sf(coords = c("lon", "lat"), crs=st_crs("epsg:4326"))

fillScheme <- scale_fill_scico(
  direction = -1, palette = 'hawaii',
  breaks = c(0, round(max(pred$CM)*0.25,2), 
             round(max(pred$CM)*0.5,2), 
             round(max(pred$CM)*0.75,2), 
             round(max(pred$CM),2)),
  limits = c(0, round(max(pred$CM),2)))
colorScheme <- scale_color_scico(
  direction = -1, palette = 'hawaii',
  breaks = c(0, round(max(pred$CM)*0.25,2), 
             round(max(pred$CM)*0.5,2), 
             round(max(pred$CM)*0.75,2), 
             round(max(pred$CM),2)),
  limits = c(0, round(max(pred$CM),2)))

png('~/Desktop/conusGrid_inputmodel2_pred.png')
ggplot(conus) + 
  geom_sf()  + 
  geom_sf(data = pred, aes(fill= CM, color = CM), size = 0.5) + 
  geom_sf(data = conusGridInput, aes(fill= X), color = 'black') + 
  fillScheme + colorScheme +
  theme(legend.position = 'none') +
  theme_void() 
dev.off()




png('~/Desktop/conusGrid_inputmodel1_final.png')
ggplot(conus) + 
  geom_sf()  + 
  geom_sf(data = pred, aes(fill= AV, color = AV), size = 0.5) + 
  geom_sf(data = conusGrid0, aes(fill= X), color = 'black') + 
  fillScheme + colorScheme +
  theme(legend.position = 'none') +
  theme_void() 
dev.off()

png('~/Desktop/conusGrid_inputmodel2_final.png')
ggplot(conus) + 
  geom_sf()  + 
  geom_sf(data = pred, aes(fill= CM, color = CM), size = 0.5) + 
  geom_sf(data = conusGrid0, aes(fill= X), color = 'black') + 
  fillScheme + colorScheme +
  theme(legend.position = 'none') +
  theme_void() 
dev.off()