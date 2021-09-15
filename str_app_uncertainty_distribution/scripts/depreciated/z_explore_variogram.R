
# 0a Load package required for this script
if(!exists("Ran_a_00")){
  here::i_am("README.md")
  source(here::here('scripts', 'a_set_up', "a_00_set_up_env.R"))
}

if(!exists("Ran_a_00_ISEE")){
  here::i_am("README.md")
  source(here::here('uncertainty_factor_analysis_ISEE', 'scripts', 
                    "a_00_set_up_env_ISEE.R"))
}

# 0b Read CONUS shape file 
conus <-   st_read(here::here('ancillary_data', 'formatted', 'spatial_outlines', 
                              'conus.shp'))

# 1a Read Data
readCombinedData <- function(YYYY){
  read_fst(here::here('uncertainty_factor_analysis_ISEE', 'data', 'processed', 
                      paste0('AVGSCMJSCC_3.5_explanatory_', YYYY, '_025deg.fst'))) %>% 
    mutate(YYYY = YYYY) 
}
dta <- map(2010:2015, readCombinedData) %>% 
  bind_rows()
rm(readCombinedData)

# 1b Convert units of population density to thousand ppl per km 
dta <- dta %>% 
  mutate(pop_density_1kper1000km2 = pop_density*1000000 / 1000) %>% 
  mutate(lat = as.numeric(as.character(lat)), 
         lon = as.numeric(as.character(lon))) 

# 1d Average across years 
dta <- dta %>% 
  dplyr::select(-zcta, -state_num, -YYYY) %>%
  group_by(lat, lon, region, state) %>% 
  summarize_all(mean) %>% 
  ungroup() 
mp <- function(x){round(x, 2)}
# 1e Scale all of the variables... yes! 
# Let's not scale, it makes it a bit harder to interpret the plots
#dta <- dta %>% 
#mutate_at(vars(-lat, -lon, -state, -region), scale) 

# remove rows with missing variables
dta <- dta %>% 
  filter(complete.cases(dta))

# Calculate distance to each near monitor
dta <- dta %>% 
  mutate(monDist1 = monDist1.mean, 
         monDist2 = 2*monDist2.mean-monDist1.mean, 
         monDist3 = 3*monDist3.mean-2*monDist2.mean, 
         monDist4 = 4*monDist4.mean-3*monDist3.mean, 
         monDist5 = 5*monDist5.mean-4*monDist4.mean)
print('Distribution of Predictive Uncertainty')
data.frame(mean = mean(dta$pred_sd), sd = sd(dta$pred_sd),
           min = min(dta$pred_sd), Q1 = quantile(dta$pred_sd, 0.25), 
           median = median(dta$pred_sd),Q3 = quantile(dta$pred_sd, 0.75), 
           max = max(dta$pred_sd)) %>% 
  mutate_all(mp)

dta.dist <- dta %>% 
  dplyr::select(pred_sd, monDist1, monDist2, monDist3, monDist4, monDist5)
dta.dist.cor <- cor(dta.dist)

corrplot(dta.dist.cor, method = 'number', type = 'lower')

mod.allPsp <- gam(pred_sd ~ s(pred_mean) + s(monDist1)  +  s(winter_temp) + s(summer_temp) + s(pop_density_1kper1000km2) + s(cloud_cover) + s(elev) , data = dta)
summary(mod.allPsp)$s.table[,1]


mod.nonsp <- gam(pred_sd ~ s(pred_mean) + s(monDist1)  +  s(winter_temp) + s(summer_temp) + pop_density_1kper1000km2 + s(cloud_cover) + s(elev) , data = dta)
par(mar = c(5, 5, 5,5))
plot(mod.nonsp, rug = TRUE, select = 2, 
     xlab = 'Distance to Nearest Monitor (km)', 
     ylab = expression('Predictive Uncertainty ('*mu*g/m^3*')'), 
     main = 'Distance to Nearest Monitor and Uncertainty', 
     cex.lab = 1.4, cex.axis = 1.4, cex.main = 1.4)



# 3a Put the residuals in the dataframe 
dta$resid <- mod.nonsp$residuals

# 3b Calculate Moran's I of the residuals 
# 3b.i Create distance matrix
dta.dists <- as.matrix(dist(cbind(dta$lon, dta$lat)))
# 3b.ii Compute the inverse of the distance
dta.dists.inv <- 1/dta.dists
# 3b.iii Make all of the diagonals (self-distance) zero
diag(dta.dists.inv) <- 0
# 3b.iv Calculate Moran's I 
Moran.I(dta$resid, dta.dists.inv)

# 3b Calculate distances
dists <- dplyr::select(dta, lat, lon)
summary(dists) 
#dta <- dta %>% 
# mutate(lat = lat/1000, lon = lon/1000)
dta$rnd <- rnorm(nrow(dta), 0, 3)

# convert to a Spatial points data frame 
# first convert to sf 
dta.sf <- dta %>% 
  st_as_sf( coords = c("lon", "lat"), 
            crs=st_crs(projString))
# next convert to SP 
dta.sp <- dta.sf %>% 
  as("Spatial")

# okay now we can do variogram 
plot(dta.sp)

p_load(gstat)
prop_cor <- variogram(resid~1, data = dta.sp) #, 
                      #cutoff = 2704168, width = 200000)
# note that variogram() has a spatiotemporal version called variogramST()

plot(prop_cor, 
     xlab = 'Distance (m)')
par(mfrow)
# ah, okay, now I understand. 
# the variogram function only measures the semi-variance up to a certain distance 
# so we still have 13 bins, but those bins are focused on nearby points. 
# this avoids some structural problems in terms of lack of support. 
# But otherwise the plots are showing the same thing. 
# confirmed, if I change the cut-off for the viog function, I see the same thing. 
# we can use the cutoff function to change the maximum distance . 
# cutoff: spatial separation distance up to which point pairs are included in 
# semivariance estimates; as a default, the length of the diagonal of the box 
# spanning the data is divided by three.

max(prop_cor$dist) 
hist(dta.dists) 
abline(v=max(prop_cor$dist) )
median(dta.dists)

prop_cor_fit <- fit.variogram(prop_cor, vgm(c("Sph","Exp","Mat")))
prop_cor_fit


?variogram
dta.km <- dta %>% 
  mutate(lat=lat/1000, lon = lon/1000)
a <- variog( coords = dplyr::select(dta, lat, lon),
            data = dta$pred_sd, uvec = 'default', 
            max.dist = max(prop_cor$dist))
plot(a, xlab ='distance (km)')

hist(dta.dists)
# okay, yes, I think that  last one is just due to too small of dta 
# and strucutall, just referes to compares Fl and Washington and 
# Main and SoCal. 
# so its not exactly that. 

FLWA <- dta.km %>% 
  filter(state %in% c('FL', 'WA'))
dist.FLWA <- as.matrix(dist(cbind(FLWA$lon, FLWA$lat)))
soCAMA <- dta.km %>% 
  filter(state %in% c('CA', 'MA')) %>%
  filter(state == 'MA' | (state == 'CA' &lat < -600 ))
dist.soCAMA <- as.matrix(dist(cbind(soCAMA$lon, soCAMA$lat)))

dgk <- c(as.vector(dist.FLWA), as.vector(dist.soCAMA))
hist(dgk)

# okay, but is there a way we can prove that the 4km distances only come from these 4 states? 
big <- function(x){as.numeric(x >4400)} 

dta.dists <- as.matrix(dist(cbind(dta.km$lon, dta.km$lat)))
dta.dists.dgk <- as.data.frame(dta.dists)
dta.dists.dgk <- dta.dists.dgk%>% 
  mutate_all(big)
dta.dists.dgk$big <- rowSums(dta.dists.dgk)
dta.dists.dgk <- dta.dists.dgk %>% 
  mutate(row_id = row_number())

dta.km <- dta.km %>% 
  mutate(row_id = row_number())

dta.bigDist<- dta.dists.dgk %>% 
  filter(big > 0) %>% 
  inner_join(dta.km, by = 'row_id')
table(dta.bigDist$state)



head(dta.dists.dgk$big)




# okay, I think this is okay. 
# note that we can use 
# fit.variogram(v, vgm(c("Exp", "Mat", "Sph"))) 
# to find the best-fitting model to mathematically describe the variogram
coordinates(dta) = ~lat+lon

TheVariogram=variogram(rnd~1, data=dta)
plot(TheVariogram)

TheVariogram=variogram(pred_mean~1, data=dta)
plot(TheVariogram)


#bin1 <- variog(data=dplyr::select(dta, rnd), 
#              coords = dplyr::select(dta, lat, lon), #uvec=seq(0,1,l=11), 
#             option = 'bin')

#plot(bin1, main = "classical estimator")


