library(tidyverse)
library(psych)
library(raster)
library(ggcorrplot)
library(cptcity)
library(cowplot)
library(colorspace)
library(rasterVis)

# 1.Lectura de datos espaciales -------------------------------------------
reg <- st_read("new_reg.gpkg")

via <- raster("vias_teu_costo.sdat") %>% crop(reg) %>% mask(reg)
roam <- raster("roam_costo.sdat") %>% crop(reg) %>% mask(reg)
polos <- raster("polos_costo.sdat") %>% crop(reg) %>% mask(reg)
luces <- raster("LUCES_costo.sdat") %>% crop(reg) %>% mask(reg)
cp <- raster("cp_costo.sdat") %>% crop(reg) %>% mask(reg)
cobAg <- raster("cobert_agric_costo.sdat") %>% crop(reg) %>% mask(reg)
atTuris <- raster("atrac_turis_costo.sdat") %>% crop(reg) %>% mask(reg)
arCons <- raster("Areas_conserv_costo.sdat") %>% crop(reg) %>% mask(reg)
anp <- raster("ANP.sdat") %>% crop(reg) %>% mask(reg)
defo <- raster("defo_ali_1.tif") %>% crop(reg) %>% mask(reg)
defo<-  raster::resample(defo,via)
input<-  stack(via, roam, polos, luces, cp, cobAg, atTuris, arCons, anp, defo)


# 2. Escalado de datos ----------------------------------------------------
input_df <- input %>% 
  as.data.frame(xy = TRUE)

xy <- input_df %>% 
  dplyr::select(x,y)

input_value <- input_df %>% 
  drop_na() %>% 
  scale(center = TRUE)

matrix_cor <- input_value %>%
  dplyr::select(-c(x,y)) %>% 
  cor()

# (measure of sampling adequacy, MSA >=7)
KMO(r = matrix_cor) 
ggcorrplot(corr = matrix_cor,
           ggtheme = theme_minimal(),
           title = 'Matrix of variables correlation',
           lab = TRUE,
           colors = cpt(pal = 'mpl_viridis',n=3),
           lab_col = 'white')

# 3.PCA -------------------------------------------------------------------
new_pca <- prcomp(input_value,center = FALSE)
summary(new_pca)
# Kaiser criterion var > 80%
componentes <- newpca$x[,1:4]

dataset_pcs <- cbind(input_value,san_diego$x[,1:3])
img_pca <- xy %>%
  left_join(y = input_value,by = c('y','x'))

newraster <- img_pca %>%
  dplyr::select(x, y, PC1, PC2, PC3)

img_crs  <- crs(input)
img_res <- res(input)
newraster <- rasterFromXYZ(
  newraster,
  img_res,
  img_crs
)

# Export new pca image ----------------------------------------------------
writeRaster(newraster,'./def_pca.tif',overwrite = TRUE)