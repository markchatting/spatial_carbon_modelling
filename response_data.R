########################################################################################

#	Response Variable

########################################################################################

#Extract response data from various sources

#Natura
setwd("~/UCD/datasets")				
natura_psa <- readOGR(dsn = "PSA", layer = "Natura_PSA")

#Mason et et al. (2017) aka Diesing et al. (2017)
setwd("~/UCD/datasets")
mason <- readOGR(dsn = "Mason_2017", layer = "Recordset_9483Point")
mason@data$RelativeDe <- as.factor(mason@data$RelativeDe)
masondf <- mason@data[mason@data$RelativeDe == "surface", ]

#Marine Institute
data_dirs <- list.files("~/UCD/datasets/MI_Irish_sea_PSA", full.names=T) #These 2 directories are the same!!!
out_list <- list()
for(j in 1:length(data_dirs)){
	data_files <- list.files(data_dirs[[j]], full.names=T)
	my_list <- list()
	for(i in 1:length(data_files)){
		my_df <- read_excel(data_files[i])
		my_list[[i]] <- data.frame(my_df$Longitude, my_df$Latitude, my_df$loi_percentage_450degrees)
	}
	out_list[[j]] <- my_list
	print(data_dirs[[j]])
}
str(out_list)
new_df <- data.frame(rbind(out_list[[1]][[1]], out_list[[1]][[2]], out_list[[1]][[3]], out_list[[1]][[4]], 
			out_list[[1]][[5]], out_list[[1]][[6]], out_list[[1]][[7]], out_list[[1]][[8]], 
			out_list[[1]][[9]]))
names(new_df) <- c("lon", "lat", "loi")

#INFOMAR
infomar <- read.csv("infomar.csv", header = T)

#Scottish Blue Carbon Forum aka Smeaton et al. (2021)
setwd("~/UCD/datasets/ScottishBlueCarbonForum")
sbf_1 <- read.csv("Sedimentary_OC_Quality_Reactivity.csv", header=T, fileEncoding="latin1")
sbf_2 <- read.csv("SBF_INFOMAR.csv", header=T, fileEncoding="latin1")
sbf_3 <- read.csv("SBF_primary.csv", header=T, fileEncoding="latin1")

#HABMAP Natural Resources Wales aka McBreen et al. (2008)
setwd("~/UCD/datasets/HABMAP")
habmap <- read.csv("HABMAP.csv", header=T, sep=",")


#Combining all data into one dataframe
train_df <- data.frame(
			rbind(
				cbind(infomar$Longitude, infomar$Latitude, infomar$TOC, rep("infomar", times=nrow(infomar))), 
				cbind(natura_psa@data$Longitude, natura_psa@data$Latitude, (natura_psa@data$LOI_pc/1.724), rep("natura_psa", times=nrow(natura_psa@data))),
				cbind(masondf$Longitude, masondf$Latitude, masondf$X2mm_OC__, rep("mason", times=nrow(masondf))),
				cbind(new_df$lon, new_df$lat, (new_df$loi/1.724), rep("mi_psa", times=nrow(new_df))),
				cbind(sbf_1$Longitude, sbf_1$Latitude, sbf_1$OC_., rep("SBF_2021", times=nrow(sbf_1))),
				cbind(sbf_2$Dec..Long, sbf_2$Dec..Lat, sbf_2$OC...., rep("SBF_INFOMAR", times=nrow(sbf_2))),
				cbind(sbf_3$Dec..Long, sbf_3$Dec..Lat, sbf_3$OC...., rep("SBF_primary", times=nrow(sbf_3))),
				cbind(habmap$lon, habmap$lat, habmap$OrgCarbon., rep("HABMAP", times=(nrow(habmap))))
				)
			)
names(train_df) <- c("lon", "lat", "carbon", "source")

#	LOI / 1.724 converts LOI to TOC (Frqangipane et al. 2009)


train_df$lon <- as.numeric(train_df$lon)
train_df$lat <- as.numeric(train_df$lat)
train_df$carbon <- as.numeric(train_df$carbon)
train_df$source <- as.factor(train_df$source)
train_df <- na.omit(train_df)

train_df <- train_df[, 1:3]
str(train_df)


setwd("~/UCD/model_training")
write.csv(train_df, "training_data.csv", row.names=F)

GB_and_I <- read_sf("~/QU/GIS shapefiles/GB_I_Isles/GB_I_Isles.shp")

my_sf <- st_as_sf(train_df, coords=c("lon", "lat"))
st_crs(my_sf) <- "+proj=longlat +datum=WGS84"
sampling_plot <- ggplot(my_sf) + geom_sf(aes(color=carbon), size=0.15) +
					xlab("") + ylab("") +
					theme_bw() +
					theme(axis.line = element_blank(),
    				panel.grid.major = element_blank(),
    				panel.grid.minor = element_blank(),
			    	panel.border = element_blank(),
			    	panel.background = element_blank(),
			    	plot.margin=unit(c(-0.5,-0.5,-0.5,-0.5), "cm")
			        ) +
			    geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=TRUE)
    
setwd("~/UCD/model_training")
tiff("Sampling_data_locations.tiff", units="cm", width=10, height=10, res=200)
sampling_plot
dev.off()
