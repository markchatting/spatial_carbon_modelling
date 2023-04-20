########################################################################################

		#		AREA OF APPLICABILITY AOA		#
		
########################################################################################

#Load previously created objects needed to run AOA
setwd("~/UCD/model_training") # This is a folder I created on my computer to save objects I created in previous scripts in the repo. CHANGE THIS TO A FOLDER WHERE YOU'D LIKE OBJECTS SAVED ON YOUR COMPUTER
infer_df <- readRDS("inference_vars.RData")
infer_pts <- readRDS("infer_pts.RData")
selected_model <- readRDS("rf_mod_ffs.RData")
train_df <- train_df <- readRDS("train_df.RData")
train_df_i <- train_df[, c(2:14, 17:18)]

train_DI <- trainDI(model = selected_model, train = train_df_i, variables = selected_model$finalModel$xNames)
AOA <- aoa(infer_df, model = selected_model, trainDI = train_DI, variables = selected_model$finalModel$xNames, CVtest = indices$index)
plot(AOA)
outsideAOA <- ifelse(AOA$DI > AOA$parameters$threshold, 1, 0)
outsideAOA_pts <- data.frame(infer_pts[, 1:2], AOA$DI, outsideAOA)

# Since our inference area is an irregular shape, I had to set up an 'empty' raster to project the irregular data onto.
e <- extent(min(outsideAOA_pts$lon), min(outsideAOA_pts$lat), max(outsideAOA_pts$lon), max(outsideAOA_pts$lat))
nCol <- max(outsideAOA_pts$lon) - min(outsideAOA_pts$lon)
nRow <- max(outsideAOA_pts$lat) - min(outsideAOA_pts$lat)
cell_size <- 0.0125 #This is the same cell size I used when creating the inference grid. It can be changed and made higher resolution but that's a trade-off with longer compute times.

r <- raster(e, ncol=nCol/cell_size, nrow=nRow/cell_size)
proj4string(r) <- CRS("+proj=longlat +datum=WGS84")

# When there are multiple predictions per cell I used the maximum value AOA threshold value.
aoa_ras <- rasterize(outsideAOA_pts[, 1:2], r, outsideAOA_pts[, 4], fun = max)
aoa_ras <- resample(aoa_ras, r)
plot(aoa_ras)

#Write the resulting raster to disk
writeRaster(pred_ras, "AOA_ras.tif", options=c("TFW=YES"), overwrite=T)

#Display raster using ggplot
r_points <- rasterToPoints(aoa_ras)
r_df <- data.frame(r_points)
aoa_plot <- ggplot() + geom_tile(data=r_df, aes(x=x, y=y, fill=as.factor(layer))) + 
	theme(axis.title = element_blank()) + scale_fill_discrete(labels=c("No", "Yes")) +
	xlab("") + ylab("") + labs(fill="Above AOA threshold") + 
	theme_bw() +
	theme(axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	plot.margin=unit(c(-0.5,-0.15,-0.5,-0.15), "cm")
        ) + geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=FALSE)

#Write the resulting map to a .tiff file on disk
tiff("AOA_threshold.tiff", units="cm", width=12, height=14, res=300)
aoa_plot
dev.off()

########################################################################################

		#		END!!!		#
		
########################################################################################
