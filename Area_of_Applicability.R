########################################################################################

		#		AREA OF APPLICABILITY AOA		#
		
########################################################################################

#Load previously created objects
setwd("~/UCD/model_training")
infer_df <- readRDS("inference_vars.RData")
infer_pts <- readRDS("infer_pts.RData")

train_DI <- trainDI(model = selected_model, train = train_df_i, variables = selected_model$finalModel$xNames)
AOA <- aoa(infer_df, model = selected_model, trainDI = train_DI, variables = selected_model$finalModel$xNames, CVtest = indices$index)
plot(AOA)
outsideAOA <- ifelse(AOA$DI > AOA$parameters$threshold, 1, 0)
outsideAOA_pts <- data.frame(infer_pts[, 1:2], AOA$DI, outsideAOA)

# set up an 'empty' raster, here via an extent object derived from your data
e <- extent(min(outsideAOA_pts$lon), min(outsideAOA_pts$lat), max(outsideAOA_pts$lon), max(outsideAOA_pts$lat))
nCol <- max(outsideAOA_pts$lon) - min(outsideAOA_pts$lon)
nRow <- max(outsideAOA_pts$lat) - min(outsideAOA_pts$lat)
cell_size <- 0.0125

r <- raster(e, ncol=nCol/cell_size, nrow=nRow/cell_size)
proj4string(r) <- CRS("+proj=longlat +datum=WGS84")

# you need to provide a function 'fun' for when there are multiple points per cell
aoa_ras <- rasterize(outsideAOA_pts[, 1:2], r, outsideAOA_pts[, 4], fun = max)
aoa_ras <- resample(aoa_ras, r)
plot(aoa_ras)

setwd("~/UCD/model_training")
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
aoa_plot

setwd("~/UCD/model_training")
tiff("AOA_threshold.tiff", units="cm", width=12, height=14, res=300)
aoa_plot
dev.off()
