########################################################################################

		#		INFERENCE OVER AREA		#
		
########################################################################################

setwd("~/UCD/model_training")
infer_df <- readRDS("inference_vars.RData")
infer_pts <- readRDS("infer_pts.RData")
selected_model <- readRDS("rf_mod_ffs.RData")

#Predict to inference data using selected model
pred_rf <- predict(selected_model, newdata = infer_df)
pred_df <- data.frame(infer_pts$lon, infer_pts$lat, as.numeric(pred_rf))
names(pred_df) <- c("x", "y", "z")


# set up an 'empty' raster, here via an extent object derived from your data
e <- extent(pred_df[,1:2])
#e <- e + 1000 # add this as all y's are the same
nCol <- max(pred_df$x) - min(pred_df$x)
nRow <- max(pred_df$y) - min(pred_df$y)
cell_size <- 0.0125

r <- raster(e, ncol=nCol/cell_size, nrow=nRow/cell_size)
proj4string(r) <- CRS("+proj=longlat +datum=WGS84")

# you need to provide a function 'fun' for when there are multiple points per cell
pred_ras <- rasterize(pred_df[, 1:2], r, pred_df[,3], fun=mean)
pred_ras <- resample(pred_ras, r)
#plot(pred_ras)

setwd("~/UCD/model_training")
writeRaster(pred_ras, "pred_ras.tif", options=c("TFW=YES"), overwrite=T)

#Display raster using ggplot
r_points <- rasterToPoints(pred_ras)
r_df <- data.frame(r_points)
 quest_plot <- ggplot() + geom_tile(data=r_df, aes(x=x, y=y, fill=layer)) + 
	theme(axis.title = element_blank()) + scale_fill_viridis_c(option = "D", limits=c(0.03, 2.74), na.value="white") +
	xlab("") + ylab("") + labs(fill="Organic carbon %") + 
	theme_bw() +
	theme(axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	plot.margin=unit(c(-0.5,-0.15,-0.5,-0.15), "cm")
        ) + geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=FALSE)
#quest_plot

setwd("~/UCD/model_training")
tiff("QUEST.tiff", units="cm", width=12, height=14, res=300)
quest_plot
dev.off()
