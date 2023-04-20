# spatial_carbon_modelling
Spatial modelling of organic carbon density in surficial marine sediment. Various shapefiles are needed throughout the scripts. I have included a 'shapefiles' folder in this repo. Make sure when the scripts call a specific shapefile that the filepath mathces your computer.

Scripts should be run in this order:

1)  library_setup.R: Install and import all libraries need for the whole work flow.
  
2)  response_data.R: Extract reponse data (organic carbon % and coordinates) from the various sources of data we have. For example, Infomar, Scottish Blue Carbon Forum, Mason et al. (2017), etc. I have uploaded a folder "datasets" to the QUEST WP2 spatial predictive modelling shared folder. This folder has all the datasets required for this script. WHen importing them into R just make sure the file paths match up with where they are on your computer.
    
3)  Predictor_data.R: Extract predictor data (bathymetry, distance to coast, mud, etc.) from the various sources for the same geographic locations as the response data.

4)  train_model.R 
    
  i) TRAINING DATA FRAME CREATION: Create a training data frame of all predictor and response data and the spatial training folds.

  ii) CARBON DENSITY MODEL TRAINING: Train a model using forward feature selection (FFS) and spatial cross validation using previously created spatial folds.
  
  iii)  PARTIAL PLOTS: Create partial plots of predictors selected by FFS.
  
5)  Inference_predictor_data.R: Extract predictor data (bathymetry, distance to coast, mud, etc.) accross the entire inference area I created around Ireland and the UK.
6)  Inference.R: Use the RF model previously created and the inference predictor data to predict %organic carbon accross the entire inference area.
7)  Area_of_Applicability: Use the CAST aoa() function to check the dissimilarity between training data predictor varaibles and inference data predictor variables.
