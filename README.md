# spatial_carbon_modelling
Spatial modelling of organic carbon density in surficial marine sediment. Various shapefiles are needed throughout the scripts. I have included a 'shapefiles' folder in this repo. Make sure when the scripts call a specific shapefile that the filepath mathces your computer.

Scripts should be run in this order:

1)  library_setup.R
  
2)  response_data.R 
    
  i)  I have uploaded a folder "datasets" to the QUEST WP2 spatial predictive modelling shared folder. This folder has all the datasets required for this        script.
    
3)  Predictor_data.R
4)  train_model.R
5)  Inference_predictor_data.R
6)  Inference.R
7)  Area_of_Applicability
