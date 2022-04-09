# Replication Code: Can Satellite Imagery Predict Poverty in Brazil? A Transfer Learning Approach

## Introduction

This document shows how to replicate the results of the dissertation "Can Satellite Imagery Predict Poverty in Brazil? A Transfer Learning Approach". This work was the basis for the paper "Predicting socioeconomic indicators using transfer learning on imagery data: an application in Brazil" published in GeoJournal (<https://link.springer.com/article/10.1007/s10708-022-10618-3>).

The steps bellow show how to reproduce the results (some adjusts may be necessary due to changes in data files).

## Download data

+ Donwload nighttime lights data: https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html
+ Download poverty proxies: 
	+ https://sidra.ibge.gov.br/  (number of the tables as on the report)
	+ http://app4.cidades.gov.br/serieHistorica/ (in the link "municipios", download the variables IN049_AE, IN055_AE and IN084_AE)
+ Download shape files for Brazil, Bahia and Rio Grande do Sul: https://portaldemapas.ibge.gov.br/ (click on "Organização do Território" -> "Malhas territoriais" -> "Malha de municípios")
+ Download file with geographic coordinates of each city (city centre) of Brazil: ftp://geoftp.ibge.gov.br/estrutura_territorial/localidades/Shapefile_SHP/BR_Localidades_2010_v1.shp

## Save data in the correct directories

First, save files in the correct directory for Rio Grande do Sul:

+ In the folder "RS/input/nightlight", save nighttime light file;
+ In the folder "RS/input/censo", save data from SIDRA;
+ In the folder "RS/input/SNIS", save data for water index;
+ In the folder "RS/input/coordenadas", save shapefile with geographic coordinates of each city;
+ In the folder "RS/input/shapes/RS/cities", save shapefile for Rio Grande do Sul called "43MUE250GC_SIR.shp" (and auxiliar files);
+ In the folder "RS/input/shapes/BR/BR_cities/", save shapefile with all Brazilian cities called "BRMUE250GC_SIR.shp" (and auxiliar files);

## Run the scripts in the correct order

Run the scripts to generate results for Rio Grande do Sul in the following order:

+ Create an R project called RS in the directory "RS";
+ In the folder "RS/scripts", run: 
	+ the three R codes that start with "prepare_data_";
	+ "get_nightlight_coordinates.R";
	+ "associate_cities_nightlights.R";
	+ "associate_images_income_city.R";
	+ "get_all_br_cities_coordinates.R";
	+ "nearest_nightlights_per_city.R";
	+ "nearest_nightlights_per_city_BRAZIL.R";
	+ "GMM_nightlights.py" (to define classes for nightime lights);
	+ "get_thresholds_to_split_classification.R" (to define how to split classes into low and high income);

## Get Google Maps Static images

After preparing the data for running the models, it is necessary to download the Google Maps Static images:

+ In the folder "RS/scripts", run "download_google_images.py";

## Extract features and evaluate models

Extract features for Images Basic Features, VGG16 and transfer learning, running:

+ "extract_basic_features_only_daytime.py"
+ "extract_cnn_vgg_features.py"
+ "extract_cnn_transfer_learning_features.py"

Run model to get evalution metrics:

+ "evaluate_all_models.R"


## Similar steps for Bahia

For Bahia, first:

+ In the folder "BA/input/shapes/BA/cities", save shapefile for Bahia called "29MUE250GC_SIR.shp" (and auxiliar files);

Then, create an R project called BA in the directory "BA" and run the scripts inside the folder "scripts" following the same order as in Rio Grande do Sul.

Finally, run the codes inside the folder "BA_using_RS_weights" and "RS_using_BA_weights" to get the evaluation metrics for the cross-state analysis.


