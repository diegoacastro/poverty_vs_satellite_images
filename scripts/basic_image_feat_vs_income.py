# -*- coding: utf-8 -*-
"""
Created on Mon Jul 15 11:06:14 2019

@author: diego
"""

### Libraries --------------------------------------------------------------

import plaidml.keras
plaidml.keras.install_backend()
import time
import os
import os.path
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from sklearn.model_selection import KFold
from sklearn.linear_model import Ridge


### Get files names --------------------------------------------------------

images_name = []
for i in range(3):
    dir_ = 'input/google_images/class' + str(i + 1) + '/'
    image_files = os.listdir(dir_)
    images_name.append(image_files)


### Function to get images features ----------------------------------------
    
# Basically, get the max, min, mean, median and std values of the pixels for
# each colour (RGB)

def get_image_basic_feature(image_file):
    image = plt.imread(image_file)
    features = []
    for i in range(3):
        image_one_band = image[:, :, i].flatten()
        features.append(image_one_band)
    features = np.asarray(features)
    max_ = np.max(features, axis=1)
    min_ = np.min(features, axis=1)
    mean_ = np.mean(features, axis=1)
    median_ = np.median(features, axis=1)
    std_ = np.std(features, axis=1)
    return(np.concatenate([max_, min_, mean_, median_, std_]).tolist())
    

### Get features of all images ---------------------------------------------

feature_all = []
a = 0
t1 = time.time()
for i, images in enumerate(images_name):
    path = 'input/google_images/class' + str(i + 1) + '/'
    for image in images:    
        x, y = [int(idx) for idx in image.split('_')[1:3]]
        file_ = path + image
        feature = get_image_basic_feature(file_)
        feature = [x, y] + feature
        feature_all.append(feature)
        if a % 10000 == 0:
            t2 = time.time()
            print(a)
            print(t2 - t1)
            t1 = time.time()
        a += 1

feature_all = np.asarray(feature_all)

### Save features ----------------------------------------------------------
#np.savetxt('input/model/google_image_features_basic.csv', feature_all)


### Load features ----------------------------------------------------------

#features_basic = np.loadtxt('input/model/google_image_features_basic.csv')
features_basic = pd.read_csv('input/model/google_image_features_basic.csv', 
                             header=None,
                             sep=' ')


### Load nightlights -------------------------------------------------------

nightlights = pd.read_csv('input/model/download_coordinates.txt')
nightlights = nightlights[['row_raster', 'col_raster', 'city_code']]


### Load income ------------------------------------------------------------

income = pd.read_csv('input/model/income_df.txt')
income = income[income['state'] == 'RS']
income = income[['code', 'income']]


### Joining dataframes -----------------------------------------------------

images_lights = pd.merge(features_basic, nightlights, how='inner', 
                         left_on=[0, 1], right_on=['row_raster', 'col_raster'])

images_lights = images_lights.drop([0, 1, 'row_raster', 'col_raster'], axis=1)

images_lights = images_lights.groupby('city_code', as_index=False).mean()

images_income = pd.merge(images_lights, income, how='inner', 
                         left_on='city_code', right_on='code')

images_income = images_income.drop(['city_code', 'code'], axis=1)

images_income = images_income.to_numpy()


### Save array ------------------------------------------------------------

#np.savetxt('input/model/Income_daytime.csv', images_income)


### Load data -------------------------------------------------------------

data_all = np.loadtxt('input/model/Income_daytime.csv')


### Fit a model of income as a function of basic daytime features ---------

alphas_list = np.logspace(-2, 3, 50)
final = []

for alpha in alphas_list:
    kf = KFold(n_splits=10, shuffle=True)
    scores = []
    for train_index, test_index in kf.split(data_all):
        reg = Ridge(alpha=alpha)
        train = data_all[train_index]
        test = data_all[test_index]
        reg.fit(train[:, :-1], train[:, -1])
        s = reg.score(test[:, :-1], test[:, -1])
        scores.append(s)
    final.append(np.mean(scores))

print('R^2 of the best model: {:.3f}'.format(np.max(final)))