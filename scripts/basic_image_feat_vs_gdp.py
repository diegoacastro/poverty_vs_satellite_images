# -*- coding: utf-8 -*-
"""
Created on Tue Jul 16 10:38:34 2019

@author: diego
"""

### Libraries --------------------------------------------------------------

import plaidml.keras
plaidml.keras.install_backend()
import pandas as pd
import numpy as np
from sklearn.model_selection import KFold
from sklearn.linear_model import Ridge


### Load features ----------------------------------------------------------

#features_basic = np.loadtxt('input/model/google_image_features_basic.csv')
features_basic = pd.read_csv('input/model/google_image_features_basic.csv', 
                             header=None,
                             sep=' ')


### Load nightlights -------------------------------------------------------

nightlights = pd.read_csv('input/model/download_coordinates.txt')
nightlights = nightlights[['row_raster', 'col_raster', 'city_code']]


### Load gdp_per_capita ------------------------------------------------------------

income = pd.read_csv('input/model/gdp_per_capita_df.txt')
income = income[income['state'] == 'RS']
income = income[['city_code', 'gdp_per_capita']]


### Joining dataframes -----------------------------------------------------

images_lights = pd.merge(features_basic, nightlights, how='inner', 
                         left_on=[0, 1], right_on=['row_raster', 'col_raster'])

images_lights = images_lights.drop([0, 1, 'row_raster', 'col_raster'], axis=1)

images_lights = images_lights.groupby('city_code', as_index=False).mean()

images_income = pd.merge(images_lights, income, how='inner', on='city_code')

images_income = images_income.drop(['city_code'], axis=1)

images_income = images_income.to_numpy()


### Save array ------------------------------------------------------------

#np.savetxt('input/model/gdp_daytime.csv', images_income)


### Load data -------------------------------------------------------------

data_all = np.loadtxt('input/model/gdp_daytime.csv')


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
