# -*- coding: utf-8 -*-
"""
Created on Wed Jul 17 00:40:47 2019

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