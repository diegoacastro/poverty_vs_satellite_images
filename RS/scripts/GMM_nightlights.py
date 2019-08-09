# -*- coding: utf-8 -*-
"""
Created on Wed Aug  7 21:02:40 2019

@author: diego
"""

# Import libraries

import pandas as pd
from sklearn.mixture import GaussianMixture


# Read data

radiance = pd.read_csv('input/model/near_nightlights_GMM.csv')

# Fit GMM model

gmm = GaussianMixture(n_components=3, covariance_type='full').fit(radiance)


# Predict labels

labels = gmm.predict(radiance)


# Bind columns (original DF and labels)

df_labels = pd.concat([radiance, pd.DataFrame(labels)], axis=1, ignore_index=True)


# Observations per class

obs_class = df_labels.groupby(1).agg({0: 'count'})
print(obs_class)


# mean per class

mean_class = df_labels.groupby(1).agg({0: 'mean'})
print(mean_class)


# Max per class

max_class = df_labels.groupby(1).agg({0: 'max'})
print(max_class)

# Min per class

min_class = df_labels.groupby(1).agg({0: 'min'})
print(min_class)