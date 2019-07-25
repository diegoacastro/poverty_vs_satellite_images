# -*- coding: utf-8 -*-
"""
Created on Wed Jul 17 22:43:30 2019

@author: diego
"""

### Libraries ---------------------------------------------------------------

import plaidml.keras
plaidml.keras.install_backend()
from keras.applications.vgg16 import VGG16
from keras.preprocessing import image
from keras.models import Model
from keras.applications.imagenet_utils import preprocess_input
import pandas as pd
import numpy as np


### Read salary_city_files --------------------------------------------------

salary_city_files = pd.read_csv('input/model/salary_city_file.txt')


### Get CNN features --------------------------------------------------------

base_model = VGG16(weights='imagenet')
model = Model(inputs=base_model.input, outputs=base_model.get_layer('fc2').output)

features_final = []
pos = 0

for city in salary_city_files['city_code'].unique():
    features_temp = []
    pos += 1
    print(pos)
    df_filter = salary_city_files[salary_city_files['city_code']==city]
    for i in range(df_filter.shape[0]):
        img = image.load_img(df_filter.iloc[i, [2]][0], target_size=(224, 224))
        x = image.img_to_array(img)
        x = np.expand_dims(x, axis=0)
        x = preprocess_input(x)
        feat_image = model.predict(x)[0]
        features_temp.append(feat_image)
    city_feat = np.append(np.mean(features_temp, axis=0), df_filter.iloc[0, [1]])
    features_final.append(city_feat)
    print(len(features_temp))

features_final = np.asarray(features_final)

df_features_final = pd.DataFrame(data=features_final)


### Write features file -----------------------------------------------------

np.savetxt('input/model/google_image_features_cnn.csv', features_final)

df_features_final.to_csv(path_or_buf='input/model/google_image_features_cnn_df.csv')

