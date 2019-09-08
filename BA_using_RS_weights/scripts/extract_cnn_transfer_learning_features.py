# -*- coding: utf-8 -*-
"""
Created on Sun Aug 18 16:19:29 2019

@author: diego
"""

### Libraries --------------------------------------------------------------

import plaidml.keras
plaidml.keras.install_backend()
import keras
from keras.applications.vgg16 import VGG16
from keras.preprocessing import image
from keras.applications.imagenet_utils import preprocess_input
from keras.models import Model
from keras.models import model_from_json
import numpy as np
import pandas as pd


### Load model structure and weights ---------------------------------------

json_file = open('../RS/input/model/model.json', 'r')
loaded_model_json = json_file.read()
json_file.close()
model = model_from_json(loaded_model_json)
model.save_weights('../RS/input/model/weights.hdf5')

### extract features dropping last layers ----------------------------------

model_vgg16 = VGG16(weights='imagenet', include_top=False)

model_transfer = Model(inputs=model.input, 
                       outputs=model.get_layer('add_pool').output)


### Read salary_city_files --------------------------------------------------

income_city_files = pd.read_csv('../BA/input/model/income_city_file.txt')


### extract features averaging values for each city ------------------------

features_final = []
pos = 0

for city in income_city_files['city_code'].unique():
    features_temp = []
    pos += 1
    print(pos)
    df_filter = income_city_files[income_city_files['city_code']==city]
    for i in range(df_filter.shape[0]):
        img = image.load_img('../BA/' + df_filter.iloc[i, [2]][0])
        x = image.img_to_array(img)
        x = np.expand_dims(x, axis=0)
        x = preprocess_input(x)
        feat_image = model_vgg16.predict(x)
        feat_image_transfer = model_transfer.predict(feat_image)[0]
        features_temp.append(feat_image_transfer)
    city_feat = np.append(np.mean(features_temp, axis=0), df_filter.iloc[0, [0]])
    features_final.append(city_feat)


features_final = np.asarray(features_final)


### Write features file -----------------------------------------------------

np.savetxt('input/model/google_image_features_cnn_transfer.csv', features_final)

