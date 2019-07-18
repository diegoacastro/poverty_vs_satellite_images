# -*- coding: utf-8 -*-
"""
Created on Wed Jul 17 22:43:30 2019

@author: diego
"""

### Libraries ---------------------------------------------------------------

import plaidml.keras
plaidml.keras.install_backend()
from keras.applications.vgg16 import VGG16
import numpy as np
from keras.preprocessing import image
from keras.models import Sequential
from keras.applications.imagenet_utils import preprocess_input, decode_predictions
from keras.layers.convolutional import Convolution2D, AveragePooling2D
from keras.optimizers import SGD
from keras.layers.core import Activation
from keras.layers.core import Flatten
from keras.layers.core import Dense
from keras.layers import Dropout
from multiprocessing import Pool
import os
import time
import pandas as pd
import numpy as np
from keras.models import Model


### Read salary_city_files --------------------------------------------------

salary_city_files = pd.read_csv('input/model/salary_city_file.txt')
salary_city_files["feature"] = np.nan
salary_city_files["feature"] = salary_city_files["feature"].astype(object)


### Get CNN features --------------------------------------------------------

base_model = VGG16(weights='imagenet')
model = Model(inputs=base_model.input, outputs=base_model.get_layer('fc2').output)

# =============================================================================
# salary_city_files = pd.read_csv('input/model/google_image_features_cnn.csv',
#                                 sep=';')
# =============================================================================

for i in range(salary_city_files.shape[0]):
#for i in range(1):
    img = image.load_img(salary_city_files.iloc[i, [2]][0], target_size=(224, 224))
    x = image.img_to_array(img)
    x = np.expand_dims(x, axis=0)
    x = preprocess_input(x)
    salary_city_files.at[i, 'feature'] = model.predict(x)[0].tolist()
    if i % 100 == 0:
        print(i)


### Write features file -----------------------------------------------------
        
# =============================================================================
# salary_city_files.to_csv(path_or_buf='input/model/google_image_features_cnn.csv',
#                          index=False,
#                          sep=';')
# =============================================================================
