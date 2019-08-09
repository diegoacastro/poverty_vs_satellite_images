# -*- coding: utf-8 -*-
"""
Created on Wed Jul 24 11:02:33 2019

@author: diego
"""

### Libraries --------------------------------------------------------------

import plaidml.keras
plaidml.keras.install_backend()
import time
import os
import os.path
import matplotlib.pyplot as plt
import numpy as np

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
np.savetxt('input/model/google_image_features_basic.csv', feature_all)