# -*- coding: utf-8 -*-
"""
Created on Fri Jul 19 23:59:29 2019

@author: diego
"""

### Libraries --------------------------------------------------------------

import plaidml.keras # Required if using AMD GPU
plaidml.keras.install_backend() # Required if using AMD GPU
import keras
from keras.applications.vgg16 import VGG16
from keras.preprocessing import image
from keras.models import Sequential, load_model
from keras.applications.imagenet_utils import preprocess_input
from keras.layers.convolutional import Conv2D, AveragePooling2D
from keras.callbacks import ModelCheckpoint, Callback, EarlyStopping
from keras.optimizers import SGD
from keras.layers.core import Activation
from keras.layers.core import Flatten
from keras.layers.core import Dense
from keras.layers import Dropout
from keras.models import Model
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd
import json as simplejson
import os
import time
import gc


### get image featuers -----------------------------------------------------

model_old = VGG16(weights='imagenet', include_top=False)

def get_input_feature(img_path):
    img = image.load_img(img_path)
    x = image.img_to_array(img)
    x = np.expand_dims(x, axis=0)
    x = preprocess_input(x)
    features = model_old.predict(x)
    return features[0]


### Append correct label to data -------------------------------------------
    
all_figures = []
trainLabels = []

path_1 = 'input/google_images/class1/'
class_1_files = os.listdir(path_1)
trainLabels += [[1, 0, 0]] * len(class_1_files)
all_figures += [path_1 + i for i in class_1_files]

path_2 = 'input/google_images/class2/'
class_2_files = os.listdir(path_2)
trainLabels += [[0, 1, 0]] * len(class_2_files)
all_figures += [path_2 + i for i in class_2_files]

path_3 = 'input/google_images/class3/'
class_3_files = os.listdir(path_3)
trainLabels += [[0, 0, 1]] * len(class_3_files)
all_figures += [path_3 + i for i in class_3_files]

trainData = []
t1 = time.time()
for idx, i in enumerate(all_figures):
    a = get_input_feature(i)
    trainData.append(a)
    if idx % 1000 == 0:
        t2 = time.time()
        print(idx)
        print(t2 - t1)
        t1 = time.time()


x_all = np.asarray(trainData)
y_all = np.asarray(trainLabels)


### Save data --------------------------------------------------------------

np.save('input/model/x_all.npy', x_all)
np.save('input/model/y_all.npy', y_all)

del(a, all_figures, class_1_files, class_2_files, class_3_files, i, idx, 
    path_1, path_2, path_3, t1, t2, trainData, trainLabels)
gc.collect()

#x_all = np.load('input/model/x_all.npy')
#y_all = np.load('input/model/y_all.npy')

### Split data into training and testing -----------------------------------

x_train, x_test, y_train, y_test = train_test_split(x_all, y_all,
                                                    stratify=y_all, 
                                                    test_size=0.2,
                                                    random_state=123) # seed

del(x_all, y_all)
gc.collect()


# =============================================================================
# indices_split = np.arange(len(x_all))
# np.random.seed(123)
# np.random.shuffle(indices_split)
# train_indices = indices_split[0: int((len(indices_split) * 0.8))]
# test_indices = indices_split[int((len(indices_split) * 0.8)):len(indices_split)]
# 
# x_train = x_all[train_indices]
# y_train = y_all[train_indices]
# x_test = x_all[test_indices]
# y_test = y_all[test_indices]
# 
# del(x_all, y_all, indices_split, test_indices, train_indices)
# =============================================================================


### Define CNN model configuration -----------------------------------------

model = Sequential()
model.add(Conv2D(4096, (6, 6), 
                 activation='relu', 
                 input_shape=(12, 12, 512), 
                 strides=(6, 6), 
                 name='input'))
model.add(Dropout(0.5))
model.add(Conv2D(4096, (1, 1), 
                 activation='relu', 
                 strides=(1, 1), 
                 name='conv_7'))
model.add(Dropout(0.5))
model.add(Conv2D(4096, (1, 1), 
                 strides=(1, 1), 
                 name='conv_8'))
model.add(AveragePooling2D((2, 2), 
                           strides=(1, 1), 
                           name='add_pool'))

model.add(Flatten(name='flatten'))
model.add(Dense(3))
model.add(Activation("softmax"))

opt = SGD(lr=1e-2)


### Compile model ----------------------------------------------------------

model.compile(loss="categorical_crossentropy", 
              optimizer=opt, 
              metrics=["accuracy"])

class AccuracyHistory(Callback):
    def on_train_begin(self, logs={}):
        self.acc = []

    def on_epoch_end(self, batch, logs={}):
        self.acc.append(logs.get('acc'))

history = AccuracyHistory()


### Fit model (storing  weights) -------------------------------------------

filepath="input/model/weights.hdf5"
checkpoint = ModelCheckpoint(filepath, 
                             monitor='val_acc', 
                             verbose=1, 
                             save_best_only=True, 
                             mode='max')

# Add early stopping
es = EarlyStopping(monitor='val_acc', verbose=1, patience=10)

callbacks_list = [checkpoint, history, es]

model.fit(x_train, y_train, 
          validation_data=(x_test, y_test),
          batch_size=32, 
          epochs=100, 
          verbose=1,
          callbacks=callbacks_list)

# =============================================================================
# model = load_model('input/model/weights.hdf5')
# 
# model.fit(x_train, y_train, 
#           validation_split=0.2,
#           batch_size=32, 
#           epochs=15, 
#           verbose=1,
#           callbacks=callbacks_list)
# =============================================================================


### storing Model in JSON --------------------------------------------------

model_json = model.to_json()

with open("input/model/model.json", "w") as json_file:
    json_file.write(simplejson.dumps(simplejson.loads(model_json), indent=4))


### evaluate model ---------------------------------------------------------

score_train = model.evaluate(x_train, y_train, verbose=1)
print('Test loss:', score_train[0])
print('Test accuracy:', score_train[1]) 

score = model.evaluate(x_test, y_test, verbose=1)
print('Test loss:', score[0])
print('Test accuracy:', score[1]) 


### clean variables --------------------------------------------------------

del(callbacks_list, filepath, model_json, x_test, x_train, y_test, y_train)
gc.collect()

### extract features dropping last layers ----------------------------------

model_vgg16 = VGG16(weights='imagenet', include_top=False)

model_transfer = Model(inputs=model.input, 
                       outputs=model.get_layer('add_pool').output)


### Read salary_city_files --------------------------------------------------

income_city_files = pd.read_csv('input/model/income_city_file.txt')


### extract features averaging values for each city ------------------------

features_final = []
pos = 0

for city in income_city_files['city_code'].unique():
    features_temp = []
    pos += 1
    print(pos)
    df_filter = income_city_files[income_city_files['city_code']==city]
    for i in range(df_filter.shape[0]):
        img = image.load_img(df_filter.iloc[i, [2]][0])
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
