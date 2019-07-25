# -*- coding: utf-8 -*-
"""
Created on Wed Jul 24 18:10:22 2019

@author: diego
"""

# -*- coding: utf-8 -*-
"""
Created on Fri Jul 19 23:59:29 2019

@author: diego
"""

### Libraries --------------------------------------------------------------

import plaidml.keras
plaidml.keras.install_backend()
from keras.optimizers import SGD
from keras.models import model_from_json
import numpy as np
import gc


### Load data --------------------------------------------------------------

x_all = np.load('input/model/x_all.npy')
y_all = np.load('input/model/y_all.npy')


### Split data into training and testing -----------------------------------

indices_split = np.arange(len(x_all))
np.random.seed(123)
np.random.shuffle(indices_split)
train_indices = indices_split[0: int((len(indices_split) * 0.8))]
test_indices = indices_split[int((len(indices_split) * 0.8)):len(indices_split)]

x_test = x_all[test_indices]
y_test = y_all[test_indices]

del(x_all, y_all, indices_split, test_indices, train_indices)
gc.collect()


### Rebuild Model ----------------------------------------------------------

# Load model
json_file = open("input/model/model.json", 'r')
loaded_model_json = json_file.read()
json_file.close()
model = model_from_json(loaded_model_json)

# Load weights into model
model.load_weights('input/model/weights.hdf5')


### evaluate model test set ------------------------------------------------

opt = SGD(lr=1e-2)
model.compile(loss='categorical_crossentropy', optimizer=opt, metrics=['accuracy'])
score = model.evaluate(x_test, y_test, verbose=1)
print('Test loss:', score[0])
print('Test accuracy:', score[1]) 

