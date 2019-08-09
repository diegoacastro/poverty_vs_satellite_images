# -*- coding: utf-8 -*-
"""
Created on Thu Aug  8 01:01:12 2019

@author: diego
"""

import os

path_base = 'input/google_images/class'

for i in ["1", "2", "3"]:
    path = path_base + i + "/"
    for f in os.listdir(path):
        file_path = path + f
        file = f[:-4].split("_")
        if int(file[3]) <= 87: # low radiance  
            new_file_path = path_base + "11/" + f 
            os.rename(file_path, new_file_path)
        elif int(file[3]) <= 1037: # medium radiance
            new_file_path = path_base + "22/" + f 
            os.rename(file_path, new_file_path)
        else: # high radiance
            new_file_path = path_base + "33/" + f 
            os.rename(file_path, new_file_path)

