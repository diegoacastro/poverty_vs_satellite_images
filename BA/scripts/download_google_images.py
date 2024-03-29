# -*- coding: utf-8 -*-
"""
Created on Sat Jul 13 11:02:51 2019

@author: diego
"""

# libraries ------------------------------------------------------------------

import requests


# Define API parameters ------------------------------------------------------

base_url = "https://maps.googleapis.com/maps/api/staticmap?"
key = "INCLUDE_YOUR_API_KEY_HERE"


# Functions to download images -----------------------------------------------

def download_image(lat, lon, filename):
    param_url = "maptype=satellite&center="+ lat + "," + lon + "&zoom=16&size=400x400&style=feature:all|element:labels|visibility:off&format=png&key="
    final_url = base_url + param_url + key
    r = requests.get(final_url)
    if r.status_code == 200:
        with open(filename, 'wb') as img:
            img.write(r.content)

def get_classified_images():
    c = 0
    image_folder_url = 'input/google_images/'
    with open('input/model/download_coordinates.txt') as f:
        for line in f:
            c += 1
            if c > 1: # skip header of the file
                lst = line.split(',')
                intensity = float(lst[5])
                intensity_file = str(round(intensity * 100))
                if intensity <= 0.87: # upper limit for low radiance class
                    filename = image_folder_url + 'class1/' + str(lst[0]) + '_' + str(lst[6]) + '_' + str(lst[7]) + '_' + intensity_file + '.png'
                    download_image(lst[4], lst[3], filename)
                elif intensity <= 1037: # upper limit for medium radiance class
                    filename = image_folder_url + 'class2/' + str(lst[0]) + '_' + str(lst[6]) + '_' + str(lst[7]) + '_' + intensity_file + '.png'
                    download_image(lst[4], lst[3], filename)
                else:
                    filename = image_folder_url + 'class3/' + str(lst[0]) + '_' + str(lst[6]) + '_' + str(lst[7]) + '_' + intensity_file + '.png'
                    download_image(lst[4], lst[3], filename)


# Download images ------------------------------------------------------------

get_classified_images()