import glob
from skimage import io
import os
import numpy as np
import shutil
from PIL import Image
import csv

src_dir = "C:\\Users\\aurel\\Documents\\Apples\\dev\\dev2022\\data\\original\\"

#DONE
#Add day info to file name
for tif in glob.glob(src_dir + "**/*.tif"):
    newname = tif.replace(".tif", "_" + tif.split(os.sep)[-2] + ".tif")
    os.rename(tif, newname)

#DONE
#Change to PNG (keeps .tifs)
for tif in glob.glob(src_dir + "**/*.tif"):
    with Image.open(tif) as img:
        img.save(tif.replace(".tif", ".png"))

#DONE
#Copy all to one folder, separating a/b images and eggs
dst_dir = "C:\\Users\\aurel\\Documents\\Apples\\dev\\dev2022\\data\\foranalysis\\"
for tif in glob.glob(src_dir + "**/*.png"):
    tif_info = tif.split(os.sep)[-1].split("_")

    if "eggs" in tif_info[-1]:
        shutil.copy(tif, dst_dir + "egg\\")
    elif "b" in tif_info[-2]:
        shutil.copy(tif, dst_dir + "gonad\\")
    else:
        shutil.copy(tif, dst_dir + "body\\")

#DONE
#Copy random sample (20% of files) to new folder for annotation
for (root, dirs, files) in os.walk(dst_dir):
    for folder in dirs:
        folder_path = dst_dir + folder + os.sep
        nb_tosample = round(len(glob.glob(folder_path + "*.png"))/5)
        rdm_sample = np.random.choice(os.listdir(folder_path), nb_tosample, replace=False)
        sample_folder = folder_path +  folder + "_forannotation" + os.sep
        os.makedirs(sample_folder)
        for f in rdm_sample:
            shutil.copy(folder_path + f, sample_folder)
            #File not found error, but worked.

#DONE
#Blind rename of files to annotate
original_to_anonymized = []
counter = 1

for (root, dirs, files) in os.walk(dst_dir):
    for folder in dirs:
        if "forannotation" in folder:
            path_tofiles = dst_dir + folder.split("_")[0] + os.sep + folder + os.sep

            png_files = [f for f in os.listdir(path_tofiles) if f.endswith('.png')]
            
            for file in png_files:
                new_name = f"{folder.split('_')[0]}_{counter}.png"
                original_to_anonymized.append((file, new_name))
                os.rename(os.path.join(path_tofiles, file), os.path.join(path_tofiles, new_name))
                counter += 1

with open(os.path.join(dst_dir, 'anonymizedimg.csv'), 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(['Original Name', 'Anonymized Name'])
    writer.writerows(original_to_anonymized)