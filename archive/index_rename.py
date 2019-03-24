# rename "00Tree.html" index html files

import os
import sys

print('Renaming directories..')

count = 0

directory = os.path.dirname(os.path.realpath(sys.argv[0])) #get the directory of script
for subdir, dirs, files in os.walk(directory):
 for filename in files:
   subdirectoryPath = os.path.relpath(subdir, directory) # get the path to subdirectory
   filePath = os.path.join(subdirectoryPath, filename) # get the path to file
   newFilePath = filePath.replace("00Tree","index") # create the new name
   if not filePath[0] == '.' and "00Tree" in filePath:
	   os.rename(filePath, newFilePath) # rename file
	   count += 1
	   print(filePath+' Renamed ('+str(count)+')')

print('Complete, '+str(count)+' file(s) renamed.')