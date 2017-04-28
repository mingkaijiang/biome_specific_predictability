# Biome-specific climate predictability

This is the code base for the paper "Biome-specific climatic space defined by temperature and precipitation predictability". 


# Repository structure
The code structures are:
1. Folder "analyses" stores all the preliminary analysis output pdf files
2. Folder "data" stores all the input data and processed data
3. Folder "functions" stores all the source code to generate the analyses
4. Folder "Plots" stores all the manuscript figures
5. Folder "R" stores the setting scripts, i.e. variables and settings that should have a global attribute. 
6. Folder "Tables" stores all the manuscript tables
7. The script "Run_program.R" initiate the analysis

# Issues to be aware of
There are several issues you should be aware of before initiating the simulation:
1. The raw data should be downloaded first and put into the correct directory "data/raw_data". There is a function
to download the data but it just prints out where the data is and what name you need to download instead of downloading the data for you.
2. Some functions take some time to run (already commented with # of hours needed in the program). 
3. For the functions that are time-consuming to run, I performed data analysis using all climate data first, but later decided to only use part of the data (e.g. ~ 1000 data points per biome rather than > 10000 per biome) selected randomly as the results don't really differ that much. 
4. The entire repository includes all functions used in performing the analyses - many of the functions are no longer needed for the manuscript, but may be of interests to some other studies. 


# Author information
This repository is maintained by Mingkai Jiang (m.jiang@westernsydney.edu.au). 
