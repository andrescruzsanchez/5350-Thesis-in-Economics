# Thesis in Economics

This repository contains the work for our thesis in Economics, conducted in collaboration with Klara Holmer and Andres Cruz (me). In this project, we import, modify, and analyze publicly available data from Skolverket using Python and R. The project covers:

	•	Data Importing and Cleaning: Retrieving and preparing data for analysis.
	•	Data Transformation: Modifying and structuring data.
	•	Data Analysis: Employing analytical techniques with R to derive meaningful insights.

Note: The code must be executed in the following order:

0. 	File paths in the current version of the code are hardcoded and may not run correctly on systems with different directory structures. If you intend to run the scripts, 	please update the file paths manually (in all scripts) to match your local setup. This limitation will be addressed in future projects by using dynamically constructed, 	relative paths to ensure full cross-platform compatibility.
   
1.	Run grundskola_data_preparation.ipynb and gymnasieskola_data_preparation.ipynb.
	•	If you wish to select a different program in gymnasieskola_data_preparation.ipynb, please open the notebook and manually input the program of your choice.

2.	Then run school_data_preparation.ipynb.
   
3.	Finally, run Main_Analysis.R, which calls the following scripts internally: Themes.R, Propensity_Score_Matching.R, and Analysis_Pre_Matching.R.
   	•	You need to manually change the subject to English, Swedish, or Mathematics in Main_Analysis.R.

