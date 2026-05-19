#If working in R studio, you need to have Python installed 
#and set up a virtual environment before running anything

#To do this, run these next few lines in the terminal :
#(change the path following "cd" to suit your needs)
pip install virtualenv
cd /home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/3_Ethnobotany/Scrape_PFAF/
virtualenv my_env
source my_env/bin/activate
which python
pip install numpy pandas matplotlib bs4 requests argparse
##################################


###### Set working directory #####
import os
os.chdir("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/3_Ethnobotany/Scrape_PFAF/") #specify path



###### Function and packages to source #####
exec(open("FUNCTION_scrape_PFAF.py").read()) #contains the function get_plant_info which is used to scrape PFAF




### Now run the scrape function and create a dataframe grouping all of the data :

###### Initialise the dataframe ######
COLUMNS = ["Genus", "Species", "Edible_parts", "Edible_uses", "Edible_description", "Medicinal_uses", "Medicinal_description", "Other_uses", "Other_uses_description", "Special_uses", "Special_uses_description"]
df = pd.DataFrame(columns = COLUMNS)



###### Import species list ######
#species list must be 1 column containing species names in the format "Genus species"
species_data = pd.read_csv('/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/WHP_correpondence_table_v17.csv')
species_list = species_data['PFAF_name']
#Remove NaN values and entries with "not found", and keep only unique values
species_list = species_list[species_list !=  'not_found']        # Remove entries with "not found"
species_list = species_list.dropna().unique()        # Drop NaN values
   
# Convert back to a DataFrame
species_list = pd.DataFrame(species_list, columns = ['PFAF_name'])

###### Create a loop, and run the scrape function for each species in the list. ###### 
#note : this takes around 20 minutes for ~700 species

for i in range(0,len(species_list),1) :
  
  #get species name
  sp = species_list.iloc[i, 0].split()
  genus = sp[0]
  species = sp[1]
  
  #call scraping function
  df = pd.concat([df, pd.Series(get_plant_info(genus, species), index = COLUMNS)], axis = 1)
  
  print (sp, [str(i+1) + "/" + str(len(species_list))]) #to know where we're at

df = df.T[11:] #transpose
    #11 is the number of columns



###### Clean the dataframe ######
  # Function to replace bracket patterns within each cell of the DataFrame
def remove_bracket(text):
  return text.replace("[\'", "").replace("']", "").replace('[\"', "").replace('"]', "")

  # Function to remove all of the "None Known" occurences
def remove_non_known(text):
  return text.replace("None known", "")

  # Apply the functions to the entire DataFrame using map
df = df.map(remove_bracket)
df = df.map(remove_non_known)

  # Add a column with the species names and CD_REF
df["PFAF_name"] = df["Genus"] + " " + df["Species"]
df_final = pd.merge(df, species_data[["CD_REF", "NOM_VALIDE", "PFAF_name"]], on = 'PFAF_name', how = 'left')  




###### EXPORT ######
# Step 1: Remove the first two columns
df_reduced = df_final.iloc[:, 2:]  # Keeping columns from index 2 onwards

# Step 2: Select the last two columns
last_two_columns = df_final.iloc[:, -2:]  # Getting the last two columns

# Step 3: Combine the last two columns with the remaining DataFrame, and remove last 2 columns
new_df = pd.concat([last_two_columns, df_reduced.iloc[:, :-3]], axis = 1)

new_df.to_csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/3_Ethnobotany/raw_data/PFAF_database.csv", index = False)
