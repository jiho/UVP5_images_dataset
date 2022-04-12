import pandas as pd
import os
#google sheet api
from auth import spreadsheet_service
from auth import drive_service

### UTIL

filePath = os.path.expanduser("~/datasets/view_contact.txt")
csvPath_reclassify_taxa = os.path.expanduser("~/datasets/export_contact_reclassify_taxa.csv")
csvPath_annotators_credits = os.path.expanduser("~/datasets/export_contact_annotators_credits.csv")

def new_google_sheet(data):
    # Create sheet
    spreadsheet_details = {
    'properties': {
        'title': 'Dataset UVP5 credits for ' + data['data_owner'].unique()[0]
        }
    }
    sheet = spreadsheet_service.spreadsheets().create(body=spreadsheet_details).execute()
    sheetId = sheet.get('spreadsheetId')
    sheetURL = sheet.get('spreadsheetUrl')
    
    # Grant permissions    
    permission = {
        'type': 'anyone',
        'role': 'writer'
    }
    sheets_file = drive_service.permissions().create(fileId=sheetId, body=permission).execute()
    
    return sheetURL, sheetId
  
def fill_google_sheet(projects_data, sheetId):
    
    requests=[]
    # Set col names
    values = [["Project name", "Project URL", "Data owner contact", "Annotators", "Additional authors"]]
    body = {'values': values}
    
    for i in range(0, len(projects_data)) :
        # Add projects_data to sheetId at line i
        project_data=projects_data[i].astype({"tot_annotations": str})
        values.append([
            project_data["title"].unique()[0], 
            project_data["url"].unique()[0], 
            project_data["data_owner"].unique()[0], 
            ', \n'.join(project_data["annotator_name"].unique()+", "+project_data["annotator_email"].unique()+" (n = " +project_data["tot_annotations"].unique()+")"), ""
        ])

    body = {'values': values}
    result = spreadsheet_service.spreadsheets().values().append(
        spreadsheetId=sheetId, range="Sheet1!A1:E"+str(len(projects_data)+2),
        valueInputOption='USER_ENTERED', body=body).execute()

# reset file if exist
try:
    os.remove(filePath)
except:
    pass

# get stored data
df_reclassify_taxa = pd.read_csv(csvPath_reclassify_taxa)
df_annotators_credits = pd.read_csv(csvPath_annotators_credits)

# group by data owner
grouped_df = df_reclassify_taxa.groupby(by = ["data_owner"])

# open the file and starts writing in it trought prints
with open(filePath, "a") as f:
    for key, item in grouped_df:
      
      print("Destinataire : ", key, file=f)
      print("""Sujet : "Effort to homogenise and publish UVP5 images datasets"

Dear Colleague,
  
You are identified as the data owner of EcoPart projects linked to the following EcoTaxa projects : """, file=f)
      
      sheetUrl, sheetId = new_google_sheet(df_annotators_credits.loc[df_annotators_credits["data_owner"]==key])    
  
      for projid in grouped_df.get_group(key)["projid"].unique() :
          tmp = grouped_df.get_group(key).loc[grouped_df.get_group(key)["projid"]==projid]
          print(tmp.url.unique()[0], " : ", tmp.title.unique()[0], file=f)
      
      x_tmp=item[["to_reclassify_nb_objects","living_nb_objects", "projid"]].drop_duplicates()
      x="{:.2f}".format((100*x_tmp["to_reclassify_nb_objects"].sum())/x_tmp["living_nb_objects"].sum())
      print("""
We are contacting you with respect to the publication of a global UVP5 image dataset and an accompanying data paper in EESD. 
We would like to invite you to co-author the dataset and data paper, as you were part of this endeavor (maybe without knowing it explicitly until now  and pushed the consistent use of the UVP5 during several cruises, contributed data or contributed to the data aggregation and quality control work needed to get the dataset together. 
  
You might have gotten notice of this action already and probably was involved in the UVP ‘particle’ data paper by Rainer Kiko. We are doing a similar effort but for the images stored and validated on ECOTAXA. We are now contacting data owners that have validated their images. 
This dataset has to be seen as the current state of validation (~2800 profiles) that is frozen for the purpose of this paper and in the future, better sorting and more profiles will allow the building up of the database (currently up to 9000 profiles) that could be saved as a second version.
  
The goal is to make public all the ecotaxa data (images, metadata, morphological measures, identification) of the UVP5sd and UVP5hd for other users. The data is including all categories for which validation of plankton has been made at 99%. Plankton must be sorted in homogeneous categories (which is the case thanks to Ecotaxa) and the metadata must be  corrected (which is the case thanks to the effort by Rainer in the previous paper).
The data product will contain the details of the taxonomic level currently in ECOTAXA but the paper will describe aggregated groups (for example at copepod level even if detailed recognition exist). The data product will also contains all the vignettes of the detritus but as only one category because the sorting is not homogeneous among us.
  
We noticed that some images are still in undefined or temporary categories. Those represent""", str(x),"""% of the living objects in your projects. It would be really great if you could create new categories with descriptive names (e.g. 'long with dark center', 'round with fuzzy edge') branched within the taxonomy of life (or check if such names already exist) and move the images there. It would allow other UVP users to follow this nomenclature and would avoid loosing the data when we aggregate it (because your t001 is not the same as that of others). We can assist you with this process.
For the categories that still need sorting (othertocheck, some temporary ones), it would be excellent if you could make a last bit of effort to sort what can be sorted. Again, we may be able to assist you if need be.
Here is the list of the undefined or temporary ones  :""", file=f)
      
      # initialize / empty projects_data
      projects_data=[]
      for projid in grouped_df.get_group(key)["projid"].unique() :
          tmp = grouped_df.get_group(key).loc[grouped_df.get_group(key)["projid"]==projid]
          #fill project data
          projects_data.append(df_annotators_credits.loc[df_annotators_credits["projid"]==projid])
          print("\n",tmp.url.unique()[0], " : ", file=f)
          for taxon in tmp.taxon.unique() :
              try :
                  print("   ",taxon, " : ", tmp.loc[tmp["taxon"]==taxon].nb_bad_obj.unique()[0], " objects", file=f)
              except : 
                  print("   ", "Nothing to reclassify", file=f)
          
      #fill the  google sheet with project data
      fill_google_sheet(projects_data, sheetId)
      print("""
Finally, to compile a fair list of authors, we collected the names of all people who provided more that 1000 annotations in this Google Sheet : """, sheetUrl, """.\nCould you please edit it to:
  - remove names of people you feel should not be authors (e.g. employees/students who have since left the field and have contributed little)
  - add the names and affiliations of people you would like to see added (e.g. participants to the cruises during which the data was collected, people having sorted the images prior to their import on EcoTaxa)
  
Can you inform us if you are willing to participate to this effort.
The dataset preparation is already in a good state (waiting for your changes) and I will soon share a draft with the current version.
The goal is to have a draft submitted for the end of june.
  
  
Thank you in advance!
  
  
Lars Stemmann""", file=f)
      print("\n\n------------------------------------------------------------- \n\n", file=f)
    f.close()   
