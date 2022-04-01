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
    return "#sheetURL", "#sheetId"
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
    users = ['julie.coustenoble@imev-mer.fr']#, 'irisson@normalesup.org', 'lars.stemmann@imev-mer.fr', data['data_owner'].unique()[0]]
    for user_email in users :
        permission1 = {
            'type': 'user',
            'role': 'writer',
            'emailAddress': user_email
        }
        sheets_file = drive_service.permissions().create(fileId=sheetId, body=permission1).execute()
    
    # Set col names
    values = [["Project name", "Project URL", "Data owner contact", "Annotators", "Additional authors"]]
    body = {'values': values}
    result = spreadsheet_service.spreadsheets().values().append(
        spreadsheetId=sheetId, range="Sheet1!A1:E1",
        valueInputOption='USER_ENTERED', body=body).execute()
    
    return sheetURL, sheetId
  
def fill_google_sheet(project_data, sheetId, i):
    return #TODO remove 
    # Add project_data to sheetId at line i
    project_data=project_data.astype({"tot_annotations": str})
    values = [[
        project_data["title"].unique()[0], 
        project_data["url"].unique()[0], 
        project_data["data_owner"].unique()[0], 
        ', '.join(project_data["annotator_name"].unique()+" (n = " +project_data["tot_annotations"].unique()+")"), ""
    ]]
    body = {'values': values}
    result = spreadsheet_service.spreadsheets().values().append(
        spreadsheetId=sheetId, range="Sheet1!A"+str(i)+":E"+str(i),
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
        # create the google sheet permissions and headers
        sheetUrl, sheetId = new_google_sheet(df_annotators_credits.loc[df_annotators_credits["data_owner"]==key])    
        # cpt for row in exported google sheetstart at 2
        i = 2

        # general intro
        print("Destinataire : ", key, file=f)
        print('Sujet : "Effort to homogenise and publish UVP5 images datasets" \n', file=f)
        print("#TODO corps du message, le contexte du projet etc.  \n", file=f)
        
        # list projects for this owner
        print("You are identified as the data owner of EcoPart projects linked to the following EcoTaxa projects : ", file=f)
        for projid in grouped_df.get_group(key)["projid"].unique() :
            tmp = grouped_df.get_group(key).loc[grouped_df.get_group(key)["projid"]==projid]
            x="#TODO"
            print(tmp.url.unique()[0], " : ", tmp.title.unique()[0], file=f)
          
        # list undefined or temporary categories
        print("\nWe noticed that some images are still in undefined or temporary categories. Those represent", x," of the living objects in your projects. It would be really great if you could create new categories with descriptive names (e.g. 'long with dark center', 'round with fuzzy edge') branched within the taxonomy of life (or check if such names already exist) and move the images there. It would allow other UVP users to follow this nomenclature and would avoid loosing the data when we aggregate it (because your t001 is not the same as that of others). We can assist you with this process.", file=f)
        print("For the categories that still need sorting (othertocheck, some temporary ones), it would be excellent if you could make a last bit of effort to sort what can be sorted. Again, we may be able to assist you if need be.", file=f)
        print("Here is the list of the undefined or temporary ones :", file=f)
        for projid in grouped_df.get_group(key)["projid"].unique() :
            tmp = grouped_df.get_group(key).loc[grouped_df.get_group(key)["projid"]==projid]
            #fill the  google sheet with project data
            fill_google_sheet(df_annotators_credits.loc[df_annotators_credits["projid"]==projid], sheetId, i)
            i= i+1
            print("\n",tmp.url.unique()[0], " : ", file=f)
            for taxon in tmp.taxon.unique() :
                try :
                    print("   ",taxon, " : ", tmp.loc[tmp["taxon"]==taxon].nb_bad_obj.unique()[0], " objects", file=f)
                except : 
                    print("   ", "Nothing to reclassify", file=f)
            #print("   ","Latest annotators in these taxa : ", file=f)
            #for annotator in tmp.annotator.unique() :
            #    try :
            #        print("      ", annotator, " : ", tmp.loc[tmp["annotator"]==annotator].nb_bad_obj_annoted.unique()[0], "objects", file=f)
            #    except : 
            #        print("   ", "ERROR", file=f)

        print("""
    Finally, to compile a fair list of authors, we collected the names of all people who provided more that 1000 annotations in this Google Sheet : """, sheetUrl, """.\nCould you please edit it to:
    - remove names of people you feel should not be authors (e.g. employees/students who have since left the field and have contributed little)
    - add the names and affiliations of people you would like to see added (e.g. participants to the cruises during which the data was collected, people having sorted the images prior to their import on EcoTaxa)

    Thank you in advance!""", file=f)
        print("\n\n------------------------------------------------------------- \n\n", file=f)
    f.close()   
