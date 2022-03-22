import pandas as pd
import os

### GET ALL DATA

path_backup_all = "/home/jcoustenoble/datasets/UVP5_images_dataset/all.feather"
all=pd.read_feather(path_backup_all, columns=["projid", "taxon", "objid"])
#print(all.columns)

### DF TAXON : projid ; taxon_to_sort ; nb_obj
#select t0N and othertocheck classified taxons
df_taxon = all.loc[all['taxon'].str.startswith('t0') | all['taxon'].str.startswith("othertocheck")] 
#select needed col
df_taxon = df_taxon[["projid", "taxon", "objid"]]
#get uniques
df_taxon = df_taxon.drop_duplicates()
#get the count of eatch unwanted taxon for eatch projects
df_taxon = df_taxon.groupby(["projid", "taxon"])["objid"].count().reset_index(name="nb_obj")
# sort
df_taxon = df_taxon.sort_values(by=['projid', 'taxon', "nb_obj"], ascending=[True, True, False])
#print(df_taxon)
#print(len(df_taxon.projid.unique()))


### DF OWNER : proj_owner_email
#select needed col
df_owner = pd.DataFrame({'projid': all["projid"].values})
#get uniques
df_owner = df_owner.drop_duplicates()
# sort
df_owner = df_owner.sort_values(by='projid')
#get "Data owner" of eatch projects from google sheet
projects_info = pd.read_csv("https://docs.google.com/spreadsheets/d/1CrR-5PdhQ09JSTU482HOkTjtCvXRmpngYDuYSwaGQAo/export?format=csv")
projects_info = projects_info[projects_info["use"]=="x"][["projid","data_owner"]].astype({'projid':'int'})
df_owner["data_owner"] = [projects_info[projects_info["projid"]==projid]["data_owner"].values[0]for projid in df_owner["projid"]]
#print(df_owner)
#print(len(df_owner.projid.unique()))


### DF ANNOTATORS : projid ; classif_histo_user_id ; classif_histo_name ; classif_histo_email ; nb_classif
#select needed col 
df_projects_ids = pd.DataFrame({'projid': all["projid"].values})
#get uniques
df_projects_ids = df_projects_ids.drop_duplicates()
# sort
df_projects_ids = df_projects_ids.sort_values(by='projid')
#print(df_projects_ids)

path = "/home/jcoustenoble/datasets/UVP5_images_dataset_with_annotators/projects"
#read feathers
tmp_feathers = []
for root, dirs, files in os.walk(path):
    for filename in files:
        #read and select needed col
        tmp_feathers.append(pd.read_feather(path+'/'+filename, columns=['projid', 'userid', 'email', 'name', 'n']))
df_tmp_feathers = pd.concat(tmp_feathers)
#print(df_tmp_feathers)

#get anotators with more than 1000 contributions on the project
df_annotators = df_tmp_feathers[df_tmp_feathers.n>=1000]
#get uniques
df_annotators = df_annotators.drop_duplicates()
# drop projects that are not in the final dataset
df_annotators = df_annotators[df_annotators['projid'].isin(df_projects_ids["projid"].values)]
# sort
df_annotators = df_annotators.sort_values(by=['projid', "n"], ascending=[True, False])
#Rename collums to match the desiered output
df_annotators.rename(columns={'userid': 'annotator_histo_userid', 'email': 'annotator_histo_email', 'name': 'annotator_histo_name', 'n': 'annotator_histo_n'}, inplace=True)
#print(df_annotators)
#print(len(df_annotators["projid"]))


### Concat all df
df_final = pd.merge(df_owner, df_taxon, on='projid', how="outer")
df_final = pd.merge(df_final, df_annotators, on='projid', how="outer")

### Save df
df_final.to_csv("~/datasets/export_contact.csv", index = False, header=True)
