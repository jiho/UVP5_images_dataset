import pandas as pd
import os

## UTIL
path_all_feather = os.path.expanduser("~/datasets/UVP5_images_dataset/all.feather")
path_dataset_with_annotators = os.path.expanduser("~/datasets/UVP5_images_dataset_with_annotators/projects")
csvPath_reclassify_taxa = os.path.expanduser("~/UVP5_images_dataset/data/export_contact_reclassify_taxa.csv")
csvPath_annotators_credits = os.path.expanduser("~/UVP5_images_dataset/data/export_contact_annotators_credits.csv")

## GET ALL DATA
# read all.feather
all=pd.read_feather(path_all_feather, columns=["projid", "taxon", "objid","annotator"])

# read projid.feathers
tmp_projid_feathers = []
for root, dirs, files in os.walk(path_dataset_with_annotators):
    for filename in files:
        # read and select needed col
        tmp_projid_feathers.append(pd.read_feather(path_dataset_with_annotators+'/'+filename, columns=['projid', 'userid', 'email', 'name', 'n']))
projid_feathers = pd.concat(tmp_projid_feathers)

## RECLASSIFY TAXA DF Owner_df & Taxa_df  (current annotators)
### DF TAXA : projid ; taxon_to_sort ; nb_obj ; bad_taxa_annotators

# select t0N and othertocheck classified taxons
df_taxon = all.loc[all['taxon'].str.startswith('t0') | all['taxon'].str.startswith("othertocheck")] 
# get the list of annotators that set bad taxa 
bad_taxa_annotators=df_taxon[["projid", "annotator", "objid"]].drop_duplicates()
# get the count of eatch unwanted taxon annotation for eatch annotator
bad_taxa_annotators = bad_taxa_annotators.groupby(["projid", "annotator"])["objid"].count().reset_index(name="nb_bad_obj_annoted")
# sort
bad_taxa_annotators = bad_taxa_annotators.sort_values(by=['projid', 'nb_bad_obj_annoted', 'annotator'], ascending=[True, False, True])
#get others needed col
df_taxon = df_taxon[["projid", "taxon", "objid"]].drop_duplicates()
# get the count of eatch unwanted taxon for eatch projects
df_taxon = df_taxon.groupby(["projid", "taxon"])["objid"].count().reset_index(name="nb_bad_obj")
# merge bad_taxa_annotators into df_taxon on projid
df_taxon = pd.merge(df_taxon, bad_taxa_annotators, on='projid', how="outer")
# sort
df_taxon = df_taxon.sort_values(by=['projid', 'taxon', "nb_bad_obj"], ascending=[True, True, False])

### DF OWNER : proj_id, proj_title, proj_owner_email
# select needed col
df_owner = pd.DataFrame({'projid': all["projid"].values})
# get uniques
df_owner = df_owner.drop_duplicates()
# sort
df_owner = df_owner.sort_values(by='projid')
# get "Data owner" of eatch projects from google sheet
projects_info = pd.read_csv("https://docs.google.com/spreadsheets/d/1CrR-5PdhQ09JSTU482HOkTjtCvXRmpngYDuYSwaGQAo/export?format=csv")
projects_info = projects_info.dropna(subset=['use'])[["projid", "title", "data_owner"]].astype({'projid':'int'})
df_owner["title"] = [projects_info[projects_info["projid"]==projid]["title"].values[0]for projid in df_owner["projid"]]
df_owner["url"] = ["https://ecotaxa.obs-vlfr.fr/prj/"+str(projid) for projid in df_owner["projid"]]
df_owner["data_owner"] = [projects_info[projects_info["projid"]==projid]["data_owner"].values[0]for projid in df_owner["projid"]]
# add information about the total number of **LIVING** objects  
df_tmp_ = all.loc[all['taxon'].str.startswith('t0') | all['taxon'].str.startswith("othertocheck")] 
df_owner["total_nb_objects"] = [len(all.loc[all['projid']==projid]["objid"].unique()) for projid in df_owner["projid"]]
df_non_living = all.loc[all['taxon'].str.startswith('artefact') | all['taxon'].str.startswith("detritus")]
df_owner["non_living_nb_objects"] = [len(df_non_living.loc[df_non_living["projid"]==projid]["objid"].unique())for projid in df_owner["projid"]]
df_owner["living_nb_objects"] = df_owner["total_nb_objects"] - df_owner["non_living_nb_objects"]
df_tmp = all.loc[all['taxon'].str.startswith('t0') | all['taxon'].str.startswith("othertocheck")] 
df_owner["to_reclassify_nb_objects"] = [len(df_tmp.loc[df_tmp["projid"]==projid]["objid"].unique())for projid in df_owner["projid"]]

### Merge df_owner, df_taxon to create RECLASSIFY TAXA DF
df_reclassify_taxa = pd.merge(df_owner, df_taxon, on='projid', how="outer")
# Save as csv
df_reclassify_taxa.to_csv(csvPath_reclassify_taxa, index = False, header=True)


## ANNOTATORS CREDITS DF owner & annotator (current + histo)
### DF ANNOTATORS : projid ; classif_user_id ; classif_name ; classif_email ; nb_classif (current + histo)

#### CURRENT
# get the list of annotators that set current taxa 
current_taxa_annotators=all[["projid", "annotator", "objid"]].drop_duplicates()
# get the count of current classified objects for eatch annotator
current_taxa_annotators = current_taxa_annotators.groupby(["projid", "annotator"])["objid"].count().reset_index(name="nb_obj_annoted")

#### HISTO
# get uniques
histo_taxa_annotators = projid_feathers.drop_duplicates()
# drop projects that are not in the final dataset
histo_taxa_annotators = histo_taxa_annotators[histo_taxa_annotators['projid'].isin(current_taxa_annotators["projid"].unique())]
# sort
histo_taxa_annotators = histo_taxa_annotators.sort_values(by=['projid', "n"], ascending=[True, False])
# Rename collums to match the desiered output
histo_taxa_annotators.rename(columns={'userid': 'annotator_histo_userid', 'email': 'annotator_histo_email', 'name': 'annotator_histo_name', 'n': 'annotator_histo_n'}, inplace=True)

#### MERGE AND FILTER
# for eatch project and annotators add current and histo count of annotations 
# on "annotator"(email) and "annotator_histo_email"(email
df_annotators_tmp = pd.merge(current_taxa_annotators, histo_taxa_annotators, left_on=['projid', 'annotator'], right_on=['projid', 'annotator_histo_email'], how="outer")
# fill missing nb with 0
df_annotators_tmp[['nb_obj_annoted','annotator_histo_n']] = df_annotators_tmp[['nb_obj_annoted','annotator_histo_n']].fillna(0)
# fill missing emails
df_annotators_tmp['annotator_histo_email'] = df_annotators_tmp['annotator_histo_email'].fillna(df_annotators_tmp['annotator_histo_n'])
# select only usfull col
df_annotators= df_annotators_tmp[["projid", "annotator_histo_email", "annotator_histo_name"]]
# sum the total of classif 
df_annotators["tot_annotations"] = df_annotators_tmp["annotator_histo_n"] + df_annotators_tmp["nb_obj_annoted"]
df_annotators["tot_annotations"] = df_annotators["tot_annotations"].astype(int)
# rename col
df_annotators.rename(columns={'annotator_histo_email': 'annotator_email', 'annotator_histo_name': 'annotator_name'}, inplace=True)
# filter n>1000
df_annotators = df_annotators[df_annotators.tot_annotations>=1000].drop_duplicates()


### Merge df_owner, df_annotators to create ANNOTATORS CREDITS DF 
df_annotators_credits = pd.merge(df_owner, df_annotators, on='projid', how="outer")
# sort
df_annotators_credits = df_annotators_credits.sort_values(by=['projid', 'tot_annotations'], ascending=[True, False])
# Save as csv
df_annotators_credits.to_csv(csvPath_annotators_credits, index = False, header=True)
