import pandas as pd
import os
from IPython.core.display import display,HTML
path_all_feather = os.path.expanduser("~/datasets/UVP5_images_dataset/backup_all.feather")
export_contact_reclassify_taxa = os.path.expanduser("~/datasets/export_contact_reclassify_taxa.csv")

# read data
all=pd.read_feather(path_all_feather, columns=["projid", "taxon", "objid", "file_name"]).drop_duplicates().sort_values(by="projid")

df_export_contact = pd.read_csv(export_contact_reclassify_taxa, usecols= ["projid","title","url", "taxon"]).drop_duplicates().sort_values(by="title")

df_export_contact["img1"]=""
df_export_contact["img2"]=""
df_export_contact["img3"]=""
df_export_contact["img4"]=""
df_export_contact["img5"]=""

# merge data to obtain complete info
merged_df = pd.merge(df_export_contact, all, how="left", on=["projid","taxon"])
# get only five first row of eatch group ("projid","taxon")
grouped_df = merged_df.groupby(["projid","taxon"]).head(5).reset_index(drop=True)

# iter over eatch group and set the url path 
tmp = grouped_df.groupby(["projid","taxon"])
for key, item in tmp:
    path_img = merged_df.loc[((merged_df["projid"]==key[0]) & (merged_df["taxon"]==key[1])),["file_name"]].values
    for i in range(0,5) :
        try :
            df_export_contact.loc[((df_export_contact["projid"]==key[0]) & (df_export_contact["taxon"]==key[1])), ["img"+str(i+1)]] = "https://ecotaxa.obs-vlfr.fr/vault/" + path_img[i]
        except :
            print("no more object for : ", key)
            
            
# convert links to html tags 
def path_to_image_html(path):
    return '<img src="'+ path + '" width="160" >' if path else path

pd.set_option('display.max_colwidth', None)

# define which columns will be used to convert to html
image_cols = ['img1', 'img2', 'img3', 'img4', 'img5']  

# Create the dictionariy to be passed as formatters
format_dict = {}
for image_col in image_cols:
    format_dict[image_col] = path_to_image_html

#export to html
df_export_contact.to_html('data/reclassify_table_summary.html', escape=False, formatters=format_dict)

