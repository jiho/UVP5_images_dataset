import pandas as pd
import os

from IPython.core.display import display,HTML
path_all_feather = os.path.expanduser("~/datasets/UVP5_images_dataset/all.feather")
export_contact_reclassify_taxa = os.path.expanduser("~/UVP5_images_dataset/data/export_contact_reclassify_taxa.csv")
# read data
all=pd.read_feather(path_all_feather, columns=["projid", "taxon", "objid", "file_name"]).drop_duplicates().sort_values(by="projid")

df_export_contact = pd.read_csv(export_contact_reclassify_taxa, usecols= ["projid","title","url", "taxon"]).drop_duplicates().sort_values(by="title")

# select t0N and othertocheck classified taxons
df_taxon = all.loc[all['taxon'].str.startswith('t0') | all['taxon'].str.startswith("othertocheck")] 

# merge data to obtain complete info
merged_df = pd.merge(df_taxon, df_export_contact, how="left", on=["projid","taxon"])

# get only five first row of eatch group ("projid","taxon")
grouped_df = merged_df.groupby(["projid","taxon"]).head(5).reset_index(drop=True)

# get number of objects to resort by project and category
count_df = merged_df.groupby(["projid","taxon"]).size().reset_index(name='counts')
# iter over eatch group and set the url path 
tmp = grouped_df.groupby(["projid","taxon"])
final_df = grouped_df[["projid","title", "url", "taxon"]].drop_duplicates()
final_df["count"]=0
final_df["img1"]=""
final_df["img2"]=""
final_df["img3"]=""
final_df["img4"]=""
final_df["img5"]=""

for key, item in tmp:
    path_img = merged_df.loc[((merged_df["projid"]==key[0]) & (merged_df["taxon"]==key[1])),["file_name"]].values
    count = count_df.loc[((count_df["projid"]==key[0]) & (count_df["taxon"]==key[1])),["counts"]].values
    # add this count to the main df
    final_df.loc[((final_df["projid"]==key[0]) & (final_df["taxon"]==key[1])), ["count"]] = count
    for i in range(0,5) :
        try :
            final_df.loc[((final_df["projid"]==key[0]) & (final_df["taxon"]==key[1])), ["img"+str(i+1)]] = "https://ecotaxa.obs-vlfr.fr/vault/" + path_img[i]
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

final_df=final_df.sort_values(by=['projid',"taxon","count"])

#export to html
final_df.to_html('data/reclassify_table_summary.html', escape=False, formatters=format_dict)

