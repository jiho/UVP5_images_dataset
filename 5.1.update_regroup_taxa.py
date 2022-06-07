#imports
import pandas as pd

# paths
old_path = "https://docs.google.com/spreadsheets/d/1NFgpzkFXVBEuobaggmApIYgQ2HE837zwsStu7WTU5hQ/export?format=csv"
new_path = "~/datasets/UVP5_images_dataset/all.feather"

# read data
old = pd.read_csv(old_path)
new = pd.read_feather(new_path, columns=["objid","taxon", "lineage"])

# summurise taxa information for new dataset
new_summary = new.groupby(["taxon", "lineage"])["objid"].count().reset_index(name="nb_objects")

# complete taxa information from new dataset with filled group and comment from old taxa information dataset
new_completed_with_old_annotation = pd.merge(new_summary, old, how='left', on="taxon")

# arrange cols and sort by lineage
new_completed_with_old_annotation = new_completed_with_old_annotation[["lineage", 
                                                                       "taxon", 
                                                                       "nb_objects",
                                                                       "group_thelma",
                                                                       "comment_thelma",
                                                                       "group_laeti",
                                                                       "comment_laeti",
                                                                       "group_tristan",
                                                                       "comment_tristan"]].sort_values(by=['lineage'])
# save as tsv and import it manualy in google drive : https://docs.google.com/spreadsheets/d/1a-W6dSxm9q-2-ANRsfTu6yXK3BeKDwO16kaw0NMSc9k/edit?usp=sharing
new_completed_with_old_annotation.to_csv("data/new_taxa_completed_with_old_annotation.tsv",index=False, sep="\t")
