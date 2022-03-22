import pandas as pd

df_final = pd.read_csv("~/datasets/export_contact.csv")

grouped_df = df_final.groupby(by = ["data_owner"])

for key, item in grouped_df:
    #print(grouped_df.get_group(key), "\n\n")
    print("DATA OWNER CONTACT : ",key)
    print("projects : ", grouped_df.get_group(key)["projid"].unique())
    for projid in grouped_df.get_group(key)["projid"].unique() :
        tmp = grouped_df.get_group(key).loc[grouped_df.get_group(key)["projid"]==projid]
        print("------- ", projid, " : ")
        print("Taxon to reclassify : ")
        for taxon in tmp.taxon.unique() :
          print("   ",taxon, " : ", tmp.loc[tmp["taxon"]==taxon].nb_obj.unique()[0], " objects")
            
        print("Annotators history : ")
        for annotator in tmp.annotator_histo_userid.unique() :
          print("   ", tmp.loc[tmp["annotator_histo_userid"]==annotator].annotator_histo_name.unique()[0], " : ", tmp.loc[tmp["annotator_histo_userid"]==annotator].annotator_histo_n.unique()[0], " annotations")
    print("\n")
