import pandas as pd
import os

### UTIL
filePath = os.path.expanduser("~/datasets/datasets/view_contact.txt")
csvPath = os.path.expanduser("~/datasets/export_contact.csv")

try:
    os.remove(filePath)
except:
    pass

df_final = pd.read_csv(csvPath)
grouped_df = df_final.groupby(by = ["data_owner"])

with open(filePath, "a") as f:
  for key, item in grouped_df:
      #print(grouped_df.get_group(key), "\n\n")
      print("DATA OWNER CONTACT : ", key, file=f)
      print("projects : ", grouped_df.get_group(key)["projid"].unique(), file=f)
      for projid in grouped_df.get_group(key)["projid"].unique() :
          tmp = grouped_df.get_group(key).loc[grouped_df.get_group(key)["projid"]==projid]
          print("------- ", projid, " : ", file=f)
          print("Taxon to reclassify : ", file=f)
          for taxon in tmp.taxon.unique() :
            try :
                print("   ",taxon, " : ", tmp.loc[tmp["taxon"]==taxon].nb_obj.unique()[0], " objects", file=f)
            except : 
                print("   ", "Nothing to reclassify", file=f)             
          print("Annotators history : ", file=f)
          for annotator in tmp.annotator_histo_userid.unique() :
            try :
                print("   ", tmp.loc[tmp["annotator_histo_userid"]==annotator].annotator_histo_name.unique()[0], " : ", tmp.loc[tmp["annotator_histo_userid"]==annotator].annotator_histo_n.unique()[0], " annotations", file=f)
            except : 
                print("   ", "No significant annotator", file=f)
          print("\n", file=f)
      
  f.close()   
