''' 
    Author: Zachary Keyes
    Purpose: Use ml4tt module to preprocess tigress_test data and put
    it into a pandas dataframe for easy machine learning use. 
'''


from module.ml4tt import Preprocess_Json
preprocess_json = Preprocess_Json(snippet=False)

path = "/Users/zachary/Documents/github/Machine-Learning-for-Tigress-Test/\
tigress_test/tigress-test-output/"
name = "2023-01-13/raw_json/results_testcases_simple_c_1_20230119210217_20230119\
T210217-700.json"
filename = path + name
file_names_list = [filename]
df = preprocess_json.data_to_df(file_names_list)
# file names should always be in a list, even if it's only 1 long!

name = "2023-01-13/df_csv/simple_c_1_20230119.csv"
df.to_csv(path+name)