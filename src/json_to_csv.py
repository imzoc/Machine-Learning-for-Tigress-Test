''' 
    Author: Zachary Keyes
    Purpose: Use ml4tt module to preprocess tigress_test data and put
    it into a pandas dataframe for easy machine learning use. 
'''


import ml4tt.ml4tt as ml4tt

in_file = "/Users/zachary/Documents/github\
/Machine-Learning-for-Tigress-Test/tigress_test\
/tigress-test-output/simple_c_tests\
/2023-01-13/raw_json\
/results_testcases_simple_c_1_20230119210217_20230119T210217-700.json"
preprocess_json = ml4tt.Preprocess_Json(snippet=False)
df = preprocess_json.data_to_df(in_file)

out_file = "/Users/zachary/Documents/github\
/Machine-Learning-for-Tigress-Test/tigress_test\
/tigress-test-output/simple_c_tests\
/2023-01-13/df_csv/\
simple_c_1_20230119.csv"
df.to_csv(out_file)