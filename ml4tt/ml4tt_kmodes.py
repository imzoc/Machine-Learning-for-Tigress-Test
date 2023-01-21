''' Time to do k-means!
'''

from kmodes.kmodes import KModes 
import ml4tt.ml4tt as ml4tt

file = "/Users/zachary/Documents/github\
/Machine-Learning-for-Tigress-Test/tigress_test\
/tigress-test-output/simple_c_tests\
/2023-01-13/raw_json\
/results_testcases_simple_c_1_20230119210217_20230119T210217-700.json"
preprocess_json = ml4tt.Preprocess_Json()
X = preprocess_json.data_to_df(file)

print(X.columns) # parameters so far

""" Then we do kmodes
kmodes = KModes(n_clusters=10, init='Huang', n_init=2, verbose=1)
clusters = kmodes.fit_predict(X)
print(kmodes.cluster_centroids_)
"""