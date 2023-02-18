''' 
    Author: 

    Purpose:
'''

from kmodes.kmodes import KModes 
import ml4tt

def main():
    in_file = "PATHNAME"
    preprocess_json = ml4tt.Preprocess_Json()
    X = preprocess_json.data_to_df(in_file)

    # if taking data from a .csv file generated previously, simply use
    # pandas's csv support.

    # think about what parameters we want to use.

    kmodes = KModes(n_clusters=10, init='Huang', n_init=2, verbose=1)
    clusters = kmodes.fit_predict(X)
    print(kmodes.cluster_centroids_)

main()