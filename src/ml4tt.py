''' 
    Author: Zachary Keyes
    Context: For the Tigress_test program for the Tigress project
    (https://tigress.wtf)
    Purpose: This ml4tt module preprocesses tigress_test data and puts
    it into a pandas dataframe in preparation for machine learning.
'''

import json
import pandas as pd

def main():
    preprocess_json = Preprocess_Json()
    in_file = "PATHNAME_IN"
    df = preprocess_json.data_to_df(in_file)
    out_file = "PATHNAME_OUT"
    df.to_csv(out_file)

class Preprocess_Json:
    def __init__(self, snippet_mode=False):
        self.snippet_mode = snippet_mode

    def snippet_functionality(self, d):
        ''' If snippet mode is on, only a portion of each file will be
        used.
        '''
        if self.snippet_mode:
            return d[1:12]
        else:
            return d[1:]
    
    def fill_unfilled_lists(self, container, desired_length, val="NaN"):
        ''' Takes a dictionary of lists and fills the lists with a
        value (default "NaN") to a desired length.
        '''
        for item in container:
            current_length = len(container[item])
            diff_length = desired_length - current_length
            if current_length < desired_length:
                container[item].extend([val for i in range(diff_length)])
        return container

    def flatten_and_map(self, d, parent_key='', separator='/'):
        ''' Takes a possibly multi-dimensional dictionary and returns a
        modified one; specifically, along with the
        "flatten_and_map_list_handler" function, it returns a
        dictionary that is flat/linear/not-multi-dimensional and where
        nested values are mapped to keys that recursively indicate the
        nested container(s) they belonged to, much like a pathname in
        filesystem directories. If a list is encountered, flatten_list
        is called. It does the same thing, just for lists.
        '''
        items = []
        for key, value in d.items():
            new_key = parent_key + separator + key if parent_key else key
            if isinstance(value, dict):
                items.extend(self.flatten_and_map(value, new_key,
                    separator=separator).items())
            elif isinstance(value, list):
                items.extend(self.flatten_and_map_list_handler(value,
                    new_key, separator=separator).items())
            else:
                items.append((new_key, value))
        return dict(items)

    def flatten_and_map_list_handler(self, l, parent_key='', separator='/'):
        """ Handles lists for flatten_and_map.
        """
        items = []
        for i, value in enumerate(l):
            new_key = (parent_key + separator + str(i))\
                # if parent_key else str(i)
            if isinstance(value, dict):
                items.extend(self.flatten_and_map(value, new_key,
                    separator=separator).items())
            elif isinstance(value, list):
                items.extend(self.flatten_and_map_list_handler(value, new_key,
                    separator=separator).items())
            else:
                items.append((new_key, value))
        return dict(items)

    def format_halstead_test_values(self, d):
        ''' The "halstead_before" and "halstead_after" values are
        integers, but Tigress_test2 formats them like so:

            "halstead_before": "halstead.tigress_obf.length = 12",
            "halstead_after": "halstead.tigress_obf.length = 487"
        
        This function removes "halstead.tigress_obf.length = " and
        "halstead.tigress_obf.length = " from their values,
        respectively. If the halstead test didn't run (for example, the
        obfuscated code threw an error), then 0 will be a placeholder.
        '''
        if isinstance(d["halstead_before"], int):
            d["halstead_before"] = int(d["halstead_before"][30:])
        else:
            d["halstead_before"] = 0
        
        if isinstance(d["halstead_after"], int):
            d["halstead_after"] = int(d["halstead_after"][30:])
        else:
            d["halstead_after"] = 0
        return d

    def update_nonexistant_keys(self, df, d):
        for key in d:
            if key not in df:
                df.update({key:[]})
        return df

    def populate_df_with_data(self, df, d, count):
        ''' Takes a dataframe and a dictionary and returns the
        dataframe updated with the dictionary. Because some keys only
        begin to appear some ways in to the data_to_df function, this
        function checks if they exist and creates them if they don't.

        It also takes "count" (representing the total number of test
        cases that have been processed) to make lower-level tasks
        easier.
        '''
        df = self.update_nonexistant_keys(df, d)
        df = self.fill_unfilled_lists(df, desired_length=count)

        for key, value in d.items():
            df[key].append(value) if value != "" else df[key].append("None")
                
        return df

    def loop_through_data(self, df, d):
        """
        """
        count = 0
        for sub_d in d:
            sub_d = self.flatten_and_map(sub_d)
            sub_d = self.format_halstead_test_values(sub_d)
            
            df = self.populate_df_with_data(df, sub_d, count=count)
            count += 1
            
        return df, count

    def data_to_df(self, file_name_list):
        ''' Takes a list of filenames containing Tigress_test data and
        outputs them as a pandas dataframe with appropriately-named
        labels.
        '''
        df = {}
        if isinstance(file_name_list, str):
            file_name_list = [file_name_list]
        for file_name in file_name_list:
            d = json.load(open(file_name, "r"))['results']
            d = self.snippet_functionality(d)  # shorten data,
                                               # if applicable
            df, count = self.loop_through_data(df, d)
        df = self.fill_unfilled_lists(df, desired_length=count)

        print("Data exported as pandas dataframe.")
        return pd.DataFrame.from_dict(df)

    def gen_file_list(filename, n_files):
        file_name_list = []
        for i in range(n_files):
            file_name_list.append(filename.format(str(i+1)))

class Machine_Learning:
    '''
        To-do!
    '''
    def __init__(self, type="kmodes"):
        self.type = type
    
main()