import json
import pandas as pd

class Process_Json:
    def __init__(self, file_names_list=[], snippet=False):
        self.snippet = snippet
        self.file_names_list = file_names_list

    def _snippet_functionality(self, d):
        """Self explanatory"""
        if self.snippet:
            return d[1:12]
        else:
            return d[1:]
    
    def _fill_unfilled_lists(self, container, desired_length, val="NaN"):
        """ Takes a dictionary of lists and 
        fills the lists with a value (default "NaN") to a desired length.
        """
        for item in container:
            current_length = len(container[item])
            diff_length = desired_length - current_length
            if current_length < desired_length:
                container[item].extend([val for i in range(diff_length)])
        return container

    def _flatten_and_map(self, d, parent_key='', separator='/'):
        """ Takes a possibly-multi-dimensional dictionary and returns a
        modified one;specifically, along with the "flatten_list"
        function, it returns a dictionary:

        1. That is flat/linear/not-multi-dimensional.
        2. Where nested values are mapped to keys that recursively
        indicate the nested container(s) they belonged to, like a
        pathname in filesystem directory traversal.
        
        If a list is encountered, flatten_list is called. Refer to that
        function to see how it handles lists (it's pretty similar).
        """

        items = []
        for key, value in d.items():
            new_key = parent_key + separator + key if parent_key else key
            if isinstance(value, dict):
                items.extend(self._flatten_and_map(value, new_key,
                    separator=separator).items())
            elif isinstance(value, list):
                items.extend(self._flatten_and_map_list_handler(value,
                    new_key, separator=separator).items())
            else:
                items.append((new_key, value))
        return dict(items)

    def _flatten_and_map_list_handler(self, l, parent_key='', separator='/'):
        """ Handles lists for flatten_and_map.
        """
        items = []
        for i, value in enumerate(l):
            new_key = (parent_key + separator + str(i))\
                # if parent_key else str(i)
            if isinstance(value, dict):
                items.extend(self._flatten_and_map(value, new_key,
                    separator=separator).items())
            elif isinstance(value, list):
                items.extend(self._flatten_and_map_list_handler(value, new_key,
                    separator=separator).items())
            else:
                items.append((new_key, value))
        return dict(items)

    def _format_halstead_test_values(self, d):
        """ The "halstead_before" and "halstead_after" values are
        integers, but Tigress_Test2 formats them like so:

            "halstead_before": "halstead.tigress_obf.length = 12",
            "halstead_after": "halstead.tigress_obf.length = 487"
        
        This function removes "halstead.tigress_obf.length = " and
        "halstead.tigress_obf.length = " from their values,
        respectively. If the halstead test didn't run (for example, the
        obfuscated code threw an error), then 0 will be a placeholder.

        I need to implement this better.
        """
        if isinstance(d["halstead_before"], int):
            d["halstead_before"] = int(d["halstead_before"][30:])
        else:
            d["halstead_before"] = 0
        
        if isinstance(d["halstead_after"], int):
            d["halstead_after"] = int(d["halstead_after"][30:])
        else:
            d["halstead_after"] = 0
        return d

    def _update_nonexistant_keys(self, df, d):
        for key in d:
            if key not in df:
                df.update({key:[]})
        return df

    def _populate_df_with_data(self, df, d, count):
        """ Takes a dataframe and a dictionary and returns the
        dataframe updated with the dictionary. Because some keys only
        begin to appear some ways in to the data_to_df function, this
        function checks if they exist and creates them if they don't.

        Takes count to make lower tasks easy.
        """
        df = self._update_nonexistant_keys(df, d)
        df = self._fill_unfilled_lists(df, count=count)

        for key, value in d.items():
            df[key].append(value) if value != "" else df[key].append("None")
                
        return df

    def _loop_through_data(self, df, d):
        """
        """
        count = 0
        for sub_d in d:
            sub_d = self._flatten_and_map(sub_d)
            sub_d = self._format_halstead_test_values(sub_d)
            
            df = self._populate_df_with_data(df, sub_d, count=count)
            count += 1
        return df, count

    def data_to_df(self):
        """
        """
        df = {}
        
        for file_name in self.file_names_list:
            d = json.load(open(file_name, "r"))['results']
            d = self._snippet_functionality(d)
            df, count = self._loop_through_data(df, d)

        df = self._fill_unfilled_lists(df, count=count)
        return pd.DataFrame.from_dict(df)