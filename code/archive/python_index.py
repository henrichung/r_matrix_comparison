import timeit
import random

def list_lookup(lst, key):
    for k, v in lst:
        if k == key:
            return v

def set_lookup(s, key):
    for k, v in s:
        if k == key:
            return v

for size in [10, 100, 1000, 10000, 100000, 1000000, 10000000]:
    data_dict = {i: random.randint(0, 100) for i in range(size)}
    data_list = list(data_dict.items())
    data_set = set(data_dict.items())
    key_to_lookup = random.randint(0, size - 1)
    
    dict_time = timeit.timeit(lambda: data_dict.get(key_to_lookup), number=1)
    list_time = timeit.timeit(lambda: list_lookup(data_list, key_to_lookup), number=1)
    set_time = timeit.timeit(lambda: set_lookup(data_set, key_to_lookup), number=1)
    
    print(f"Size: {size}, Dict: {dict_time:.6f}, List: {list_time:.6f}, Set: {set_time:.6f}")

# Size: 10, Dict: 0.001016, List: 0.004416, Set: 0.001959
# Size: 100, Dict: 0.001049, List: 0.003385, Set: 0.016808
# Size: 1000, Dict: 0.001070, List: 0.023659, Set: 0.061930
# Size: 10000, Dict: 0.001154, List: 0.222599, Set: 1.280814
# Size: 100000, Dict: 0.001126, List: 3.924927, Set: 58.703029
# Size: 1000000, Dict: 0.001179, List: 113.052778, Set: 625.535511
# Size: 10000000, Dict: 0.001078, List: 2429.358179, Set: 888.002084