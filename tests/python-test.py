import sys

sys.path.insert(0, "@METACALL_LIBRARY_PATH@")

from _py_port import *

print(metacall_load_from_file('mock', ['test.mock']));
print(metacall('three_str', 'a', 'b', 'c'));
