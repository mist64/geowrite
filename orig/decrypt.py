#!/usr/local/bin/python3

import sys

data = bytearray(open(sys.argv[1], 'rb').read())

for i in range(0, len(data)):
	data[i] = data[i] ^ 0xde

open(sys.argv[2], "wb").write(data)


