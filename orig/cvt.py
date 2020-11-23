# http://unusedino.de/ec64/technical/formats/cvt.html

import sys
import math
import pprint
import os

def decode_cvt(filename):
	data = bytearray(open(filename, "rb").read())

	dentry    = data[:0x1e]
	signature = data[0x1e:0x3a]
	empty     = data[0x3a:0xfe]
	fhdr      = bytearray([0, 0xff]) + data[0xfe:0x1fc]
	index     = data[0x1fc:0x2fa]
	data      = data[0x2fa:]

	if signature[:23] == "PRG formatted GEOS file":
		pass
	elif signature[:23] == "SEQ formatted GEOS file":
		raise Exception('SEQ CVT')
	else:
		print(signature)
		raise Exception('Missing signature')

	broken = False

	records = []
	for i in range(0, 127):
		a1 = index[i * 2];
		a2 = index[i * 2 + 1];

		if a1 == 0x00 and a2 == 0xFF:
			records.append(None);
		else:
			if broken:
				chain_size = a1 * 254 + a2;
				skip_size = chain_size;
			else:
				chain_size = (a1 - 1) * 254 + a2 - 1;
				skip_size = a1 * 254;
			records.append(data[:chain_size])
			data = data[skip_size:]

	return {
		"dentry" : dentry,
		"fhdr"   : fhdr,
		"records": records
	}

def encode_cvt(filename, cvt):
	data = bytearray()
	data.extend(cvt["dentry"])
	signature = (bytearray("PRG formatted GEOS file", "utf-8") + bytearray(256 * [0]))[:224]
	data.extend(signature)
	data.extend(cvt["fhdr"][2:])
	for i in range(0, 127):
		r = cvt["records"][i]
		if r:
			a1 = int(math.ceil(len(r)/254.0))
			a2 = len(r) % 254
			if (a2 != 0):
				a2 += 1
			data.extend([a1, a2])
		else:
			data.extend([0x00, 0xff])
	for r in cvt["records"]:
		if r:
			data.extend(r)
			a1 = int(math.ceil(len(r)/254.0))
			data.extend([0] * (a1 * 254 - len(r)))

	open(filename, "wb").write(data)

res = decode_cvt(sys.argv[1])
#pprint.pprint(res)

outname = os.path.splitext(sys.argv[1])[0] + ".vlir"

os.mkdir(outname)
open(outname + "/fhdr", "wb").write(res["fhdr"])
for i in range(0, 126):
	if res["records"][i]:
		open(outname + "/{:02x}".format(i), "wb").write(res["records"][i])

