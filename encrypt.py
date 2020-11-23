#!/usr/bin/env python3

start_address_r0 = 0x0400
start_address_r1 = 0x3244

# read symbols
symbols = {}
f = open('build/current/symbols.txt', 'r')
for l in f.readlines():
	l = l.split()
	symbols[l[2][1:]] = int(l[1], 16)

# checksum record 1 for the key
data = bytearray(open('build/current/geoWrite-1_plain.bin', 'rb').read())

checksum = 0
for c in data:
	checksum += c

checksum -= data[symbols["serial"] - start_address_r1]
checksum -= data[symbols["serial"] - start_address_r1 + 1]
checksum -= data[symbols["protExecTrack"] - start_address_r1]
checksum -= data[symbols["protExecSector"] - start_address_r1]
checksum -= data[symbols["protSerialTrack"] - start_address_r1]
checksum -= data[symbols["protSerialSector"] - start_address_r1]
checksum += 0xde
checksum += 0xde
checksum += 0xde
checksum += 0xde
checksum &= 0xFF

# encrypt record 1
for i in range(0, len(data)):
	data[i] ^= 0xde

open('build/current/geoWrite-1.bin', "wb").write(data)

# encrypt part of record 0
data = bytearray(open('build/current/geoWrite-0_plain.bin', 'rb').read())

start = symbols['r0_encrypted_start'] - start_address_r0
end = symbols['r0_encrypted_end'] - start_address_r0

for i in range(start, end):
	data[i] = data[i] ^ checksum

open('build/current/geoWrite-0.bin', "wb").write(data)


