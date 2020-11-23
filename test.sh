make clean > /dev/null &&
make all LOCALE=en > /dev/null &&
make all LOCALE=de > /dev/null &&

RESULT=$(
(
md5 -r build/en/geoWrite-0.bin orig/geoWrite_en_2.1_1988-07-06.vlir/00
md5 -r build/en/geoWrite-1.bin orig/geoWrite_en_2.1_1988-07-06.vlir/01
md5 -r build/en/geoWrite-2.bin orig/geoWrite_en_2.1_1988-07-06.vlir/02
md5 -r build/en/geoWrite-3.bin orig/geoWrite_en_2.1_1988-07-06.vlir/03
md5 -r build/en/geoWrite-4.bin orig/geoWrite_en_2.1_1988-07-06.vlir/04
md5 -r build/en/geoWrite-5.bin orig/geoWrite_en_2.1_1988-07-06.vlir/05
md5 -r build/en/geoWrite-6.bin orig/geoWrite_en_2.1_1988-07-06.vlir/06
md5 -r build/en/geoWrite-7.bin orig/geoWrite_en_2.1_1988-07-06.vlir/07
md5 -r build/en/geoWrite-8.bin orig/geoWrite_en_2.1_1988-07-06.vlir/08
md5 -r build/en/geoWrite-fhdr.bin orig/geoWrite_en_2.1_1988-07-06.vlir/fhdr
md5 -r build/en/protection.bin orig/protection.bin

md5 -r build/de/geoWrite-0.bin orig/geoWrite_de_2.1_1989-01-03.vlir/00
md5 -r build/de/geoWrite-1.bin orig/geoWrite_de_2.1_1989-01-03.vlir/01
md5 -r build/de/geoWrite-2.bin orig/geoWrite_de_2.1_1989-01-03.vlir/02
md5 -r build/de/geoWrite-3.bin orig/geoWrite_de_2.1_1989-01-03.vlir/03
md5 -r build/de/geoWrite-4.bin orig/geoWrite_de_2.1_1989-01-03.vlir/04
md5 -r build/de/geoWrite-5.bin orig/geoWrite_de_2.1_1989-01-03.vlir/05
md5 -r build/de/geoWrite-6.bin orig/geoWrite_de_2.1_1989-01-03.vlir/06
md5 -r build/de/geoWrite-7.bin orig/geoWrite_de_2.1_1989-01-03.vlir/07
md5 -r build/de/geoWrite-8.bin orig/geoWrite_de_2.1_1989-01-03.vlir/08
md5 -r build/de/geoWrite-fhdr.bin orig/geoWrite_de_2.1_1989-01-03.vlir/fhdr
md5 -r build/de/protection.bin orig/protection.bin
) | cut -d " " -f 1 | uniq -c | sed -e "s/^ *//" | cut -d " " -f 1 | uniq

)

if [ "$RESULT" = "2" ]; then
	echo "PASS"
	exit 0
else
	echo "FAIL"
	exit 1
fi

