AS = ca65
ASFLAGS = -g

LOCALE ?= en

ifeq ($(LOCALE),en)
ASFLAGS+=-D LOCALE=0 -I en
else ifeq ($(LOCALE),de)
ASFLAGS+=-D LOCALE=1 -I de
else
$(error Unsupported LOCALE)
endif

BUILD_DIR = build/$(LOCALE)

SOURCES = \
	zeropage.s \
	geoWrite-0.s \
	geoWrite-1.s \
	geoWrite-2.s \
	geoWrite-3.s \
	geoWrite-4.s \
	geoWrite-5.s \
	geoWrite-6.s \
	geoWrite-7.s \
	geoWrite-8.s \
	geoWrite-fhdr.s

OBJS=$(SOURCES:.s=.o)

PREFIXED_OBJS = $(addprefix $(BUILD_DIR)/, $(OBJS))

$(BUILD_DIR)/%.o: %.s
	@mkdir -p `dirname $@`
	$(AS) $(ASFLAGS) $< -o $@

all: $(PREFIXED_OBJS) $(BUILD_DIR)/protection.o

	rm -f build/current
	ln -s $(LOCALE) build/current
	ld65 -C geoWrite.cfg $(PREFIXED_OBJS) -o $(BUILD_DIR)/geoWrite-0_plain.bin -Ln build/current/symbols.txt

	ld65 -C protection.cfg \
		$(BUILD_DIR)/protection.o \
		-o $(BUILD_DIR)/protection.bin

	./encrypt.py

	rm -f build/current

cvt: cvt.s geoWrite-cvt.cfg $(PREFIXED_OBJS) $(BUILD_DIR)/protection.o
	rm -f build/current
	ln -s $(LOCALE) build/current
	$(AS) $(ASFLAGS) cvt.s -o $(BUILD_DIR)/cvt.o
	ld65 -C geoWrite-cvt.cfg $(BUILD_DIR)/cvt.o -o $(BUILD_DIR)/geoWrite.cvt -Ln $(BUILD_DIR)/cvtsymbols.lbl
	rm -f build/current

clean:
	rm -rf build
