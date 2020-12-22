SC=swiftc
SRC=src
day?=1

.PHONY: clean

utils.swiftmodule: utils.swift
	$(SC) -emit-module -emit-library $<

day%: utils.swiftmodule
	if [ ! -f $(SRC)/$@.swift ]; then cp $(SRC)/template.swift $(SRC)/$@.swift; fi;
	$(SC) -I. -L. -lutils $(SRC)/$@.swift
	./$@

clean:
	rm -f *.swiftdoc *.swiftsourceinfo *.swiftmodule *.dylib day*
