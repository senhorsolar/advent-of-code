SC=swiftc
SRC=src

utils.swiftmodule: utils.swift
	$(SC) -emit-module -emit-library $<

day%: utils.swiftmodule
	$(SC) -I. -L. -lutils $(SRC)/$@.swift
	./$@

clean:
	rm -f *.swiftdoc *.swiftsourceinfo *.swiftmodule *.dylib day*
