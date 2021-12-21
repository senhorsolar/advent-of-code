SC=swiftc
SRC=src
day?=1
FLAGS=-O 

.PHONY: clean

utils.swiftmodule: $(SRC)/utils.swift
	$(SC) -emit-module -emit-library $<

day%: utils.swiftmodule
	if [ ! -f $(SRC)/$@.swift ]; then cp $(SRC)/template.swift $(SRC)/$@.swift; fi;
	$(SC) $(FLAGS) -I. -L. -lutils $(SRC)/$@.swift
	./$@

clean:
	rm -f *.swiftdoc *.swiftsourceinfo *.swiftmodule *.dylib day*
