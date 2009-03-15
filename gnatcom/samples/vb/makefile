all: AdaToVB VBtoAda

.PHONY: AdaToVB
AdaToVb:
	make -C AdaToVB/ada

.PHONY: VBtoAda
VBtoAda:
	make -C VBtoAda/ada

clean:
	make -C AdaToVB/ada clean
	make -C VBtoAda/ada clean
