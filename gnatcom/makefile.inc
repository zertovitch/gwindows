ifdef UNIXTOOLS
	DIRSEP = /
	DELTOOL = rm
	DELDIRTOOL = rm -rf
	LOCKTOOL = chmod 444
	UNLOCKTOOL = chmod 666
else
	DIRSEP = \\
	DELTOOL = del
	DELDIRTOOL = rmdir
	LOCKTOOL = attrib +R
	UNLOCKTOOL = attrib -R
endif
 
ifdef DEBUG
	ADAOPTS += -g
endif

ifdef RELEASE
	POSTOPTS += -largs -s
endif

.PHONY: $(PACKAGES)

.PHONY: all
all: ADAOPTS += -gnatq
all: $(PACKAGES) $(SUB_PACKAGES)

.PHONY: debug
debug: ADAOPTS += -gnatq -g
debug: SUBOPT = debug
debug: $(PACKAGES) $(SUB_PACKAGES)

.PHONY: release
release: ADAOPTS += -gnatq -O2
release: POSTOPTS += -largs -s
release: SUBOPT = release
release: $(PACKAGES) $(SUB_PACKAGES) compress

.PHONY: maxcheck
maxcheck: ADAOPTS += -gnatwuwe -gnaty3abcefhikmprt
maxcheck: SUBOPT = maxcheck
maxcheck: $(PACKAGES) $(SUB_PACKAGES)

.PHONY: gnatcheck
gnatcheck: ADAOPTS += -gnaty -gnatwae
gnatcheck: SUBOPT = gnatcheck
gnatcheck: $(PACKAGES) $(SUB_PACKAGES)

.PHONY: compress
compress:
	-upx *.exe *.dll

.PHONY: submake $(SUB_PACKAGES)
submake: $(SUB_PACKAGES)

$(SUB_PACKAGES):
	make $(SUBOPT) -C $@

SUB_CLEAN = $(addprefix 9, $(SUB_PACKAGES))

.PHONY: subclean $(SUB_CLEAN)
subclean: $(SUB_CLEAN)

$(SUB_CLEAN): CURRENT = $@
$(SUB_CLEAN):
	make clean -C $(subst 9, ,$(CURRENT))

.PHONY: clean
clean: $(SUB_CLEAN)
	-$(DELTOOL) *.ali
	-$(DELTOOL) *.o
	-$(DELTOOL) *.exe
	-$(DELTOOL) *.dll
	-$(DELTOOL) *~
	-$(DELTOOL) b~*.*
	-$(DELTOOL) *.c
	-$(DELTOOL) *.coff
ifdef CLEAN_ALSO
	-$(DELTOOL) $(CLEAN_ALSO)
endif
