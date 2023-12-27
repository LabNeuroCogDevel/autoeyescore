.SUFFIXES:
.PHONY: all_asc
all_asc: $(subst .edf,.asc.gz,$(wildcard example/sub*/*/*/*edf))
%.asc.gz: %.edf
	edf2asc $^
