# A Makefile for managing files and scripts for my blog
#
# --- Macros ---
#PATH = content
#BUILD = build.sh
# --- Targets ---
publish:
	./build.sh

cleanall:
	rm -rf public

cleancache:
	@echo "removing cache files"
	rm -rf ./public/*~

watch: ./build.sh ./content
	@find . -type f | entr ./build.sh

.PHONY: cleanall cleancache watch publish
