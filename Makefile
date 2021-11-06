.PHONY: clean

publish:
	./build.sh

clean:
	rm -rf public
watch:
	find -iname "*.org" | entr ./build.sh
