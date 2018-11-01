.PHONY: index.html

index.html: index.jingoo
	jingoo -i $< -o $@

build: index.html
	dune build --profile release try_jingoo.bc.js
	cp _build/default/try_jingoo.bc.js try_jingoo.js

clean:
	rm -f try_jingoo.js
	dune clean
