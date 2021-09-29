compile: JJSON/jjson.erl esea.erl main.erl
	make -C ./JJSON
	cp ./JJSON/jjson.beam .
	erlc ./esea.erl
	erlc ./main.erl
run:
	erl -eval "main:main()."
