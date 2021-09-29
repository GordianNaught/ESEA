compile: esea.erl main.erl
	erlc ./esea.erl
	erlc ./main.erl
run:
	erl -eval "main:main()."
