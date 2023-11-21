build:
	@rebar3 compile

clean:
	@rebar3 clean

fmt:
	@erlfmt -w '{src,include,priv,test}/**/*.{erl,hrl,app.src,es}'
