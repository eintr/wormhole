-module(cryptor).

-export([en/2, de/2]).

en(RawData, _Key) ->
	RawData.

de(CryData, _Key) ->
	CryData.

