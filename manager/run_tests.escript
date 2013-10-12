%% "escript run_tests.escript"
%% "escript run_tests.escript verbose"
%% replace "escript" with "escript.exe" in windows

main(Params) ->
	Options = case erlang:length(Params) of
				1 -> 
					[Verbose] = Params,
					[erlang:list_to_atom(Verbose)];
				_ -> []
			end,

	c:cd("ebin"),
	eunit:test({dir, "."}, Options).

