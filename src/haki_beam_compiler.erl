%%%-------------------------------------------------------------------
%%% @doc Using the syntax compiler has an over head of creating an AST
%%%      and having the compiler to lint/expand records/optimize.
%%%      Moreover, building an AST and compiling has a huge overhead on
%%%      memory, if the data is large.
%%%
%%%      However for the most usecases of this library you are caching
%%%      a literal value, so if we hack the ASM file on the fly it should
%%%      produce a decent module that can be then compiled and loaded.
%%% @end
%%%-------------------------------------------------------------------
-module(haki_beam_compiler).

-behavior(haki_compiler).

-include("internal.hrl").
-include("types.hrl").

-export([
    compile/2
]).


-spec compile(cache_module_name(), cache_value()) -> compile_ret().
compile(ModName, Val) ->
    Compile = beam_asm(ModName, Val),

    case Compile of
        {ok, Bin} ->
            code:soft_purge(ModName),
            F = atom_to_list(ModName) ++ ".erl",
            {module, ModName} = code:load_binary(ModName, F, Bin);

        Error ->
            io:format(user, "~p~n", [Error]),
            Error
    end.

beam_asm(ModName, Val) ->
    Code = {ModName,
            [{get, 0}, {module_info, 0}, {module_info, 1}],
            [],
            [{function, get, 0, 2, [
                {label, 1},
                {func_info, {atom, ModName}, {atom, get}, 0},
                {label, 2},
                {move,
                 {literal, Val},
                 {x,0}},
                return]},
             {function,module_info,0,4,
              [{label,3},
               {func_info,{atom,ModName},{atom,module_info},0},
               {label,4},
               {move,{atom,ModName},{x,0}},
               {call_ext_only,1,{extfunc,erlang,get_module_info,1}}]},
             {function,module_info,1,6,
              [{label,5},
               {func_info,{atom,ModName},{atom,module_info},1},
               {label,6},
               {move,{x,0},{x,1}},
               {move,{atom,ModName},{x,0}},
               {call_ext_only,2,{extfunc,erlang,get_module_info,2}}]}],
            7},
    Chunks = [],
    Source = atom_to_list(ModName) ++ ".S",
    Opts = ?COMPILER_OPTS,
    CompilerOpts = ?COMPILER_OPTS,

    beam_asm:module(Code, Chunks, Source, Opts, CompilerOpts).