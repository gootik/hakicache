%%%-------------------------------------------------------------------
%%% @doc NOTE: Super hacky (but fast) at the moment. If you want to use
%%%      this compiler you have to force it by forcing it. For example:
%%%      haki:cache(K, V, haki_beam_compiler).
%%%
%%%      TODO: Explain what's going on.
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
            {module, ModName} = code:load_binary(ModName, F, Bin),

            {ok, Bin};
        Error ->
            error_logger:error_msg("[hakicache] - Could not build module: ~p", [Error]),

            Error
    end.

-spec beam_asm(cache_module_name(), cache_value()) -> {ok, binary()} | error.
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