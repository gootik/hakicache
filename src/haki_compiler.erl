%%%-------------------------------------------------------------------
%%% @doc The compiler behavior and module that controls the compiling
%%%      of the cache key/val.
%%% @end
%%%-------------------------------------------------------------------
-module(haki_compiler).

-include("internal.hrl").
-include("types.hrl").

-export([
    compile/3,

    mod_name/1
]).

-callback compile(cache_module_name(), cache_value()) -> compile_ret().

-spec compile(cache_key(), cache_value(), cache_options()) -> ok.
compile(Key, Val, Options) ->
    {ok, Binary} = case Options of
        #{compiler := haki_default_compiler} ->
            compile(Key, Val);

        #{compiler := Compiler} ->
            ModName = mod_name(Key),
            Compiler:compile(ModName, Val)
    end,

    case Options of
        #{save_snapshot := true} ->
            FileName = atom_to_list(mod_name(Key)) ++ ".beam",
            file:write_file(FileName, Binary),

            ok;
        _ ->
            ok
    end.

-spec compile(cache_key(), cache_value()) -> compile_ret().
compile(Key, Val) when is_list(Val) andalso length(Val) > ?LARGE_LIST_LENGTH ->
    ModName = mod_name(Key),
    haki_asm_compiler:compile(ModName, Val);

compile(Key, Val) ->
    ModName = mod_name(Key),
    haki_syntax_compiler:compile(ModName, Val).

-spec mod_name(cache_key()) -> atom().
mod_name(Key) ->
    list_to_atom("haki_" ++ atom_to_list(Key)).