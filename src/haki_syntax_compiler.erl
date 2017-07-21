%%%-------------------------------------------------------------------
%%% @doc Using the simple erl_syntax module to build a AST before
%%%      compiling the module. This is way more stable than the ASM
%%%      compiler and thus should be used for most cases.
%%% @end
%%%-------------------------------------------------------------------
-module(haki_syntax_compiler).

-behavior(haki_compiler).

-include("internal.hrl").
-include("types.hrl").

-export([
    compile/2,
    compile_bucket/2
]).

-spec compile(cache_module_name(), cache_value()) -> compile_ret().
compile(ModName, Val) ->
    Forms = forms(ModName, Val),
    Compile = ?timed(compile, compile:forms(Forms, ?COMPILER_OPTS)),

    case Compile of
        {ok, Module, Bin} ->
            code:soft_purge(Module),
            Filename = atom_to_list(ModName) ++ ".erl",
            {module, Module} = code:load_binary(Module, Filename, Bin),

            ok;
        Error ->
            Error
    end.

compile_bucket(ModName, Map) ->
    Forms = forms_bucket(ModName, Map),
    Compile = ?timed(compile, compile:forms(Forms, ?COMPILER_OPTS)),

    case Compile of
        {ok, Module, Bin} ->
            code:soft_purge(Module),
            Filename = atom_to_list(ModName) ++ ".erl",
            {module, Module} = code:load_binary(Module, Filename, Bin),

            ok;
        Error ->
            Error
    end.

forms(ModName, Val) ->
    ?timed(forms,
           begin
               ModuleAttr = erl_syntax:attribute(
                   erl_syntax:atom(module),
                   [erl_syntax:atom(ModName)]),

               GetExport = erl_syntax:arity_qualifier(
                   erl_syntax:atom(?GET_FUNC),
                   erl_syntax:integer(0)),

               CompileInfoExport = erl_syntax:arity_qualifier(
                   erl_syntax:atom(?INFO_FUNC),
                   erl_syntax:integer(0)),

               ExportsAttr = erl_syntax:attribute(
                   erl_syntax:atom(export), [
                       erl_syntax:list(
                           [GetExport, CompileInfoExport])]),

               Functions = [get_function(Val), compile_info_function()],
               Attributes = [ModuleAttr, ExportsAttr],
               Module = Attributes ++ Functions,

               [erl_syntax:revert(X) || X <- Module]
           end).

forms_bucket(ModName, Map) ->
    ?timed(forms,
           begin
               ModuleAttr = erl_syntax:attribute(
                   erl_syntax:atom(module),
                   [erl_syntax:atom(ModName)]),

               GetExport = erl_syntax:arity_qualifier(
                   erl_syntax:atom(?GET_FUNC),
                   erl_syntax:integer(1)),

               CompileInfoExport = erl_syntax:arity_qualifier(
                   erl_syntax:atom(?INFO_FUNC),
                   erl_syntax:integer(0)),

               ExportsAttr = erl_syntax:attribute(
                   erl_syntax:atom(export), [
                       erl_syntax:list(
                           [GetExport, CompileInfoExport])]),

               Functions = [get_function_map(Map), compile_info_function()],
               Attributes = [ModuleAttr, ExportsAttr],
               Module = Attributes ++ Functions,

               [erl_syntax:revert(X) || X <- Module]
           end).


%% For large lists, parallelize them
get_function(Val) when is_list(Val) andalso length(Val) > ?LARGE_LIST_LENGTH ->
    Vals = rpc:pmap({erl_syntax, abstract}, [], Val),
    erl_syntax:function(
        erl_syntax:atom(?GET_FUNC),
        [erl_syntax:clause([], [], [erl_syntax:list(Vals)])]);

get_function(Val) ->
    erl_syntax:function(
        erl_syntax:atom(?GET_FUNC),
        [erl_syntax:clause([], [], [erl_syntax:abstract(Val)])]).


get_function_map(Map) ->
    Clauses = maps:fold(
        fun(Key, Val, Acc) ->
            Acc ++ [get_function_clause(Key, Val)]
        end, [], Map),

    UnknownClause = erl_syntax:clause(
        [erl_syntax:variable('_')],
        [],
        [erl_syntax:atom(key_not_found)]),

    erl_syntax:function(
        erl_syntax:atom(?GET_FUNC),
        Clauses ++ [UnknownClause]).

get_function_clause(Key, Val) ->
    erl_syntax:clause(
        [erl_syntax:atom(Key)],
        [],
        [erl_syntax:abstract(Val)]).

compile_info_function() ->
    CompileDate = now_ms(),
    erl_syntax:function(
        erl_syntax:atom(?INFO_FUNC),
        [erl_syntax:clause([], [], [erl_syntax:integer(CompileDate)])]).

now_ms() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs) * 1000 + round(MicroSecs / 1000).