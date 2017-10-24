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

            {ok, Bin};
        Error ->
            error_logger:error_msg("[hakicache_syntax_compiler] - Could not build module: ~p", [Error]),

            Error
    end.

-spec compile_bucket(cache_module_name(), cache_bucket_value()) -> compile_ret().
compile_bucket(ModName, Map) ->
    Forms = forms_bucket(ModName, Map),
    Compile = ?timed(compile, compile:forms(Forms, ?COMPILER_OPTS)),

    case Compile of
        {ok, Module, Bin} ->
            code:soft_purge(Module),
            Filename = atom_to_list(ModName) ++ ".erl",
            {module, Module} = code:load_binary(Module, Filename, Bin),

            {ok, Bin};
        Error ->
            error_logger:error_msg("[hakicache_syntax_compiler] - Could not build bucket module: ~p", [Error]),

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

               ExportsAttr = erl_syntax:attribute(
                   erl_syntax:atom(export), [
                       erl_syntax:list(
                           [GetExport])]),

               Functions = [get_function(Val)],
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

               ExportsAttr = erl_syntax:attribute(
                   erl_syntax:atom(export), [
                       erl_syntax:list(
                           [GetExport])]),

               Functions = [get_function_bucket(Map)],
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

get_function_bucket(Map) ->
    MapClauses = maps:fold(
        fun(Key, Val, Clauses) ->
            Clauses ++ [get_function_clause(Key, Val)]
        end, [], Map),

    Clauses = MapClauses ++ [unknown_key_clause()],

    erl_syntax:function(
        erl_syntax:atom(?GET_FUNC),
        Clauses).

get_function_clause(Key, Val) ->
    KeyClause = key_clause(Key),
    erl_syntax:clause([KeyClause], [], [erl_syntax:abstract(Val)]).

unknown_key_clause() ->
    erl_syntax:clause([erl_syntax:variable("_")], [], [erl_syntax:atom(bad_key)]).

key_clause(Key) when is_atom(Key) ->
    erl_syntax:atom(Key);

key_clause(Key) when is_integer(Key) ->
    erl_syntax:integer(Key);

key_clause(Key) when is_binary(Key) ->
    String = erl_syntax:string(binary_to_list(Key)),
    erl_syntax:binary([erl_syntax:binary_field(String)]).