-ifdef(DEBUG).
-define(timed(TimedName, Code),
    begin
        {StartMegaSecs, StartSecs, StartMicroSecs} = os:timestamp(),
        StartMs = (StartMegaSecs * 1000000 + StartSecs) * 1000 + round(StartMicroSecs / 1000),

        Result = Code,

        {EndMegaSecs, EndSecs, EndMicroSecs} = os:timestamp(),
        EndMs = (EndMegaSecs * 1000000 + EndSecs) * 1000 + round(EndMicroSecs / 1000),

        io:format(user, <<"~p took ~pms~n">>, [TimedName, EndMs - StartMs]),

        Result
    end
).
-else.
-define(timed(N, C), C).
-endif.

-define(DEFAULT_CACHE_OPTIONS, #{
    compiler => haki_default_compiler,
    save_binary => false
}).

-define(LARGE_LIST_LENGTH, 1000).

-define(GET_FUNC, get).
-define(INFO_FUNC, info).

-define(COMPILER_OPTS, [
    %% Turn off warnings for faster compiling time
    nowarn_unused_vars,
    nowarn_unused_vars,
    nowarn_export_all,
    nowarn_export_vars,
    nowarn_shadow_vars,
    nowarn_unused_import,
    nowarn_unused_function,
    nowarn_unused_record,
    nowarn_deprecated_function,
    nowarn_deprecated_type,
    nowarn_obsolete_guard,
    nowarn_untyped_record,
    nowarn_missing_spec,
    nowarn_missing_spec_all,
    nowarn_get_stacktrace,

    no_line_info,
    report_errors,
    report_warnings,
    binary
]).