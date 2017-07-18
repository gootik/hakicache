-ifdef(DEBUG).
-define(debug_enabled(Code), Code).
-else.
-define(debug_enabled(Code), ok).
-endif.

-define(timed(TimedName, Code),
    begin
        ?debug_enabled(
            begin
                {StartMegaSecs, StartSecs, StartMicroSecs} = os:timestamp(),
                StartMs = (StartMegaSecs * 1000000 + StartSecs) * 1000 + round(StartMicroSecs / 1000)
            end
        ),

        Result = Code,

        ?debug_enabled(
            begin
                {EndMegaSecs, EndSecs, EndMicroSecs} = os:timestamp(),
                EndMs = (EndMegaSecs * 1000000 + EndSecs) * 1000 + round(EndMicroSecs / 1000),

                io:format(user, <<"~p took ~pms~n">>, [TimedName, EndMs - StartMs])
            end
        ),

        Result
    end).