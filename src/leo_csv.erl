%%======================================================================
%%
%% Leo CSV 
%% 
%% Copyright (c) 2012-2015 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------
%% CSV Parser for Erlang
%% @doc
%% @end
%%======================================================================
-module(leo_csv).

-export([parse/3, parse/4]).

-define(DEF_READ_AHEAD_SIZE, 65536).
-define(DEF_START_LINE, 1).
-define(DEF_COLUMN_HEADER, false).
-define(DEF_DELIMITER, $,).
-define(DEF_QUOTE, $").
-define(DEF_MAX_LINES, -1).

-type parse_options() :: [{column_header, boolean()} |
                          {delimiter, char()} |
                          {quote, char()} |
                          {start_line, pos_integer()} |
                          {max_lines, integer()} |
                          {read_ahead, pos_integer()}
                         ].

-type callback_fun() :: fun((EOF::boolean(),
                             Columns::list(string()),
                             RawLine::string(),
                             CallbackFunState::any()) -> {ok|abort, NewState::any()}).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------
%% @doc Parse a CSV file
-spec(parse(Filename, Opts, CallbackFun) ->
             {ok|abort, any()} when Filename::file:filename_all(),
                                    Opts::parse_options(),
                                    CallbackFun::callback_fun()).
parse(Filename, Opts, CallbackFun) when is_function(CallbackFun) ->
    parse(Filename, Opts, CallbackFun, []).

parse(Filename, Opts, CallbackFun, CallbackFunState) ->
    ReadAhead = proplists:get_value(read_ahead, Opts, ?DEF_READ_AHEAD_SIZE),
    {ok, F} = file:open(Filename, [read, raw, {read_ahead, ReadAhead}]),
    try
        SkipCount = proplists:get_value(start_line, Opts, ?DEF_START_LINE) - 1 +
                        case proplists:get_value(column_header, Opts, ?DEF_COLUMN_HEADER) of
                            true -> 1;
                            false -> 0
                        end,
        skip_lines(F, SkipCount),
        Delimiter = proplists:get_value(delimiter, Opts, ?DEF_DELIMITER),
        Quote = proplists:get_value(quote, Opts, ?DEF_QUOTE),
        MaxLines = proplists:get_value(max_lines, Opts, ?DEF_MAX_LINES),
        parse(F, file:read_line(F), Delimiter, Quote, MaxLines, CallbackFun, CallbackFunState)
    after
        file:close(F)
    end.

%% @private
-spec(skip_lines(IoDevice, SkipCount) ->
             ok when IoDevice::file:io_device(),
                     SkipCount::non_neg_integer()).
skip_lines(_F, 0) ->
    ok;
skip_lines(F, SkipCount) ->
    _ = file:read_line(F),
    skip_lines(F, SkipCount - 1).

%% @private
-spec(parse(F, Line, Delimiter, Quote, MaxLines, CallbackFun, CallbackFunState) ->
             {ok|abort, any()} when F::file:io_device(),
                                    Line::eof|{ok, string()|binary()}|{error, any()},
                                    Delimiter::char(),
                                    Quote::char(),
                                    MaxLines::integer(),
                                    CallbackFun::callback_fun(),
                                    CallbackFunState::any()).
parse(_F, eof, _Delimiter, _Quote, _MaxLines, CallbackFun, CallbackFunState) ->
    CallbackFun(true, [], [], CallbackFunState);
parse(_F, _Line, _Delimiter, _Quote, 0, CallbackFun, CallbackFunState) ->
    CallbackFun(true, [], [], CallbackFunState);
parse(F, {ok, Line}, Delimiter, Quote, MaxLines, CallbackFun, CallbackFunState) ->
    Fields = parse_line(Line, Delimiter, Quote),
    case CallbackFun(false, Fields, Line, CallbackFunState) of
        {abort, _} = Ret ->
            Ret;
        {ok, NewState}->
            parse(F, file:read_line(F), Delimiter, Quote, MaxLines - 1, CallbackFun, NewState)
    end.

%% @private
%% Parse one line
-spec(parse_line(Line, Delimiter, Quote) ->
             list() when Line::string()|binary(),
                         Delimiter::char(),
                         Quote::char()).
parse_line(Line, Delimiter, Quote) ->
    parse_line(Line, Delimiter, Quote, []).

parse_line([], _Delimiter, _Quote, Fields) ->
    lists:reverse(Fields);
parse_line([$\n], _Delimiter, _Quote, Fields) ->
    lists:reverse(Fields);
parse_line([Delimiter|Line], Delimiter, Quote,  Fields) ->
    parse_field(Line, Delimiter, Quote, Fields);
parse_line(Line, Delimiter, Quote, Fields) ->
    parse_field(Line, Delimiter, Quote, Fields).

%% @private
%% Parse one field
-spec(parse_field(Line, Delimiter, Quote, Fields) ->
             list() when Line::string()|binary(),
                         Delimiter::char(),
                         Quote::char(),
                         Fields::list()).
parse_field([Quote|Line], Delimiter, Quote, Fields) ->
    parse_field_q(Line, Delimiter, Quote, [], Fields);
parse_field(Line, Delimiter, Quote, Fields) ->
    parse_field(Line, Delimiter, Quote, [], Fields).

parse_field([Delimiter|_] = Line, Delimiter, Quote, Buf, Fields) ->
    parse_line(Line, Delimiter, Quote, [lists:reverse(Buf)|Fields]);
parse_field([], Delimiter, Quote, Buf, Fields) ->
    parse_line([], Delimiter, Quote, [lists:reverse(Buf)|Fields]);
parse_field([$\n], Delimiter, Quote, Buf, Fields) ->
    parse_line([], Delimiter, Quote, [lists:reverse(Buf)|Fields]);
parse_field([C|Line], Delimiter, Quote, Buf, Fields) ->
    parse_field(Line, Delimiter, Quote, [C|Buf], Fields).

%% @private
%% Parse one field enclosed the Quote character
-spec(parse_field_q(Line, Delimiter, Quote, Buf, Fields) ->
             list() when Line::string()|binary(),
                         Delimiter::char(),
                         Quote::char(),
                         Buf::string(),
                         Fields::list()).
parse_field_q([Quote,Quote|Line], Delimiter, Quote, Buf, Fields) ->
    parse_field_q(Line, Delimiter, Quote, [Quote|Buf], Fields);
parse_field_q([Quote|Line], Delimiter, Quote, Buf, Fields) ->
    parse_line(Line, Delimiter, Quote, [lists:reverse(Buf)|Fields]);
parse_field_q([C|Line], Delimiter, Quote, Buf, Fields) ->
    parse_field_q(Line, Delimiter, Quote, [C|Buf], Fields).

-ifdef(TEST).
parse_csv_line_test() ->
    ?assertEqual(["field","field with dq\"","","after empty field"], parse_line("field,field with dq\",,after empty field", $,, $")),
    ?assertEqual(["field","tailed empty","","","", ""], parse_line("field,tailed empty,,,,", $,, $")),
    ?assertEqual(["field","field enclosed dq"," hoge ","","", "field including \" enclosed dq"], parse_line("field,\"field enclosed dq\", hoge ,,,\"field including \"\" enclosed dq\"", $,, $")).

parse_tsv_line_test() ->
    ?assertEqual(["field","field with sq'","","after empty field"], parse_line("field\tfield with sq'\t\tafter empty field", $\t, $')),
    ?assertEqual(["field","tailed empty","","","", ""], parse_line("field\ttailed empty\t\t\t\t", $\t, $')),
    ?assertEqual(["field","field enclosed sq"," hoge ","","", "field including ' enclosed sq"], parse_line("field\t'field enclosed sq'\t hoge \t\t\t'field including '' enclosed sq'", $\t, $')).

parse_csv_test() ->
    File = "../test/test.csv",
    Opts = [],
    Fun = gen_parse_csv_test_callback(7),
    parse(File, Opts, Fun, 1).

parse_csv2_test() ->
    File = "../test/test.csv",
    Opts = [{column_header, true}, {start_line, 2}, {max_lines, 2}],
    Fun = gen_parse_csv_test_callback(2),
    parse(File, Opts, Fun, 1).

parse_csv3_test() ->
    File = "../test/test.csv",
    Opts = [],
    Fun = fun(_, _, _, State) ->
        {abort, State}
    end,
    parse(File, Opts, Fun).

parse_big_csv_test_() ->
    {timeout, 60, fun() ->
        File = "../test/english_indices_of_deprivation_2010.csv",
        Opts = [{read_ahead, 1048576}, {column_header, true}],
        Fun = fun(true, _, _, Line) ->
                %% EOF
                io:format(user, "[test]line:~p~n",[Line - 1]),
                {ok, Line - 1};
            (_, Fields, _, 32482 = Line) ->
                %% Last Line
                io:format(user, "[test]fields at the last row:~p~n",[Fields]),
                {ok, Line + 1};
            (_,_,_,Line) ->
                %% Other Lines
                {ok, Line + 1}
        end,
        {Time, _} = timer:tc(leo_csv, parse, [File, Opts, Fun, 1]),
        io:format(user, "[test]time:~p(sec)~n", [Time / 1000000]),
        ok
    end}.

divide_big_csv_test_() ->
    {timeout, 60, fun() ->
        File = "../test/english_indices_of_deprivation_2010.csv",
        Opts = [{read_ahead, 1048576}, {column_header, true}],
        FileSize = filelib:file_size(File),
        DivideNum = 4,
        %% Cleanup the tmp files generated by the prefious run
        os:cmd("rm -f /tmp/*.leofs"),
        Fun = fun(true, _, _, {IoDevice, _, _}) ->
                %% EOF
                ok = file:close(IoDevice),
                {ok, undef};
           (_, _Fields, RawLine, {IoDevice, SplitSize, CurrentSize}) when CurrentSize >= SplitSize ->
                %% Create a new file when the file size is over SplitSize
                ok = file:close(IoDevice),
                TmpFile = string:strip(os:cmd("mktemp --suffix=.leofs"), right, $\n),
                {ok, TmpIoDev} = file:open(TmpFile, [write, raw, append, delayed_write]),
                ok = file:write(TmpIoDev, RawLine),
                {ok, {TmpIoDev, SplitSize, string:len(RawLine)}};
            (_, _Fields, RawLine, {IoDevice, SplitSize, CurrentSize}) ->
                %% Append a new line to a current tmp file
                ok = file:write(IoDevice, RawLine),
                {ok, {IoDevice, SplitSize, string:len(RawLine) + CurrentSize}}
        end,
        TmpFile = string:strip(os:cmd("mktemp --suffix=.leofs"), right, $\n),
        {ok, TmpIoDev} = file:open(TmpFile, [write, raw, append, delayed_write]),
        parse(File, Opts, Fun, {TmpIoDev, FileSize div DivideNum, 0}),
        DividedFiles = os:cmd("ls /tmp/*.leofs"),
        ?assertEqual(DivideNum, length(string:tokens(DividedFiles, [$\n])))
    end}.

filter_csv_test() ->
    File = "../test/test.csv",
    Opts = [{column_header, true}],
    IsErlang = fun("Erlang") ->
                       ok;
                  (_) ->
                       {error, not_erlang}
               end,
    FilterFuns = [undef, IsErlang, undef, undef, undef, undef],
    Fun = gen_filter_csv_test_callback(FilterFuns),
    {ok, Filtered} = parse(File, Opts, Fun, []),
    ?assertEqual(1, length(Filtered)).

filter_csv2_test() ->
    File = "../test/test.csv",
    Opts = [{column_header, true}],
    HasMultiParadigm =
        fun("multiparadigm") ->
            ok;
        (_) ->
            {error, not_multiparadigm}
        end,
    HasSafeTypeSystem =
        fun(TypeSystemCSV) when is_list(TypeSystemCSV), length(TypeSystemCSV) > 0 ->
            TypeSystemFeatures = string:tokens(TypeSystemCSV, ","),
            case lists:member("safe", TypeSystemFeatures) of
                true -> ok;
                _ -> {error, can_not_be_safe}
            end;
        (_) ->
            {error, can_not_be_safe}
        end,
    FilterFuns = [undef, undef, HasMultiParadigm, HasSafeTypeSystem, undef, undef],
    Fun = gen_filter_csv_test_callback(FilterFuns),
    {ok, Filtered} = parse(File, Opts, Fun, []),
    ?assertEqual(1, length(Filtered)).

filter_csv_unicode_test() ->
    File = "../test/test.csv",
    Opts = [{column_header, true}],
    IsNadesiko = fun("なでしこ") ->
                       ok;
                  (_) ->
                       {error, not_nadesiko}
               end,
    FilterFuns = [undef, IsNadesiko, undef, undef, undef, undef],
    Fun = gen_filter_csv_test_callback(FilterFuns),
    {ok, Filtered} = parse(File, Opts, Fun, []),
    ?assertEqual(1, length(Filtered)).

gen_parse_csv_test_callback(ExpectedLine) ->
    fun(EOF, _Fields, _RawLine, Line) ->
        io:format(user, "[test]fields:~p~n", [_Fields]),
        case EOF of
            true ->
                ?assertEqual(ExpectedLine, Line - 1);
            _ ->
                void
        end,
        {ok, Line + 1}
    end.

gen_filter_csv_test_callback(FilterFuns) ->
    fun(false, Fields, _RawLine, Acc) ->
        case length(FilterFuns) =/= length(Fields) of
            true ->
                {abort, Acc};
            false ->
                case do_filter(Fields, FilterFuns) of
                    ok ->
                        {ok, [Fields|Acc]};
                    {error, {{field, Field},{reason, Reason}}}->
                        io:format(user, "[test]field:~p reason:~p~n", [Field, Reason]),
                        {ok, Acc}
                end
        end;
       (true,_,_,Acc) ->
            {ok, Acc}
    end.

do_filter([], []) ->
    ok;
do_filter([Field|FT], [FilterFun|FFT]) when is_function(FilterFun) ->
    case FilterFun(Field) of
        ok ->
            do_filter(FT, FFT);
        {error, Reason} ->
            {error, {{field, Field}, {reason, Reason}}}
    end;
do_filter([_|FT], [_|FFT]) ->
    do_filter(FT, FFT).

-endif.
