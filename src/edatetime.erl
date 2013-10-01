%% @doc: datetime stuff
-module(edatetime).
-include_lib("eunit/include/eunit.hrl").

-export([date2ts/1,
         datetime2ts/1,
         ts2date/1,
         ts2datetime/1,
         now2us/1,
         now2ms/0,
         now2ms/1,
         now2ts/0,
         now2ts/1,
         map/4,
         foldl/5,
         shift/3,
         day_start/1, week_start/1, month_start/1
        ]).

date2ts({Y, M, D}) ->
    calendar:datetime_to_gregorian_seconds({{Y, M, D}, {0, 0, 0}})
        - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0,0,0}}).

datetime2ts(Datetime) ->
    calendar:datetime_to_gregorian_seconds(Datetime)
        - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0,0,0}}).

ts2date(Timestamp) ->
    {Date, _Time} = ts2datetime(Timestamp),
    Date.

ts2datetime(Timestamp) ->
    BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds = BaseDate + Timestamp,
    calendar:gregorian_seconds_to_datetime(Seconds).


now2us({MegaSecs,Secs,MicroSecs}) ->
        (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

now2ms() ->
    now2ms(os:timestamp()).

now2ms({MegaSecs,Secs,MicroSecs}) ->
        (MegaSecs * 1000000 + Secs) * 1000000 + (MicroSecs div 1000).

now2ts() ->
    now2ts(os:timestamp()).

now2ts({MegaSeconds, Seconds, _}) ->
    MegaSeconds * 1000000 + Seconds.


map(F, Start, End, days) ->
    do_map(F, day_start(Start), End, days, []).

do_map(F, End, End, _, Acc) ->
    lists:reverse([F(End) | Acc]);
do_map(F, Start, End, days, Acc) ->
    do_map(F, shift(Start, 1, days), End, days, [F(Start) | Acc]).



foldl(F, Acc0, Start, End, days) ->
    do_foldl(F, day_start(Start), End, days, Acc0).

do_foldl(F, End, End, _, Acc) ->
    F(End, Acc);
do_foldl(F, Start, End, days, Acc) ->
    do_foldl(F, shift(Start, 1, days), End, days, F(Start, Acc)).



day_start(Ts) when is_integer(Ts) ->
    Ts - (Ts rem 86400).

shift(Ts, N, days) ->
    Ts + (N * 86400).

week_start(Ts) when is_integer(Ts) ->
    WeekDay = calendar:day_of_the_week(ts2date(Ts)),
    day_start(shift(Ts, -WeekDay+1, days)).

month_start(Ts) when is_integer(Ts) ->
    {Y, M, _} = ts2date(Ts),
    date2ts({Y, M, 1}).

%%
%% TESTS
%%

day_start_test() ->
    ?assertEqual(datetime2ts({{2013, 1, 1}, {0, 0, 0}}),
                 day_start(datetime2ts({{2013, 1, 1}, {0, 0, 0}}))),
    ?assertEqual(datetime2ts({{2013, 1, 1}, {0, 0, 0}}),
                 day_start(datetime2ts({{2013, 1, 1}, {0, 10, 0}}))),

    ?assertEqual(datetime2ts({{2013, 1, 1}, {0, 0, 0}}),
                 day_start(datetime2ts({{2013, 1, 1}, {23, 59, 59}}))).

week_start_test() ->
    ?assertEqual({2013, 1, 7}, ts2date(week_start(date2ts({2013, 1, 7})))),
    ?assertEqual({2013, 1, 7}, ts2date(week_start(date2ts({2013, 1, 8})))),
    ?assertEqual({2012, 12, 31}, ts2date(week_start(date2ts({2013, 1, 1})))),
    ?assertEqual({2012, 12, 31}, ts2date(week_start(date2ts({2013, 1, 2})))).

month_start_test() ->
    ?assertEqual({2013, 1, 1}, ts2date(month_start(date2ts({2013, 1, 1})))),
    ?assertEqual({2013, 1, 1}, ts2date(month_start(date2ts({2013, 1, 2})))),
    ?assertEqual({2013, 1, 1}, ts2date(month_start(date2ts({2013, 1, 31})))),
    ?assertEqual({2013, 2, 1}, ts2date(month_start(date2ts({2013, 2, 28})))).

map_days_test() ->
    ?assertEqual([{2012, 12, 31},
                  {2013, 1, 1},
                  {2013, 1, 2}],
                 map(fun ts2date/1,
                     date2ts({2012, 12, 31}),
                     date2ts({2013, 1, 2}),
                     days)).

foldl_sum_days_test() ->
    ?assertEqual(34,
                 foldl(fun (Ts, Sum) ->
                               {_, _, Day} = ts2date(Ts),
                               Day + Sum
                       end,
                       0,
                       date2ts({2012, 12, 31}),
                       date2ts({2013, 1, 2}),
                       days)).

foldl_count_days_test() ->
    ?assertEqual(367,
                 foldl(fun (_, Count) -> Count + 1 end,
                       0,
                       date2ts({2012, 1, 1}),
                       date2ts({2013, 1, 1}),
                       days)).
