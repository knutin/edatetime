-module(edatetime).
-export([date2ts/1,
         datetime2ts/1,
         ts2date/1,
         ts2datetime/1,
         now2us/1,
         now2ms/0,
         now2ms/1,
         now2ts/0,
         now2ts/1,
         map_days/3
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



map_days(_F, _End, _End)   -> [];
map_days(F, Start, End) -> [F(Start) | map_days(F, edate:shift(Start, 1, day), End)].
