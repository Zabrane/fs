-module(inotifywait).
-include("api.hrl").
-export([find_executable/0, start_port/2, known_events/0, line_to_event/1]).

find_executable() -> os:find_executable("inotifywait").
known_events() -> [created, deleted, renamed, closed, modified, isdir, attribute, undefined].

%% compiled regular expression
%% > rp(re:compile("^(.+) ([A-Z_,]+) (.+)$", [])).
-define(RE_EVENT, {re_pattern,3,0,0,
                <<69,82,67,80,139,0,0,0,16,0,0,0,65,0,0,0,255,255,255,
                  255,255,255,255,255,0,0,32,0,0,0,3,0,0,0,64,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,131,0,
                  71,27,133,0,7,0,1,87,12,120,0,7,29,32,133,0,39,0,2,
                  110,0,0,0,0,0,16,0,0,254,255,255,135,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,107,120,0,39,29,32,133,0,7,0,
                  3,87,12,120,0,7,25,120,0,71,0>>}).

join([EventName|R]) ->
    join(R, ["-m", "-e",EventName]).

join([EventName|R], Acc) ->
    join(R, ["-e", EventName | Acc]);
join([], Acc) ->
    Acc.

event_names() ->
    join(application:get_env(fs, event_names, [ "modify", "close_write", "moved_to", "moved_from", "create", "delete", "attrib" ])).

start_port(Path, Cwd) ->
    Path1 = filename:absname(Path),
    Args = ["-c", "inotifywait \"$0\" \"$@\" & PID=$!; read a; kill $PID" ] ++
            event_names() ++
            [ "--quiet", Path1],
    erlang:open_port({spawn_executable, os:find_executable("sh")},
        [stream, exit_status, binary, use_stdio, {args, Args}, {cd, Cwd}]).

line_to_event(<<Data/binary>>) ->
    Lines = lists:usort(binary:split(binary:replace(Data, <<"\r\n">>, <<"\n">>, [global]), <<"\n">>, [global])),
    Fun = fun(<<>>, Acc) -> Acc;
             (Line, Acc) -> case re:run(Line, ?RE_EVENT, [{capture, all_but_first, binary}]) of
                                {match, [Dir, Flags, Entry]} -> [{<<Dir/binary, Entry/binary>>, flags(Flags)} | Acc];
                                nomatch -> Acc
                            end
        end,
    lists:foldl(Fun, [], Lines).

flags(Flags) ->
   [ convert_flag(Flag) || Flag <- binary:split(Flags, <<",">>, [global]) ].

convert_flag(<<"CREATE">>)      -> created;
convert_flag(<<"DELETE">>)      -> deleted;
convert_flag(<<"ISDIR">>)       -> isdir;
convert_flag(<<"MODIFY">>)      -> modified;
convert_flag(<<"CLOSE_WRITE">>) -> modified;
convert_flag(<<"CLOSE">>)       -> closed;
convert_flag(<<"MOVED_TO">>)    -> renamed;
convert_flag(<<"MOVED_FROM">>)  -> removed;
convert_flag(<<"ATTRIB">>)      -> attribute;
convert_flag(_)                 -> undefined.
