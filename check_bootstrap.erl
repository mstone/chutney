-module(check_bootstrap).
-compile(export_all).

main() ->
  ok = start_servers(2).

start_servers(N) ->
  process_flag(trap_exit, true),
  Endpoints = [["localhost", 8000 + P] || P <- lists:seq(0,N-1)],
  loop([spawn_link(?MODULE, check_status, E) || E <- Endpoints]).

loop([]) -> ok;
loop(Kids) ->
  receive
    {'EXIT', Kid, normal} -> loop(lists:delete(Kid, Kids));
    M -> {error,{bad_msg,M}}
  after 1000 ->
    {error,join_timeout}
  end.

expect(Socket, Pat, Timeout, Msg) ->
  receive
    {tcp, Socket, Data} ->
      case binary:match(Data, Pat) of
        {_, _} -> Data;
        nomatch -> exit({bad_resp,{Data},Msg})
      end;
    {tcp_closed, Socket} ->
      exit({tcp_closed,{},Msg})
  after Timeout ->
    exit({recv_timeout,{},Msg})
  end.

check_status(Addr, Port) ->
  {ok, Socket} = gen_tcp:connect(Addr, Port, [
    binary,
    {packet, line},
    {send_timeout, 1000},
    {active, true}
  ], 1000),
  ok = gen_tcp:send(Socket, "authenticate \"\"\n"),
  expect(Socket, <<"250 OK\r\n">>, 1000, [Addr, Port]),
  ok = gen_tcp:send(Socket, "GETINFO status/bootstrap-phase\n"),
  expect(Socket, <<"TAG=done">>, 1000, [Addr, Port]).
