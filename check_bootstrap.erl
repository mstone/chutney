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

check_status(Addr, Port) ->
  {ok, Socket} = gen_tcp:connect(Addr, Port, [
    binary,
    {packet, line},
    {send_timeout, 1000},
    {active, true}
  ], 1000),
  ok = gen_tcp:send(Socket, "authenticate \"\"\n"),
  receive
    {tcp, Socket, _Data = <<"250 OK\r\n">>} -> exit(normal);
    {tcp, Socket, Data} -> exit({bad_resp,{Data},[Addr,Port]})
  after 1000 ->
    exit({recv_timeout,{},[Addr,Port]})
  end.
