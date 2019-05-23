-module(matrix_server_supervisor).

-export([matrix_server_start/0]).

matrix_server_start() ->
	
	Server_pid = case whereis(matrix_server) of
        undefined ->
			Pid = spawn_link(matrix_server, server_loop, []),
			try register(matrix_server,Pid) of
				true ->
					Pid
			catch
				error:_ ->
					Pid ! shutdown,
					exit("matrix_server already registered")
			end;
		Pid when is_pid(Pid) ->
			exit("matrix_server already registered")
	end,
	receive
		{'EXIT', Server_pid, normal} -> ok;
		{'EXIT', Server_pid, shutdown} -> ok;
		{'EXIT', Server_pid, _} -> 
			matrix_server_start()
	end.