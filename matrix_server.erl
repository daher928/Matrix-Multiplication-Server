%% @author Daher Daher
%% @doc matrix_server. Server for matrix multiplication 

-module(matrix_server).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_server/0]).
-export([mult/2]).
-export([shutdown/0]).
-export([get_version/0]).

-export([mult2/4]).
-export([cross/5]).
-export([mult_loop/2]).
-export([server_loop/0]).

%% ====================================================================
%% Functions implementation
%% ====================================================================

start_server() ->
	spawn(matrix_server_supervisor,matrix_server_start,[]).

shutdown()-> matrix_server ! shutdown.

get_version()-> 
	MsgRef = make_ref(),
	matrix_server ! {self(), MsgRef, get_version},
	receive
		{MsgRef,Version} -> Version
	end.

mult(Mat1, Mat2)-> 
	MsgRef = make_ref(),
	matrix_server ! {self(), MsgRef, {multiple, Mat1, Mat2}},
	receive
		{MsgRef,Res} -> Res
	end.

%% ====================================================================
%% Auxiliary functions
%% ====================================================================

server_loop() ->
	receive
		{Pid, MsgRef, {multiple, Mat1, Mat2}} -> 
			spawn(?MODULE, mult2, [Pid, MsgRef, Mat1, Mat2]),
			server_loop();
		
 		{Pid, MsgRef, get_version} -> Pid ! {MsgRef, version_1},
									  server_loop();
		sw_upgrade -> 
			?MODULE:server_loop();
		
		shutdown -> exit(shutdown);
		_->
			server_loop()
	end.	

mult2(Pid, MsgRef, Mat1, Mat2)->
	ResRowsNum = tuple_size(matrix:getCol(Mat1,1)),
	ResColsNum = tuple_size(matrix:getRow(Mat2,1)),
	ResSize = ResRowsNum*ResColsNum,
	
	[spawn(matrix_server, cross, [self(), Mat1, Mat2, RowToMult, ColToMult]) || 
		   RowToMult <-lists:seq(1, ResRowsNum) , ColToMult <-lists:seq(1, ResColsNum)],
	
	ResMat = matrix:getZeroMat(ResRowsNum, ResColsNum),
	
	Pid ! {MsgRef,mult_loop(ResSize, ResMat)}.


mult_loop(Countdown, ResMat) ->
	case Countdown of
		0 -> ResMat;
		_ -> receive
				{Row, Col, Res} -> 
					mult_loop(Countdown-1, matrix:setElementMat(Row, Col, ResMat, Res))
			end
	end.

cross(Client, Mat1, Mat2, Row, Col)->
	RowList = tuple_to_list(matrix:getRow(Mat1, Row)),
	ColList = tuple_to_list(matrix:getCol(Mat2, Col)),
	Res = vectorProduct(RowList, ColList),
	Client ! {Row, Col, Res}.

vectorProduct(RowList,ColList) -> vectorProduct(RowList,ColList,0).
vectorProduct([H1|T1],[H2|T2],Res) -> vectorProduct(T1,T2,Res+H1*H2);
vectorProduct([],[],Res) -> Res.
