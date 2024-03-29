%%
%% Copyright (C) 2010-2012 by krasnop@bellsouth.net (Alexei Krasnopolski)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License. 
%%

%% @since 2010-12-21
%% @copyright 2010-2012 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module incapsulates functions these are responsible for sending binary packages to MySQL server.
%% The functions return result packages from the server.
%%
-module(send_cmd).

%%
%% Include files
%%
-include("my.hrl").

%%
%% Import modules
%%
-import(response).
-import(my_io).

%%
%% Exported Functions
%%
-export([send_cmd_packet/2, send_cmd_packet/3, send_cmd_packet/4, send_cmd_packet_no_resp/2]).

%%
%% API Functions
%%

%% @spec send_cmd_packet(Connection::#connection{}, Command::binary()) -> {Metadata::#metadata{}, Result} | #mysql_error{}
%% Result = [] | list( #ok_packet{} | #ok_stmt_packet{} | #rs_row_data{} )
%%
%% @doc Sends binary Command to server throw Connection's Socket.
%% @see send_cmd_packet/4
send_cmd_packet(Connection, Command) -> send_packet(Connection, Command, #metadata{}, 0, none).

%% @spec send_cmd_packet(Connection::#connection{}, Command::binary(), Control) -> {Metadata::#metadata{}, Result} | #mysql_error{}
%% Control = {N, Statement}
%% Statement = prepare | fetch | none
%% Result = [] | list( #ok_packet{} | #ok_stmt_packet{} | #rs_row_data{} )
%%
%% @doc Sends binary Command to server throw Connection's Socket.
%% @see send_cmd_packet/4
send_cmd_packet(Connection, Command, {N, Stmt}) -> send_packet(Connection, Command, #metadata{}, N, Stmt);
send_cmd_packet(Connection, Command, N) -> send_packet(Connection, Command, #metadata{}, N, none).

send_cmd_packet(Connection, Command, Initial_metadata, {N, Stmt}) -> send_packet(Connection, Command, Initial_metadata, N, Stmt).


%% @spec send_cmd_packet_no_resp(Connection::#connection{}, Command::binary()) -> ok | #mysql_error{} 
%%
%% @doc Sends binary Command to server throw Connection's Socket and is not waiting any response.
%% @see send_cmd_packet_no_resp/3
send_cmd_packet_no_resp(Connection, Command) -> send_packet_no_resp(Connection, Command, 0).

%%
%% Local Functions
%%

%% @spec send_packet(Connection::#connection{}, Command::binary(), Initial_metadata::#metadata{}, N::integer(), Stmt::atom()) -> {Metadata::#metadata{}, Result} | #mysql_error{}
%% Result = [] | list(#ok_packet{} | #ok_stmt_packet{} | #rs_row_data{})
%%
%% @doc Sends binary Command to server through Connection's Socket.
%% 
send_packet(Connection, Command, Initial_metadata, N, Stmt) ->
  case Sent = send_packet_no_resp(Connection, Command, N) of
    #mysql_error{} -> Sent;
    ok ->
      case Return = response:handle_response(Connection, Initial_metadata, Stmt) of
        {_,[#error_packet{} = P | _]} -> 
          #mysql_error{type = sqlquery, source = "send_cmd:send_cmd_packet/5", errno = P#error_packet.errno, 
                       sqlstate = P#error_packet.sqlstate, message = P#error_packet.message};
        _ -> Return
      end
  end
.

%% @spec send_packet_no_resp(Connection::#connection{}, Command::binary(), N::integer()) -> ok | #mysql_error{}
%%
%% @doc Sends binary Command to server through Connection's Socket and is not waiting any response.
%% 
send_packet_no_resp(Connection, Command, N) ->
  Packet_size = size(Command),
  my_io:send_single_packet(Connection#connection.socket, Command, Packet_size, N, 
      (Packet_size >= ?MAX_PACKET_SIZE), (Connection#connection.client_info#client_options.compress =:= 1))
.

