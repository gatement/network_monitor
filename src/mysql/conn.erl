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

%% @since 2010-10-08
%% @copyright 2010-2012 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module incapsulates functions these are responsible for connection to MySQL server. 
%% Function open_connection establishes socket connection to server and proceedes handshake with authorization.
%% Function change_user keeps connection opened but it changes authorization.

-module(conn).

%%
%% Include files
%%
-include("my.hrl").

%%
%% Import modules
%%
-import(crypto).
-import(my_io, [receive_data/2]).
-import(send_cmd, [send_cmd_packet/3]).

%%
%% Exported Functions
%%
-export([
  open_connection/6, 
  change_user/4
]).

%%
%% API Functions
%%
-spec open_connection(Host::string(), Port::0..65535, Db::string(), User::string(), Password::string(), Client_flags::#client_options{}) -> 
        #connection{} | #mysql_error{}. 
-spec change_user(Connection::#connection{}, User::string(), Password::string(), Db::string()) -> #connection{} | #mysql_error{}.

%% @spec open_connection(Host::string(), Port::integer(), Db::string(), User::string(), Password::string(), Client_flags::#client_options{}) -> #connection{} | #mysql_error{} 
%% @doc 
%% Open ip/tcp socket connection to remote MySQL server. The function returns a record 
%% that contains all information about the connection or error message if request is failed.
open_connection(Host, Port, Db, User, Password, Client_flags) -> 
  case gen_tcp:connect(Host, Port, 
        [binary, 
         {active, ?ACTIVE}, 
         {packet, 0}, 
         {recbuf, ?BUFFER_SIZE}, 
         {sndbuf, ?BUFFER_SIZE}, 
         {send_timeout, ?SEND_TIMEOUT}
        ], ?CONN_TIMEOUT) 
  of
    {ok, Socket} -> 
      case Server_info_record = parse_handshake_init_packet(receive_data(Socket, false)) of
        #mysql_error{} -> Server_info_record;
        _ ->
          Token = get_token(Password, Server_info_record#server_info.scramble_buff),
          Command = <<(util:client_flags_record_to_binary(Client_flags))/binary, (?MAX_PACKET_SIZE):32/little-integer, 
            (Client_flags#client_options.charset_number):8/integer, 0:184, 
            (list_to_binary(User))/binary, 0:8, (size(Token)):8/integer, 
            Token/binary, (list_to_binary(Db))/binary, 0:8>>,
          Connection = #connection{socket = Socket, host = Host, port = Port, user = User, password = Password, 
            dbname = Db, server_info = Server_info_record, client_info = Client_flags#client_options{compress = 0}},
          case send_cmd_packet(Connection, Command, 1) of
            #mysql_error{} = Err -> Err#mysql_error{type = connection, source="open_connection/6 [1]"};
            {_, [P|_]} ->
              case P of
                #ok_packet{} -> Connection#connection{client_info = Client_flags};
                #error_packet{} -> #mysql_error{type = connection, source="open_connection/6 [3]", 
                                                errno = P#error_packet.errno, sqlstate = P#error_packet.sqlstate, 
                                                message = P#error_packet.message};
                _ -> #mysql_error{type = connection, source="open_connection/6 [4]", message = "Unknown error."}
              end;
            _ -> #mysql_error{type = connection, source="open_connection/6 [2]"} 
          end
      end;
    {error, Reason} -> #mysql_error{type = tcp, source="open_connection/6 [0]", message = Reason}
  end
.  

%% @spec change_user(Connection::#connection{}, User::string(), Password::string(), Db::string) -> #connection{} | #mysql_error{}
%% @doc Changes user for the connection. Returns new Connection record or error record.
change_user(Connection, User, Password, Db) ->
  Token = get_token(Password, Connection#connection.server_info#server_info.scramble_buff),
  Command = <<17:8, (list_to_binary(User))/binary, 0:8, (size(Token)):8/integer, Token/binary, 
         (list_to_binary(Db))/binary, 0:8, (Connection#connection.client_info#client_options.charset_number):16/little-integer>>,
  {_, [P|_]} = send_cmd_packet(Connection, Command, 0),
  case P of
    #ok_packet{} -> 
      Connection#connection{user = User, password = Password, dbname = Db};
    #error_packet{} -> #mysql_error{type = connection, source="change_user/4", errno = P#error_packet.errno, sqlstate = P#error_packet.sqlstate, message = P#error_packet.message};
    _ -> #mysql_error{type = connection, source="change_user/4", message = "Unknown error."}
  end
.

%%
%% Local Functions
%%
-spec parse_handshake_init_packet(Packet::#packet{}) -> #server_info{}.
-spec null_terminated_string_length(Binary::binary()) -> integer().
-spec get_token(Password::string(), ServerInfoRec::#server_info{}) -> binary().
-spec binary_xor(A::binary(), B::binary()) -> binary().

%% @spec parse_handshake_init_packet(Packet::#packet{}) -> #server_info{} | #mysql_error{}
%% @doc Parses binary body of a handshake packet and return server_info record.
%%
parse_handshake_init_packet(#mysql_error{} = B) -> B;
parse_handshake_init_packet([#packet{body = B}]) ->
  <<Protocol_version:8/integer, P1/binary>> = B,    
  SL = null_terminated_string_length(P1),
  <<Server_version:SL/binary, 0:8, Thread_Id:32/little-integer, Scramble_buff1:8/bytes, 0:8, 
    Server_capabilities:2/binary, ServerLang:8/integer, Server_status:2/binary, 
    _:13/bytes, Scramble_buff2:12/bytes, 0:8, _Tail/binary>> = P1,
  Scramble_buff = <<Scramble_buff1/binary, Scramble_buff2/binary>>,
  #server_info{
   protocol_version = Protocol_version,
   server_version = binary_to_list(Server_version),
   thread_Id = Thread_Id,
   server_capabilities = util:binary_to_client_flags_record(ServerLang, Server_capabilities),
   server_status = util:server_status_to_record(Server_status),
   scramble_buff = Scramble_buff
  }
.

%% @spec get_token(Password::string(), Scramble_buff::binary()) -> binary()
%% @doc Generates security token for authorization from password and scramble buffer sent by server.
%
get_token(Password, Scramble_buff) ->
  CPswd = crypto:sha(list_to_binary(Password)),
  CCPswd = crypto:sha(CPswd),
  binary_xor(crypto:sha(<<Scramble_buff/binary, CCPswd/binary>>), CPswd)
.

%% @spec null_terminated_string_length(Binary::binary()) -> integer()
%% @doc Calculates length of null terminated string at beginning of the Binary.
%
null_terminated_string_length(<<0:8/integer, _/binary>>) -> 0;
null_terminated_string_length(<<_:8/integer, Binary/binary>>) -> 1 + null_terminated_string_length(Binary).

%% @spec binary_xor(A::binary(), B::binary()) -> binary()
%% @doc Makes bitwise xor operation (A xor B) on binary parameters. Returns binary. 
%
binary_xor(A, B) ->
  Size = bit_size(A),
  <<X:Size>> = A,
  <<Y:Size>> = B,
  <<(X bxor Y):Size>>
.
