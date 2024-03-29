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

%% @since 2010-11-18
%% @copyright 2010-2012 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module contains functions these are responsible for parsing rows exctructed 
%% from response packages these came from MySQL server.

-module(string_row_data).

%%
%% Include files
%%
-include("my.hrl").

%%
%% Import modules
%%
-import(string, []).
-import(lists, [reverse/1]).
-import(util).

%%
%% Exported Functions
%%
-export([parse_string_row/2]).

%%
%% API Functions
%%

%% @spec parse_string_row(B::binary(), Metadata::list(#field_metadata{})) -> #rs_row_data{} | #mysql_error{}
%%
%% @doc Converts binary representation of result row to list of field values wrapped in #rs_row_data{}
%%
parse_string_row(B, Metadata) -> 
  case Data = loop_string_row(B, Metadata#metadata.field_metadata, []) of
    #mysql_error{} -> Data;
    _ -> #rs_row_data{data = Data}
  end
.

%%
%% Local Functions
%%

%% @spec loop_string_row(B::binary, Field_metadata::list(#field_metadata{}), Result::list()) -> list() | #mysql_error{} 
%%
%% @doc Converts binary row to list of field values.
%%
loop_string_row(Empty, [], Result) -> 
  if
    size(Empty) =:= 0 -> reverse(Result);
    true ->
      #mysql_error{type = connection, message = "Packet format is wrong",
              source = "loop_string_row(E, [], Result)"}
  end;
loop_string_row(B, [Field_metadata|Metadata], Result) ->
  {Bin_value, B1} = util:extract_length_coded_binary(B),
  Flags = util:parse_data_flags(Field_metadata#field_metadata.flags),
  Value = draw_value(Bin_value, Field_metadata#field_metadata.type, Flags),
  loop_string_row(B1, Metadata, [Value|Result])
.

%% @spec draw_value(B::binary, MySQL_Type::integer(), Flags::list()) -> Value 
%%
%% @doc Converts a binary to a value of MySQL type.
%%
draw_value(null, _, _) -> null;
draw_value(_, ?MYSQL_TYPE_NULL, _) -> null;
draw_value(Bin_value, Type, _Flags) when  Type =:= ?MYSQL_TYPE_TINY orelse Type =:= ?MYSQL_TYPE_SHORT
    orelse Type =:= ?MYSQL_TYPE_LONG orelse Type =:= ?MYSQL_TYPE_LONGLONG orelse Type =:= ?MYSQL_TYPE_INT24 ->
  element(1,string:to_integer(binary_to_list(Bin_value)));
draw_value(Bin_value, Type, _Flags) when Type =:= ?MYSQL_TYPE_FLOAT orelse Type =:= ?MYSQL_TYPE_DOUBLE ->
  element(1,string:to_float(binary_to_list(Bin_value)));
draw_value(Bin_value, Type, _Flags) when Type =:= ?MYSQL_TYPE_TIMESTAMP orelse Type =:= ?MYSQL_TYPE_DATE
    orelse Type =:= ?MYSQL_TYPE_DATETIME ->
  case Bin_value of
    <<YY:2/binary, "-", Rest/binary>> -> true;
    <<YY:4/binary, "-", Rest/binary>> -> true;
    _ -> YY = <<"00">>, Rest = <<"00-00">>
  end,
  <<MM:2/binary, "-", DD:2/binary, TIME/binary>> = Rest,
  <<" ", Hh:2/binary, ":", Mm:2/binary, ":", Ss:2/binary>> = 
    if TIME =:= <<>> -> <<" 00:00:00">>; true -> TIME end,
  #mysql_time{year = element(1, string:to_integer(binary_to_list(YY))), 
             month = element(1, string:to_integer(binary_to_list(MM))), 
             day = element(1, string:to_integer(binary_to_list(DD))), 
             hour = element(1, string:to_integer(binary_to_list(Hh))), 
             minute = element(1, string:to_integer(binary_to_list(Mm))), 
             second = element(1, string:to_integer(binary_to_list(Ss)))
            };
draw_value(Bin_value, ?MYSQL_TYPE_TIME, _Flags) ->
  case Bin_value of
    <<Hh:2/binary, ":", Mm:2/binary, ":", Ss/binary>> -> true;
    <<Hh:3/binary, ":", Mm:2/binary, ":", Ss/binary>> -> true;
    <<Hh:4/binary, ":", Mm:2/binary, ":", Ss/binary>> -> true
  end,
  Hour = element(1, string:to_integer(binary_to_list(Hh))), 
  #mysql_time{neg = (Hour < 0), 
             hour = Hour, 
             minute = element(1, string:to_integer(binary_to_list(Mm))), 
             second = element(1, string:to_integer(binary_to_list(Ss)))
            };
draw_value(Bin_value, ?MYSQL_TYPE_YEAR, _Flags) ->
  element(1,string:to_integer(binary_to_list(Bin_value)));
draw_value(Bin_value, ?MYSQL_TYPE_BIT, _Flags) -> Bin_value;
draw_value(Bin_value, Type, _Flags) when Type =:= ?MYSQL_TYPE_NEWDECIMAL orelse Type =:= ?MYSQL_TYPE_DECIMAL ->
  case string:tokens(binary_to_list(Bin_value), ".") of
    [Int, Fract] -> true;
    [Int] -> Fract = "";
    _ -> Int = "", Fract = ""
  end,  
  #mysql_decimal{int=Int, fraction=Fract};
draw_value(Bin_value, Type, Flags) when Type =:= ?MYSQL_TYPE_BLOB orelse Type =:= ?MYSQL_TYPE_MEDIUM_BLOB
    orelse Type =:= ?MYSQL_TYPE_LONG_BLOB orelse Type =:= ?MYSQL_TYPE_TINY_BLOB ->
  Is_binary = lists:member(binary, Flags),
  if
    Is_binary -> Bin_value;
    true -> binary_to_list(Bin_value)
  end;
draw_value(Bin_value, ?MYSQL_TYPE_STRING, Flags) -> 
  Is_enum = lists:member(enum, Flags),
  Is_set = lists:member(set, Flags),
  Is_binary = lists:member(binary, Flags),
  if
    Is_enum -> list_to_atom(binary_to_list(Bin_value));
    Is_set -> lists:map(fun(A) -> list_to_atom(A) end, string:tokens(binary_to_list(Bin_value), ","));
    Is_binary -> Bin_value;
    true -> binary_to_list(Bin_value)
  end;
draw_value(Bin_value, ?MYSQL_TYPE_VAR_STRING, Flags) ->
  Is_binary = lists:member(binary, Flags),
  if
    Is_binary -> Bin_value;
    true -> binary_to_list(Bin_value)
  end
.
