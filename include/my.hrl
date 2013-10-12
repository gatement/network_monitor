%%
%% Copyright (C) 2011 by krasnop@bellsouth.net (Alexei Krasnopolski)
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

-define(ACTIVE, true).
-define(BUFFER_SIZE, 16#4000).
-define(MAX_PACKET_SIZE, 16#FFFFFF).
-define(RECV_TIMEOUT, 60000).
-define(SEND_TIMEOUT, 60000).
-define(CONN_TIMEOUT, 60000).
-define(GEN_SERVER_TIMEOUT, 300000).
%%
%% MySQL data types:
%%

-define(MYSQL_TYPE_DECIMAL, 0). 
-define(DECIMAL, ?MYSQL_TYPE_DECIMAL). 
-define(MYSQL_TYPE_TINY, 1).
-define(TINY, ?MYSQL_TYPE_TINY).
-define(MYSQL_TYPE_SHORT, 2).
-define(SHORT, ?MYSQL_TYPE_SHORT).
-define(MYSQL_TYPE_LONG, 3).
-define(LONG, ?MYSQL_TYPE_LONG).
-define(MYSQL_TYPE_FLOAT, 4).
-define(FLOAT, ?MYSQL_TYPE_FLOAT).
-define(MYSQL_TYPE_DOUBLE, 5).
-define(DOUBLE, ?MYSQL_TYPE_DOUBLE).
-define(MYSQL_TYPE_NULL, 6).
-define(NULL, ?MYSQL_TYPE_NULL).
-define(MYSQL_TYPE_TIMESTAMP, 7).
-define(TIMESTAMP, ?MYSQL_TYPE_TIMESTAMP).
-define(MYSQL_TYPE_LONGLONG, 8).
-define(LONGLONG, ?MYSQL_TYPE_LONGLONG).
-define(MYSQL_TYPE_INT24, 9).
-define(INT24, ?MYSQL_TYPE_INT24).
-define(MYSQL_TYPE_DATE, 10).
-define(DATE, ?MYSQL_TYPE_DATE).
-define(MYSQL_TYPE_TIME, 11).
-define(TIME, ?MYSQL_TYPE_TIME).
-define(MYSQL_TYPE_DATETIME, 12). 
-define(DATETIME, ?MYSQL_TYPE_DATETIME). 
-define(MYSQL_TYPE_YEAR, 13).
-define(YEAR, ?MYSQL_TYPE_YEAR).
-define(MYSQL_TYPE_NEWDATE, 14).
-define(NEWDATE, ?MYSQL_TYPE_NEWDATE).
-define(MYSQL_TYPE_VARCHAR, 15).
-define(VARCHAR, ?MYSQL_TYPE_VARCHAR).
-define(MYSQL_TYPE_BIT, 16).
-define(BIT, ?MYSQL_TYPE_BIT).
-define(MYSQL_TYPE_NEWDECIMAL, 246).
-define(NEWDECIMAL, ?MYSQL_TYPE_NEWDECIMAL).
-define(MYSQL_TYPE_ENUM, 247).
-define(ENUM, ?MYSQL_TYPE_ENUM).
-define(MYSQL_TYPE_SET, 248).
-define(SET, ?MYSQL_TYPE_SET).
-define(MYSQL_TYPE_TINY_BLOB, 249).
-define(TINY_BLOB, ?MYSQL_TYPE_TINY_BLOB).
-define(MYSQL_TYPE_MEDIUM_BLOB, 250).
-define(MEDIUM_BLOB, ?MYSQL_TYPE_MEDIUM_BLOB).
-define(MYSQL_TYPE_LONG_BLOB, 251).
-define(LONG_BLOB, ?MYSQL_TYPE_LONG_BLOB).
-define(MYSQL_TYPE_BLOB, 252).
-define(BLOB, ?MYSQL_TYPE_BLOB).
-define(MYSQL_TYPE_VAR_STRING, 253).
-define(VAR_STRING, ?MYSQL_TYPE_VAR_STRING).
-define(MYSQL_TYPE_STRING, 254).
-define(STRING, ?MYSQL_TYPE_STRING).
-define(MYSQL_TYPE_GEOMETRY, 255).
-define(GEOMETRY, ?MYSQL_TYPE_GEOMETRY). %% total 26 types

-define(MYSQL_TYPE_UNSIGNED, 32768).
-define(UNSIGNED, ?MYSQL_TYPE_UNSIGNED).

%%
%% MySQL data flags:
%%

-define(NOT_NULL_FLAG, 1).							%% Field can't be NULL 
-define(PRI_KEY_FLAG, 2).								%% Field is part of a primary key 
-define(UNIQUE_KEY_FLAG, 4).						%% Field is part of a unique key 
-define(MULTIPLE_KEY_FLAG, 8).					%% Field is part of a key 
-define(BLOB_FLAG, 16).									%% Field is a blob 
-define(UNSIGNED_FLAG, 32).							%% Field is unsigned 
-define(ZEROFILL_FLAG, 64).							%% Field is zerofill 
-define(BINARY_FLAG, 128).							%% Field is binary   
-define(ENUM_FLAG, 256).								%% field is an enum 
-define(AUTO_INCREMENT_FLAG, 512).			%% field is a autoincrement field 
-define(TIMESTAMP_FLAG, 1024).					%% Field is a timestamp 
-define(SET_FLAG, 2048).								%% field is a set 
-define(NO_DEFAULT_VALUE_FLAG, 4096).		%% Field doesn't have default value 
-define(ON_UPDATE_NOW_FLAG, 8192).			%% Field is set to NOW on UPDATE 
-define(NUM_FLAG, 32768).								%% Field is num (for clients) 

%%
%% MySQL prepared statement flags (cursor types)
%%

-define(CURSOR_TYPE_NO_CURSOR, 0).
-define(CURSOR_TYPE_READ_ONLY, 1).
-define(CURSOR_TYPE_FOR_UPDATE, 2).
-define(CURSOR_TYPE_SCROLLABLE, 4).

%% @type server_status() = #server_status{
%% 		inTransaction = boolean(), 
%% 		autocommit = boolean(),
%% 		moreResultExists = boolean(),
%% 		queryNoGoodIndexUsed = boolean(), 
%% 		queryNoIndexUsed = boolean(), 
%% 		cursorExists = boolean(),
%% 		lastRowSent	= boolean(),
%% 		dbDropped = boolean(),
%% 		noBackSlashEscapes = boolean(), 
%% 		metadataChanged = boolean(),
%% 		queryWasSlow = boolean(),
%% 		psOutParams = boolean()
%% }. The server_status() record contains information about status of SQL server after query
-record(server_status, 
	{
		inTransaction = false::boolean(),						% Transaction has started 
		autocommit = false::boolean(), 							% Server in auto_commit mode
		moreResultExists = false::boolean(),				% Multi query - next query exists
		queryNoGoodIndexUsed = false::boolean(), 
		queryNoIndexUsed = false::boolean(), 
		cursorExists = false::boolean(),						% read-only non-scrollable cursor for a query was opened
		lastRowSent	= false::boolean(),							% read-only cursor is exhausted
		dbDropped = false::boolean(), 							% A database was dropped
		noBackSlashEscapes = true::boolean(), 
		metadataChanged = false::boolean(), 				%
		queryWasSlow = false::boolean(), 						% this query was logged to the slow query log
		psOutParams = false::boolean()							% To mark ResultSet containing output parameter values
	}
).

%% @type client_options() = #client_options{
%% 		charset_number = integer(),
%% 		long_password =	0|1,
%% 		found_rows	= 0|1,
%% 		long_flag = 0|1,
%% 		connect_with_db = 0|1,
%% 		no_schema = 0|1,
%% 		compress = 0|1,
%% 		odbc = 0|1,
%% 		local_files = 0|1,
%% 		ignore_space = 0|1,
%% 		protocol_41 = 0|1,
%% 		interactive = 0|1,
%% 		ssl = 0|1,
%% 		ignore_sigpipe = 0|1,
%% 		transactions	= 0|1,
%% 		reserved = 0|1,
%% 		secure_connection = 0|1, 
%% 		multi_statements = 0|1,
%% 		multi_results = 0|1
%%}. The record keeps client options that server is capable to support for this client and this connection
-record(client_options, 
	{
		charset_number = 33::0..255,			% Character set number: default utf-8 (code= 33)
		long_password =	1::0|1,						% new more secure passwords
		found_rows	= 0::0|1,							% Found instead of affected rows
		long_flag = 1::0|1,								% Get all column flags
		connect_with_db = 1::0|1,					% One can specify db on connect
		no_schema = 0::0|1,								% Don't allow database.table.column
		compress = 0::0|1,								% Can use compression protocol
		odbc = 0::0|1,										% Odbc client
		local_files = 0::0|1,							% Can use LOAD DATA LOCAL

		ignore_space = 0::0|1,						% Ignore spaces before '('
		protocol_41 = 1::0|1,							% New 4.1 protocol
		interactive = 1::0|1,							% This is an interactive client
		ssl = 0::0|1,											% Switch to SSL after handshake
		ignore_sigpipe = 0::0|1,					% IGNORE sigpipes
		transactions	= 1::0|1,						% Client knows about transactions
		reserved = 0::0|1,								% Old flag for 4.1 protocol
		secure_connection = 1::0|1,				% New 4.1 authentication 

		multi_statements = 1::0|1,				% Enable/disable multi-stmt support
		multi_results = 1::0|1,						% Enable/disable multi-results
		trans_isolation_level =						% transaction isolation level
			default													% default isolation level
			::
			read_uncommitted |							% given session can read uncommitted updates of other session
			read_committed |								% given session can read only committed updates of other session
			repeatable_read |								% given session can read only snapshot of committed updates of other session
			serializable										% the same as repeatable read but given session blocks own reads 
	}
).

%% @type server_info() = #server_info{
%% 		protocol_version = integer(), 
%% 		server_version = string(), 
%% 		thread_Id = integer(), 
%% 		server_capabilities = client_options(), 
%% 		server_status = server_status(), 
%% 		scramble_buff = binary()
%% 	}. The server_info() contains information that SQL server sent during handshake about itself. 
-record(server_info,
	{
		protocol_version::integer(), 
		server_version::string(), 
		thread_Id::integer(), 
		server_capabilities::#client_options{}, 
		server_status::#server_status{}, 
		scramble_buff::binary()
	}
).

-record(connection,
	{
		socket::port(),
		pool_name::atom(), 
		host::string, 
		port::integer(), 
		user::string(), 
		password::string(), 
		dbname::string(), 
		server_info::#server_info{}, 
		client_info::#client_options{}
	}
).

-record(packet, 
	{
		continue = false::boolean(), 
		seq_number = 0::integer(), 
		uncompressed_size = 0::integer(), 
		body = <<>>::binary()
	}
).

-record(ok_packet, 
	{
		affected_rows::integer(), 
		insert_id::integer(), 
		server_status::#server_status{}, 
		warning_count::integer(), 
		message::string()
	}
).

-record(ok_stmt_packet, 
	{
		stmt_handle::integer(), 
		columns_number::integer(), 
		params_number::integer(), 
		warn_count::integer()
	}
).

-record(error_packet, 
	{
		errno::integer(), 
		sqlstate::string(), 
		message::string()
	}
).

-record(eof_packet, 
	{
		warning_count::integer(), 
		server_status::#server_status{}
	}
).

-record(rs_header, 
	{
		field_count::integer(), 
		extra::any() %% TODO is it any, not integer ?
	}
).

-record(field_metadata, 
	{
		catalog::string(), 
		schema::string(), 
		table::string(), 
		origtable::string(), 
		name::string(), 
		origname::string(), 
		charsetnr::integer(), 
		length::integer(), 
		type::0..255, 
		flags::bitstring(), 
		scale::0..255, 
		default::binary()
	}
).

-record(metadata, 
	{
		field_count=0::integer(), 
		param_count=0::integer(),
		server_status::#server_status{}, 
		field_metadata = []::list(#field_metadata{}),
		param_metadata = []::list(#field_metadata{})
	}
).

-record(mysql_time, 
	{
		neg = false::boolean(), 
	 	year = 0::integer(), 
		month = 0::0..12, 
		day = 0::0..31, 
		hour = 0::0..23, 
		minute = 0::0..59, 
		second = 0::0..59, 
		second_part = 0
	}
).

-record(mysql_decimal, 
	{
		int::string(), 
		fraction::string()
	}
).

-type(field_value() :: integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{}).

-record(rs_row_data, 
	{
		data = []::list(field_value())
	}
).

%% Record represents an exception that is thrown by a client's module. 
-record(mysql_error, 
	{
		type:: tcp | connection | sqlquery | statement | transaction, 
		errno = none:: none | integer(),
		sqlstate = []::string(),
		source = []::string(), 
		message = []::string()
	}
).

-record(datasource,
	{
		name :: atom(),
		host = "localhost" :: string(),
		port = 3306 :: integer(),
		database = "" :: string(),
		user :: string(),
		password :: string(),
		flags = #client_options{} :: #client_options{}
	}
).