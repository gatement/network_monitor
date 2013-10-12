
-define(MYSQL_DATASOURCE_ID, mysql_datasource).

-record(config, {
					mysql_host, 
					mysql_port, 
					mysql_user, 
					mysql_user_password, 
					mysql_database
				}
		).


