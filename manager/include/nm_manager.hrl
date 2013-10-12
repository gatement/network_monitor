-define(SESSION_COOKIE_ID, "nm_sid").

-define(SETTING_NOTIFICATION_EMAIL, "notification_email").
-define(SETTING_NOTIFICATION_EMAIL_PASSWORD, "notification_email_password").
-define(SETTING_NOTIFICATION_WARNING_THRESHOLD_PING_LOSS_PERCENTAGE, "notification_warning_threshold_ping_loss_percentage").
-define(SETTING_NOTIFICATION_WARNING_THRESHOLD_PING_AVERAGE_TIME, "notification_warning_threshold_ping_average_time").
-define(SETTING_NOTIFICATION_WARNING_WEEK_DAYS, "notification_warning_week_days").
-define(SETTING_NOTIFICATION_WARNING_HOURS, "notification_warning_hours").


-record(config, {
					rrdtool_exe_path,
					rrdtool_db_dir,
					rrdtool_img_abs_dir,
					rrdtool_img_rel_dir
				}
		).

-record(arg_state, {session_id, user_id}).

-record(nm_user, {id, password, role, status}).
-record(nm_session, {id, user_id, socket_pid, socket_territory, last_active}).

-record(nm_setting, {key, value}).

-record(nm_agent, {id, user_id, target_name, enabled}).
-record(nm_agent_mib, {id, agent_id, oid, name, type, color, enabled}).

-record(nm_notification_subscriber, {id, name, email, warning_only, enabled}).