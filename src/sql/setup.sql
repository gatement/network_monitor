-- settings -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `settings` (
	`id` INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
	`key` VARCHAR(100),
	`value` VARCHAR(1000)
);

INSERT INTO `settings`(`key`,`value`) values('ping_client', 'telecom(192.168.1.2)');
INSERT INTO `settings`(`key`,`value`) values('ping_client_os_type', 'windows'); -- windows/unix
INSERT INTO `settings`(`key`,`value`) values('ping_interval_minute', '30');
INSERT INTO `settings`(`key`,`value`) values('ping_size_byte', '32');
INSERT INTO `settings`(`key`,`value`) values('ping_count_each', '20');
INSERT INTO `settings`(`key`,`value`) values('warning_threshold_loss_percentage', '6');
INSERT INTO `settings`(`key`,`value`) values('warning_threshold_average_time', '400');
INSERT INTO `settings`(`key`,`value`) values('email_account', 'jliu@xtremeprog.com');
INSERT INTO `settings`(`key`,`value`) values('email_password', '123456');


-- targets -------------------------------------------------------
CREATE TABLE IF NOT EXISTS `targets` (
	`id` INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
	`name` VARCHAR(100),
	`host` VARCHAR(100),
	`enabled` BIT(1)
);


-- subscribers ---------------------------------------------------
CREATE TABLE IF NOT EXISTS `subscribers` (
	`id` INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
	`name` VARCHAR(100),
	`email` VARCHAR(100),
	`warning_only` BOOLEAN,
	`enabled` BOOLEAN
);


-- log -----------------------------------------------------------
CREATE TABLE IF NOT EXISTS `log` (
	`id` BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
	`iteration` BIGINT UNSIGNED,
	`datetime` DATETIME,
	`from` VARCHAR(100),
	`target` VARCHAR(100),
	`bytes` INT,
	`count` INT,
	`loss` INT,
	`time_ave` INT
);

