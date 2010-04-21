-- Table layout. All tables are in ARCHIVE format to save A LOT of
-- space. Therefore no indexes are present.

-- Phase 1 source data. Names of log files.
CREATE TABLE `phase_1_data` (
  `file` text
) ENGINE=ARCHIVE;

-- Phase 2 source data. Server and service lengths are tailored for my
-- data to save some space, feel free to tailor them for your
-- needs, too.
CREATE TABLE `phase_2_data` (
  `server` varchar(13) default NULL,
  `service` varchar(46) default NULL,
  `file` text
) ENGINE=ARCHIVE;

-- Phase 3 source data. File contents are read to this table. Site_id
-- is a reference to site.id.
CREATE TABLE `phase_3_data` (
  `site_id` smallint(6) DEFAULT NULL,
  `line` text
) ENGINE=ARCHIVE;

-- Phase 4 source data. Log lines are parsed into many columns. Like
-- before, site_id is a reference to site.id. Date column is in UTC.
CREATE TABLE `phase_4_data` (
  `site_id` smallint(6) DEFAULT NULL,
  `ip` varchar(15) DEFAULT NULL,
  `date` datetime DEFAULT NULL,
  `request` text,
  `response` smallint(6) DEFAULT NULL,
  `bytes` int(11) DEFAULT NULL,
  `referer` text,
  `browser` text
) ENGINE=ARCHIVE;

-- 
-- Tables for reporting errors in process. All tables contain the
-- exception text (error) and the data which was processed.
-- 

-- Phase 1 processing errors.
CREATE TABLE `phase_1_error` (
  `file` text,
  `error` text
) ENGINE=ARCHIVE;

-- Phase 2 processing errors.
CREATE TABLE `phase_2_error` (
  `site_id` smallint(6) DEFAULT NULL,
  `file` text,
  `error` text
) ENGINE=ARCHIVE;

-- Phase 3 processing errors.
CREATE TABLE `phase_3_error` (
  `error` text,
  `site_id` smallint(6) DEFAULT NULL,
  `line` text
) ENGINE=ARCHIVE;

-- 
-- Helper tables. These tables are in MyISAM format because they
-- contain a relatively low number of rows (at most thousands) and
-- these tables should be optimized for speed and not for size.
-- 

-- Table for holding site ID's. Used in phases 1 and 2 to switch
-- server-service pair to a single key, site_id.
CREATE TABLE `site` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `server` varchar(13) DEFAULT NULL,
  `service` varchar(46) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `server` (`server`,`service`)
) ENGINE=MyISAM;
