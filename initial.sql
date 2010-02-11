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
  `line` text
) ENGINE=ARCHIVE;

-- 
-- Tables for processing errors
-- 

-- Phase 1 processing errors. Names of failed log files and the related error
-- message.
CREATE TABLE `phase_1_error` (
  `file` text,
  `error` text
) ENGINE=ARCHIVE;

