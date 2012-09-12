create database SPAN512;
USE SPAN512;

create table diagnostics(header_id int (4) not null, version_id int (4) not null, rfi_percentage float (8), occupancy_number float (8), primary key(header_id, version_id));

create table versions(version_id int (4) auto_increment not null, institution varchar(50) not null, pipeline varchar(50) not null, version_number varchar(50) not null, added_date datetime, primary key(version_id));

create table raw_files(obs_id int(4), path varchar(255), filename varchar(255), datasize bigint(8), primary key (filename));

create table stagged_files(obs_id int(4), path varchar(255), filename varchar(255), datasize bigint(8), primary key (filename));

create table pdm_plot_types(pdm_plot_type_id smallint(2) not null auto_increment, pdm_plot_type varchar(50), primary key(pdm_plot_type_id));

create table pdm_candidate_plots(pdm_cand_id int(4) not null, pdm_plot_type_id smallint(2) not null, png_filename varchar(255) not null, primary key(pdm_cand_id, pdm_plot_type_id));

create table pdm_classification_types(pdm_class_type_id smallint(2) not null auto_increment, description varchar(255) not null, min_value float(8), max_value float(8), default_value float(8), auto_generated enum('True', 'False'), datatype enum('Int', 'Float', 'Bool'), primary key (pdm_class_type_id));

create table pdm_classifications(pdm_cand_id int(4) not null, who varchar(50) not null, pdm_class_type_id smallint(2) not null, date datetime, rank float(8), primary key(pdm_cand_id, who, pdm_class_type_id));

create table headers(header_id int(4) auto_increment, obs_id int(4), basefilename varchar(128), beam_id tinyint(1), sample_time float(8), observation_time float(8), timestamp_mjd double, num_samples int(4), center_freq float(8), channel_bandwidth float(8), num_channels int(4), n_bits smallint(2), obs_stat char(1) default NULL, num_ifs smallint(2), source_name varchar(40), sum_id tinyint(1), start_ast float(8), start_lst float(8), project_id varchar(40), observers varchar(100), file_size int(4), data_size int(4), right_ascension varchar(12), declination varchar(12), galactic_longitude float(8), galactic_latitude float(8), ra_deg float(8), dec_deg float(8), primary key(header_id));

create table pdm_cand_files(pdm_cand_id int(4), filename varchar(255), primary key(pdm_cand_id));

create table pdm_candidates(pdm_cand_id int(4) auto_increment, header_id int(4), cand_num int(4), freq_topo float(8), freq_bary float(8), period_topo float(8), period_bary float(8), freq_topo_dot float(8), freq_bary_dot float(8), dm float(8), snr float(8), coherent_power float(8), incoherent_power float(8), num_hits int(4), num_harmonics int(4), version_id int(4), proc_date datetime, presto_sigma float(8), lkj1 float(8), lkj2 float(8), lkj3 float(8), primary key(pdm_cand_id));

create table pdm_plot_pointers(pdm_cand_id int(4), pdm_plot_type_id smallint(2), path varchar(255), ps_filename varchar(255), primary key(pdm_cand_id, pdm_plot_type_id));

create table processing(obs_id int(4) auto_increment, pointing_name char(16), grid_id int(4), basefilename varchar(64) default NULL, numfiles smallint(2) default NULL, institution varchar(32) default NULL, diskname varchar(32) default NULL, proc_stat char(1) default NULL, planned_date datetime default NULL, obsdate datetime default NULL, add_date datetime default NULL, proc_date datetime default NULL, results_date datetime default NULL, srv_id int(4) default NULL, primary key(obs_id));

create table full_processing(obs_id int(4), status text default NULL, guid char(36) default NULL, updated_at datetime default NULL, primary key(obs_id));

CREATE TABLE known (name varchar(16), right_ascension char(16), declination char(16), ra_deg float, dec_deg float, period float, dm float, primary key(name));


INSERT INTO `pdm_classification_types` (`description`, `min_value`, `max_value`, `default_value`, `auto_generated`, `datatype`) VALUES ('Human 8-Level Classification;0:Not Classified;1:Class 1;2:Class 2;3:Class 3;4:RFI;5:Not A Pulsar;6:Known;7:Harmonic',0,7,0,'False','Int');

create table sp_plots(sp_cand_id int(4) auto_increment,  header_id int(4), sp_num int(4), sp_plot_type_id smallint(2) not null, ps_filename varchar(255), png_filename varchar(255), primary key(sp_cand_id));

create table sp_classifications(sp_cand_id int(4) not null, who varchar(50) not null, sp_class_type_id smallint(2) not null, date datetime, rank float(8), primary key(sp_cand_id, who, sp_class_type_id));

create table NRT_grid(grid_id int(4) auto_increment, right_ascension char(16), declination char(16), ra_deg float(8), dec_deg float(8), is_SPAN boolean, primary key(grid_id));
