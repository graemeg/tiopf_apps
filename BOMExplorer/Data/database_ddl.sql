create table Next_OID
    ( OID Integer not null,
    Primary Key ( OID ))

insert into Next_OID values ( 0 );

create table language
    ( OID Integer not null,
    language_name VarChar( 50 ) not null,
    Primary Key ( OID ))

create unique index language_uk on language ( language_name ) ;

insert into language values ( 1, "English" );
insert into language values ( 2, "Français" );

create table period
    ( OID Integer not null,
    Primary Key ( OID ))

insert into period values ( 100 );
insert into period values ( 101 );
insert into period values ( 102 );

create table period_language
    ( language_oid Integer not null,
    period_oid Integer not null,
    period_label VarChar( 50 ) not null,
    Primary Key ( language_OID, period_OID ),
    Foreign Key ( language_oid ) references language( OID ),
    Foreign Key ( period_oid ) references period( OID )
)

create unique index period_language_uk on period_language ( period_label ) ;

insert into period_language values ( 1, 100, "Spring" );
insert into period_language values ( 2, 100, "printemps" );
insert into period_language values ( 1, 101, "Fall" );
insert into period_language values ( 2, 101, "autumne" );
insert into period_language values ( 1, 102, "Winter" );
insert into period_language values ( 2, 102, "hiver" );

create table show_method
    ( OID Integer not null,
    show_method_name VarChar( 50 ) not null,
    Primary Key ( OID ))

create unique index show_method_uk on show_method ( show_method_name ) ;

insert into show_method values ( 200, "Show %" );
insert into show_method values ( 201, "Poster Count" );
insert into show_method values ( 202, "GRPs" );

create table rts_midpoint_list
    ( OID Integer not null,
    midpoint_list_name VarChar( 50 ) not null,
    Primary Key ( OID ))

create unique index midpoint_list_uk on rts_midpoint_list ( 
midpoint_list_name ) ;

insert into rts_midpoint_list values ( 300, "trips" );

create table rts_midpoint
    ( OID Integer not null,
    rts_midpoint_list_oid Integer not null,
    midpoint_punch Integer not null,
    midpoint_value Double not null,
    midpoint_name VarChar( 20 ) not null,
    Primary Key ( OID ),
    Foreign Key (rts_midpoint_list_oid) references rts_midpoint_list(OID))

create unique index midpoint_uk on rts_midpoint ( midpoint_name ) ;
create unique index midpoint_punch_uk on rts_midpoint ( 
rts_midpoint_list_oid, midpoint_punch ) ;

insert into rts_midpoint values ( 400, 300, 0, 0,   "None" );
insert into rts_midpoint values ( 401, 300, 1, 1.5, "1-2" );
insert into rts_midpoint values ( 402, 300, 2, 3.5, "3-4" );
insert into rts_midpoint values ( 403, 300, 3, 5,   "5" );
insert into rts_midpoint values ( 404, 300, 4, 7,   "6-9" );
insert into rts_midpoint values ( 405, 300, 5, 10,  "10" );
insert into rts_midpoint values ( 406, 300, 6, 13.5,"11-15" );
insert into rts_midpoint values ( 407, 300, 7, 20,  "16 or more" );

create table rts_question
    ( OID Integer not null,
    rts_midpoint_list_oid Integer not null,
    rts_question_number VarChar( 20 ),
    rts_question_name VarChar( 20 ) not null,
    Primary Key ( OID ),
    Foreign Key (rts_midpoint_list_oid) references rts_midpoint_list(OID))

create unique index rts_question_uk on rts_question ( rts_question_name ) ;

insert into rts_question values ( 500, 300, "Q495-01", "How many kilometres (miles) do you drive locally in town in an AVERAGE WEEK?" );
insert into rts_question values ( 501, 300, "Q495-02", "How many single trips (going and returning = 2 trips) in total do you take by local bus/streetcar in an AVERAGE WEEK?" );
insert into rts_question values ( 502, 300, "Q495-03", "How many single trips (going and returning = 2 trips) in total do you take by subway in an AVERAGE WEEK?" );
insert into rts_question values ( 503, 300, "Q495-04", "How many single trips (going and returning = 2 trips) in total do you take by Go Transit (bus or train) in an AVERAGE WEEK?" );
insert into rts_question values ( 504, 300, "Q495-05", "How many single trips (going and returning = 2 trips) in total do you take by Metro in an AVERAGE WEEK?" );
insert into rts_question values ( 505, 300, "Q495-06", "How many single trips (going and returning = 2 trips) in total do you take by Commuter train in an AVERAGE WEEK?" );
insert into rts_question values ( 506, 300, "Q495-07", "How many single trips (going and returning = 2 trips) in total do you take by LRT in an AVERAGE WEEK?" );
insert into rts_question values ( 507, 300, "Q495-08", "How many single trips (going and returning = 2 trips) in total do you take by Skytrain in an AVERAGE WEEK?" );


commit ;