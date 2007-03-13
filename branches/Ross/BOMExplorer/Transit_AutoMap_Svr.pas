unit Transit_AutoMap_Svr;

interface

procedure RegisterMappings;

implementation

uses
  tiOPFManager
  , tiClassToDBMap_BOM
  , Transit_BOM;

procedure RegisterMappings;
begin
  //Show Methods
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TShowMethod,
    'show_method', 'OID', 'OID', [pktDB]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TShowMethod,
    'show_method', 'Name', 'show_method_name');
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TShowMethodCollection, TShowMethod);

  //Periods
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPeriod, 'period',
    'OID', 'OID', [pktDB]);
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TPeriodCollection, TPeriod);

  //Midpoint Lists
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TMidpointList, 'rts_midpoint_list', 'OID',        'OID',                    [pktDB]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TMidpointList, 'rts_midpoint_list', 'Name',       'midpoint_list_name');
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TMidpointListCollection, TMidpointList);

  //Midpoints
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TMidpoint,      'rts_midpoint',     'OID',        'OID',                    [pktDB]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TMidpoint,      'rts_midpoint',     'Owner.OID',  'rts_midpoint_list_oid',  [pktFK] );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TMidpoint,      'rts_midpoint',     'Name',       'midpoint_name');
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TMidpoint,      'rts_midpoint',     'Punch',      'midpoint_punch');
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TMidpoint,      'rts_midpoint',     'Value',      'midpoint_value');
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TMidpointCollection, TMidpoint);

  //RTS Questions
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TRTSQuestion,
    'rts_question', 'OID', 'OID', [pktDB]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TRTSQuestion,
    'rts_question', 'Name', 'rts_question_name');
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TRTSQuestion,
    'rts_question', 'Number', 'rts_question_number');
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TRTSQuestionCollection, TRTSQuestion);

  {GOTCHA: the following won't work, because foreign keys of non-owners aren't supported
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TRTSQuestion, 'rts_question', 'MidpointList.OID', 'rts_midpoint_list_oid', [pktFK]);

  The above fails when the correct 'MidpointList.OID' is used instead of
  'Owner.OID'. Examine the following from tiClassToDBMap_BOM to see why...

  function TtiClassMap._IsPublicProperty(const AAttrName: string): Boolean;
  begin
    result :=
      SameText(AAttrName, 'OID') or
      SameText(AAttrName, 'Owner.OID') // Must do better than this
  end;

  So, automapping appears to have run out of gas.  Use other methods to support
  association relationships}

end;

end.
