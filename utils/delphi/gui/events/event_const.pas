unit event_const;

{$IFDEF fpc}
  {$mode objfpc}{$H+}
{$ENDIF}


interface

const

   BEFORE_APP_TERM = 'BeforeAppTerm';
   BEFORE_PROJ_CLOSE = 'BeforeProjClose';

   DO_ADD_UNIT = 'DoAddUnit';
   DO_CHANGE_UNIT_NAME = 'DoChangedUnitName';
   DO_CLOSE_APP = 'DoCloseApp';
   DO_CLOSE_PROJECT = 'DoCloseProject';
   DO_CLOSE_ALL_SUBCONTROLLERS = 'DoCloseAllSubControllers';
   DO_CLOSE_SUBCONTROLLER = 'DoCloseSubController';
   DO_CLOSE_VIEW = 'DoCloseView';
   DO_DELETE_UNIT = 'DoDeleteUnit';
   DO_CREATE_CLASS = 'DoCreateClass';
   DO_DELETE_CLASS = 'DoDeleteClass';
   DO_EDIT_CLASS = 'DoEditClass';
   DO_EDIT_PROJ_SETTINGS = 'DoEditProjSettings';
   DO_LOAD_PROJECT = 'DoLoadProject';
   DO_SAVE = 'DoSave';
   DO_SAVE_AS = 'DoSaveAs';

   PROJ_CLOSED = 'ProjClosed';
   ON_PROJ_LOADED = 'ProjLoaded';
   PROJ_SAVED = 'ProjSaved';

   UNIT_SELECTED = 'UnitSelected';



implementation

end.

