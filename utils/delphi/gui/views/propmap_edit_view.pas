unit propmap_edit_view;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, base_dialog_view, StdCtrls, ComCtrls;

type
  TPropMapEditView = class(TBaseDialogView)
    eFieldName: TEdit;
    lblTableName: TLabel;
    cboPropType: TComboBox;
    lblPropType: TLabel;
    lblPKField: TLabel;
    cboPropName: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  {

  property    TableName: string read FTableName write SetTableName;
    property    PKName: string read FPKName write SetPKName;
    property    PKField: string read FPKField write SetPKField;
    property    PropMappings: TPropMappingList read FPropMappings;
    property    OIDType: TOIDType read FOIDType write SetOIDType;

  }


var
  PropMapEditView: TPropMapEditView;

implementation

{$R *.dfm}

end.
