unit app_events;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,mvc_base
  ,event_const
  ;

type

  TRemoveSubControllerEvent = class(TMVCEvent)
  private
    FControllerName: string;
    procedure SetControllerName(const AValue: string);
  public
    property    ControllerName: string read FControllerName write SetControllerName;
    constructor Create(ASource: TObject; const ACtrlName: string); reintroduce; overload;
  end;

implementation

{ TRemoveSubControllerEvent }

constructor TRemoveSubControllerEvent.Create(ASource: TObject;
  const ACtrlName: string);
begin
  inherited Create(ASource);
  FControllerName := ACtrlName;
  Name := DO_CLOSE_SUBCONTROLLER;
end;

procedure TRemoveSubControllerEvent.SetControllerName(const AValue: string);
begin
  if FControllerName=AValue then exit;
  FControllerName:=AValue;
end;

end.

