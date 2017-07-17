{ This unit is for DELPHI only, and allows to ease console app development.
  This unit (with even more functionality) already exists is FPC for years. }
unit delphi_custom_app;

interface

uses
  Classes,
  SysUtils;

type

  TExceptionEvent = Procedure (Sender : TObject; E : Exception) Of Object;

  TCustomApplication = class(TComponent)
  private
    FStopOnException: Boolean;
    FTitle: string;
    FOnException: TExceptionEvent;
    FExceptionExitCode: Longint;
    FTerminated: boolean;
    FCaseSensitiveOptions: Boolean;
    FOptionChar: Char;
    function GetExeName: string;
    procedure SetTitle(const Value: string);
    function  FindOptionIndex(const S: String; var Longopt: Boolean; StartAt: Integer = -1): Integer;
    function GetParamCount: Integer;
    function GetParams(Index: integer): string;
  protected
    procedure DoRun; virtual; abstract;
    procedure HandleException(Sender: TObject); virtual;
    function  GetOptionAtIndex(AIndex: Integer; IsLong: Boolean): String;
  published
  public
    constructor Create(AOwner: TComponent); override;
    function HasOption(const S: String): Boolean; overload;
    function  HasOption(const C: Char; const S: String): Boolean; overload;
    function  GetOptionValue(Const S : String) : String; overload;
    function  GetOptionValue(const C: Char; const S: String) : String; overload;
    procedure Run;
    procedure Terminate; overload; virtual;
    procedure Terminate(AExitCode: Integer); overload; virtual;
    property  CaseSensitiveOptions: Boolean read FCaseSensitiveOptions write FCaseSensitiveOptions;
    property  ExeName: string read GetExeName;
    property  ExceptionExitCode: Longint read FExceptionExitCode write FExceptionExitCode;
    property  OnException: TExceptionEvent read FOnException write FOnException;
    property  OptionChar: Char read FOptionChar write FOptionChar;
    property  Params[Index: integer]: string read GetParams;
    property  ParamCount: Integer read GetParamCount;
    property  StopOnException: Boolean read FStopOnException write FStopOnException;
    property  Title: string read FTitle write SetTitle;
  end;


implementation

{ TCustomApplication }

constructor TCustomApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptionChar := '-';
  FCaseSensitiveOptions := True;
  FStopOnException := False;
end;

function TCustomApplication.FindOptionIndex(const S: String;
  var Longopt: Boolean; StartAt: Integer): Integer;
var
  SO, O: String;
  I, P: Integer;
begin
  if not CaseSensitiveOptions then
    SO := UpperCase(S)
  else
    SO := S;
  Result := -1;
  I := StartAt;
  if (I = -1) then
    I := ParamCount;
  while (Result = -1) and (I > 0) do
  begin
    O := Params[I];
    // - must be seen as an option value
    if (Length(O) > 1) and (O[1] = FOptionChar) then
    begin
      Delete(O, 1, 1);
      Longopt := (Length(O) > 0) and (O[1] = FOptionChar);
      if Longopt then
      begin
        Delete(O, 1, 1);
        P := Pos('=', O);
        If (P <> 0) then
          O := Copy(O, 1, P - 1);
      end;
      if not CaseSensitiveOptions then
        O := UpperCase(O);
      if (O = SO) then
        Result := I;
    end;
    Dec(I);
  end;
end;

function TCustomApplication.GetExeName: string;
begin
  Result := ParamStr(0);
end;

function TCustomApplication.GetOptionValue(const S: String): String;
begin
  Result := GetOptionValue(#255, S);
end;

function TCustomApplication.GetOptionAtIndex(AIndex: Integer; IsLong: Boolean): String;
var
  P : Integer;
  O : String;
begin
  Result:='';
  If (AIndex=-1) then
    Exit;
  If IsLong then
  begin // Long options have form --option=value
    O:=Params[AIndex];
    P:=Pos('=',O);
   If (P=0) then
      P:=Length(O);
    Delete(O,1,P);
    Result:=O;
  end
  else
  begin // short options have form '-o value'
    If (AIndex<ParamCount) then
      if (Copy(Params[AIndex+1],1,1)<>'-') then
        Result:=Params[AIndex+1];
  end;
end;

function TCustomApplication.GetOptionValue(const C: Char; const S: String): String;
var
  B : Boolean;
  I : integer;
begin
  Result:='';
  I:=FindOptionIndex(C,B);
  If (I=-1) then
    I:=FindOptionIndex(S,B);
  If I<>-1 then
    Result := GetOptionAtIndex(I,B);
end;

function TCustomApplication.GetParamCount: Integer;
begin
  Result := System.ParamCount;
end;

function TCustomApplication.GetParams(Index: integer): string;
begin
  Result := ParamStr(Index);
end;

procedure TCustomApplication.HandleException(Sender: TObject);
begin
  if not (ExceptObject is Exception) then
    SysUtils.showexception(ExceptObject,ExceptAddr)
  else
  begin
    if not Assigned(FOnexception) then
      ShowException(Exception(ExceptObject), nil)
    else
      FOnException(Sender, Exception(ExceptObject));
  end;
  if FStopOnException then
    Terminate(ExceptionExitCode);
end;

function TCustomApplication.HasOption(const S: String): Boolean;
var
  B: Boolean;
begin
  Result := FindOptionIndex(S,B)<>-1;
end;

function TCustomApplication.HasOption(const C: Char; const S: String): Boolean;
var
  B: Boolean;
begin
  Result := (FindOptionIndex(C,B)<>-1) or (FindOptionIndex(S,B)<>-1);
end;

procedure TCustomApplication.Run;
begin
  repeat
    try
      DoRun;
    except
      HandleException(Self);
    end;
  until FTerminated;
end;

procedure TCustomApplication.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TCustomApplication.Terminate(AExitCode: Integer);
begin
  FTerminated := True;
  ExitCode := AExitCode;
end;

procedure TCustomApplication.Terminate;
begin
  FTerminated := True;
end;

end.
