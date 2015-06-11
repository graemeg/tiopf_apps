unit helpers;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils
  ,Classes
  ;

  // Format, then re-rasie an exception
  procedure tiFmtException( e : Exception ;
                            const psMessage   : string ;
                            const psClassName : string = '' ;
                            const psMethod    : string = '' ) ; overload ;

  procedure tiFmtException( const psMessage   : string ;
                            const pA : Array of Const ;
                            const psClassName : string = ''   ;
                            const psMethod    : string = '' ) ; overload ;

  procedure tiFmtException( const psMessage   : string ;
                            const psClassName : string = '' ;
                            const psMethod    : string = '' ) ; overload ;


implementation

uses
  tiUtils
  ;

const
  cgExceptionMessageStart   = '* * * Message * * *'    ;
  cgExceptionMessageEnd     = '* * * End Message * * *'   ;
  cgExceptionCallStackStart = '* * * Call Stack * * *'  ;
  cgExceptionCallstackEnd   = '* * * End Call Stack * * *' ;
  cgExceptionDetailsStart   = '* * * Details * * *'    ;
  cgExceptionDetailsEnd     = '* * * End Details * * *'   ;


procedure _FormatAndRaiseException( const pMessage, pCallStack : string ) ;
var
  lsException : string ;
begin
  lsException :=
    cgExceptionMessageStart    + Cr( 1 ) +
    pMessage                   + Cr( 1 ) +
    cgExceptionMessageEnd      + Cr( 2 ) +
    cgExceptionCallStackStart  + Cr( 1 ) +
    pCallStack         + Cr( 1 ) +
    cgExceptionCallStackEnd;
  raise exception.create( lsException ) ;
end;

// -----------------------------------------------------------------------------
procedure tiFmtException( e : Exception ;
                          const psMessage   : string ;
                          const psClassName : string = '' ;
                          const psMethod    : string = '' ) ;
var
  lsException : string ;
  lsMessage   : string ;
  lsCallStack : string ;
begin
  lsException := e.message ;
  if ( Pos( cgExceptionMessageStart,   lsException ) <> 0 ) and
     ( Pos( cgExceptionMessageEnd,     lsException ) <> 0 ) and
     ( Pos( cgExceptionCallStackStart, lsException ) <> 0 ) and
     ( Pos( cgExceptionCallStackEnd,   lsException ) <> 0 ) then
  begin
    lsMessage   := tiSubStr( lsException, cgExceptionMessageStart,   cgExceptionMessageEnd ) ;
    lsCallStack := tiSubStr( lsException, cgExceptionCallStackStart, cgExceptionCallStackEnd ) ;
    lsMessage   := Trim( psMessage ) + Cr + Trim(lsMessage) ;
    lsCallStack := psClassName + '.' + psMethod + Cr + Trim( lsCallStack ) ;
  end
  else
  begin
    lsMessage   := lsException + Cr(2) + psMessage ;
    lsCallStack := psClassName + '.' + psMethod ;
  end ;

  _FormatAndRaiseException( lsMessage, lsCallStack ) ;

end ;

// -----------------------------------------------------------------------------
procedure tiFmtException( const psMessage   : string ;
                          const pA : Array of Const ;
                          const psClassName : string = ''   ;
                          const psMethod    : string = '' ) ; overload ;
begin
  _FormatAndRaiseException( Format( psMessage, pA ), psClassName + '.' + psMethod ) ;
end ;

// -----------------------------------------------------------------------------
procedure tiFmtException( const psMessage   : string ;
                          const psClassName : string = '' ;
                          const psMethod    : string = '' ) ;
begin
  _FormatAndRaiseException( psMessage, psClassName + '.' + psMethod ) ;
end ;

end.

