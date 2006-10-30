unit tiCGIExtensionRequest;

interface
uses
  tiBaseObject
  ;

type
  {: Abstarct class for shelling out to a CGI Extension.}
  TtiCGIExtensionRequest = class(TtiBaseObject)
  public
    class function CreateInstance: TtiCGIExtensionRequest;
    function Execute(const AULR : string;
                     const ACGIExeName: string ;
                     const AParams : string;
                     const AConnectWith: string;
                     const AProxyServerActive: Boolean;
                     const AProxyServerName: string;
                     const AProxyServerPort: integer ): string; virtual; abstract;
  end ;

  TtiCGIExtensionRequestClass = class of TtiCGIExtensionRequest;

var
  gCGIExtensionRequestClass: TtiCGIExtensionRequestClass;

implementation

{ TtiCGIExtensionRequest }

class function TtiCGIExtensionRequest.CreateInstance: TtiCGIExtensionRequest;
begin
  Assert(gCGIExtensionRequestClass<>nil, 'gCGIExtensionRequestClass not assigned');
  result := gCGIExtensionRequestClass.Create;
end;

end.
