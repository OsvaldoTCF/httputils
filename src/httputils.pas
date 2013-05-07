(*
  HttpUtils plugin.
  Copyright (C) 2012-2014 Silvio Clecio.

  Please see the LICENSE file.
*)

unit HttpUtils;

{$mode objfpc}{$H+}

interface

uses
  HttpSend, BlckSock, SynaUtil, Classes, SysUtils, FPJSON, JSONParser;

type
  TRequestMethod = (rmGet, rmHead, rmOptions, rmPost, rmPut, rmDelete);

  TDownloadEvent = procedure(const AMax, APosition: Integer) of object;

  TProgressEvent = procedure(const AMax, APosition: Integer);

  TMemory = Pointer;

  THttpSettings = record
    Redirection: Boolean;
  end;

  TProxySettings = record
    Host,
    Port,
    User,
    Password: ShortString;
  end;

  THttpResult = record
    Code: Integer;
    Text: string;
  end;

  { TDownload }

  TDownload = class
  private
    FDownloaded: Integer;
    FHttp: THTTPSend;
    FFileName: TFileName;
    FOnDownload: TDownloadEvent;
    FOnMonitor: THookMonitor;
    FOnProgress: TProgressEvent;
    FOnStatus: THookSocketStatus;
    FUrl: string;
    function GetHttp: THTTPSend;
  protected
    class function InternalDownload(AHttp: THTTPSend; const AUrl: string;
      const AFileName: TFileName): Boolean;
    procedure HookMonitor(ASender: TObject; AWriting: Boolean;
      const ABuffer: TMemory; ALength: Integer);
    procedure HookStatus(ASender: TObject; AReason: THookSocketReason;
      const AValue: string);
  public
    destructor Destroy; override;
    function Execute: Boolean;
    property Http: THTTPSend read GetHttp;
    property Url: string read FUrl write FUrl;
    property FileName: TFileName read FFileName write FFileName;
    property Downloaded: Integer read FDownloaded;
    property OnDownload: TDownloadEvent read FOnDownload write FOnDownload;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnMonitor: THookMonitor read FOnMonitor write FOnMonitor;
    property OnStatus: THookSocketStatus read FOnStatus write FOnStatus;
  end;

var
  HttpSettings: THttpSettings = (
    Redirection: True;
  );

  ProxySettings: TProxySettings = (
    Host: '';
    Port: '';
    User: '';
    Password: '';
  );

function NormalizeUrl(const AUrl: string): string;
function UrlWithoutFileName(const AUrl: string): string;
function FileNameFromUrl(const AUrl: string): string;
function CreateBoundary: ShortString;
function HttpRequest(AHttpSend: THttpSend;
  const AMethod, AUrl: string): THttpResult;
function HttpRequest(const AUrl: string; AResponse: TStrings;
  const AMethod: TRequestMethod = rmGet): THttpResult;
function HttpRequest(const AUrl: string; AResponse: TStream;
  const AMethod: TRequestMethod = rmGet): THttpResult;
function HttpRequest(const AUrl: string; AResponse: TJSONData;
  const AMethod: TRequestMethod = rmGet): THttpResult;
function HttpRequest(AData: TStream; const AUrl: string;
  const AMethod: TRequestMethod = rmPost;
  const AContentType: string = 'application/octet-stream'): THttpResult;
function HttpRequest(AData: TJSONData; const AUrl: string;
  const AMethod: TRequestMethod = rmPost;
  const AContentType: string = 'application/json'): THttpResult;
function HttpRequest(const AUrl, AUrlData: string; AData: TStream;
  const AMethod: TRequestMethod = rmPost;
  const AContentType: string = 'application/x-www-form-urlencoded'): THttpResult;
function HttpRequest(const AUrl, AFieldName, AFileName: string;
  AData: TStream; AResultData: TStrings; const AMethod: TRequestMethod = rmPost;
  const AContentType: string = 'application/octet-stream'): THttpResult;
function HttpGetText(const AUrl: string): string;
function Download(const AUrl: string; const AFileName: TFileName = ''): Boolean;

implementation

function NormalizeUrl(const AUrl: string): string;

  function _IsFileName: Boolean;
  var
    I: Integer;
    UrlSep: set of Char;
  begin
    I := Length(AUrl);
    UrlSep := [':'] + ['/'] + ['.'];
    while (I > 0) and not (AUrl[I] in UrlSep) do
      Dec(I);
    Result := (I > 0) and (AUrl[I] = '.');
  end;

var
  C: Char;
  S: string;
  I: Integer;
begin
  Result := AUrl;
  I := Pos('//', AUrl);
  if I > 0 then
    I += 2;
  S := Copy(AUrl, I, MaxInt);
  if (Pos('/', S) <> 0) and _IsFileName then
    Exit;
  if (Result = '') or (Pos('?', Result) <> 0) or (Pos('#', Result) <> 0) or
    (Pos('|', Result) <> 0) then
    Exit;
  C := Result[Length(Result)];
  if C <> '/' then
    Result += '/';
end;

function UrlWithoutFileName(const AUrl: string): string;
var
  I: Integer;
begin
  Result := NormalizeUrl(AUrl);
  I := LastDelimiter('/', Result);
  Result := StringReplace(Result, Copy(Result, I + 1, Length(Result) - (I)), '',
    [rfReplaceAll]);
end;

function FileNameFromUrl(const AUrl: string): string;
begin
  Result := ExtractFileName(StringReplace(AUrl, '/', DirectorySeparator,
    [rfReplaceAll]));
end;

function CreateBoundary: ShortString;
begin
  Result := IntToHex(Random(MaxInt), 8) + '_httputils_boundary';
end;

function HttpRequest(AHttpSend: THttpSend;
  const AMethod, AUrl: string): THttpResult;
var
  VResult: Boolean;
  VOldNameValSep: Char;
  VUrl, VOldUrl: string;
begin
  Result.Code := 0;
  Result.Text := '';
  if not Assigned(AHttpSend) then
    raise Exception.Create('ShotMethod: "AHttpSend"  must not be nil.');
  if AMethod = '' then
    raise Exception.Create('ShotMethod: "AMethod" must not be empty.');
  if AUrl = '' then
    raise Exception.Create('ShotMethod: "AUrl" must not be empty.');
  if ((CompareText(AMethod, 'POST') = 0) or (CompareText(AMethod, 'PUT') = 0)) and
    (AHttpSend.Document.Size = 0) then
    AHttpSend.Headers.Insert(0, 'Content-Length: 0');
  AHttpSend.ProxyHost := ProxySettings.Host;
  AHttpSend.ProxyPort := ProxySettings.Port;
  AHttpSend.ProxyUser := ProxySettings.User;
  AHttpSend.ProxyPass := ProxySettings.Password;
  if HttpSettings.Redirection then
  begin
    // checking redirection...
    VUrl := AUrl;
    VOldNameValSep := AHttpSend.Headers.NameValueSeparator;
    AHttpSend.Headers.NameValueSeparator := ':';
    try
      VResult := AHttpSend.HTTPMethod(AMethod, VUrl);
      if not VResult then
        repeat
          VOldUrl := VUrl;
          VUrl := Trim(AHttpSend.Headers.Values['location']);
          if Pos('://', VUrl) = 0 then
            VUrl := UrlWithoutFileName(VOldUrl) + VUrl;
          AHttpSend.Clear;
          VResult := AHttpSend.HTTPMethod(AMethod, VUrl);
        until (AHttpSend.ResultCode <> 302) or (AHttpSend.ResultCode <> 307) and
          (AHttpSend.ResultCode = 200);
    finally
      AHttpSend.Headers.NameValueSeparator := VOldNameValSep;
    end;
  end
  else
    AHttpSend.HTTPMethod(AMethod, AUrl);
  Result.Code := AHttpSend.ResultCode;
  Result.Text := AHttpSend.ResultString;
end;

function HttpRequest(const AUrl: string; AResponse: TStrings;
  const AMethod: TRequestMethod): THttpResult;
var
  VMethod: string;
  VHttp: THttpSend;
begin
  VHttp := THttpSend.Create;
  try
    case AMethod of
      rmGet: VMethod := 'GET';
      rmHead: VMethod := 'HEAD';
      rmOptions: VMethod := 'OPTIONS';
    end;
    Result := HttpRequest(VHttp, VMethod, AUrl);
    if Assigned(AResponse) and (VHttp.Document.Size > 0) then
      AResponse.LoadFromStream(VHttp.Document);
  finally
    VHttp.Free;
  end;
end;

function HttpRequest(const AUrl: string; AResponse: TStream;
  const AMethod: TRequestMethod): THttpResult;
var
  VMethod: string;
  VHttp: THttpSend;
begin
  VHttp := THttpSend.Create;
  try
    case AMethod of
      rmGet: VMethod := 'GET';
      rmHead: VMethod := 'HEAD';
      rmOptions: VMethod := 'OPTIONS';
    else
      raise Exception.Create('HttpRequest: Invalid request method.');
    end;
    Result := HttpRequest(VHttp, VMethod, AUrl);
    if Assigned(AResponse) and (VHttp.Document.Size > 0) then
    begin
      AResponse.Seek(0, soFromBeginning);
      AResponse.CopyFrom(VHttp.Document, 0);
    end;
  finally
    VHttp.Free;
  end;
end;

function HttpRequest(const AUrl: string; AResponse: TJSONData;
  const AMethod: TRequestMethod): THttpResult;
var
  VMethod: string;
  VHttp: THttpSend;
  VParser: TJSONParser;
begin
  VHttp := THttpSend.Create;
  try
    case AMethod of
      rmGet: VMethod := 'GET';
      rmHead: VMethod := 'HEAD';
      rmOptions: VMethod := 'OPTIONS';
    else
      raise Exception.Create('HttpRequest: Invalid request method.');
    end;
    Result := HttpRequest(VHttp, VMethod, AUrl);
    if Assigned(AResponse) and (VHttp.Document.Size > 0) then
    begin
      VHttp.Document.Position := 0;
      VParser :=  TJSONParser.Create(VHttp.Document);
      try
        FreeAndNil(AResponse);
        AResponse := VParser.Parse;
      finally
        VParser.Free;
      end;
    end;
  finally
    VHttp.Free;
  end;
end;

function HttpRequest(AData: TStream; const AUrl: string;
  const AMethod: TRequestMethod; const AContentType: string): THttpResult;
var
  VMethod: string;
  VHttp: THttpSend;
begin
  VHttp := THttpSend.Create;
  try
    case AMethod of
      rmPost: VMethod := 'POST';
      rmPut: VMethod := 'PUT';
      rmDelete: VMethod := 'DELETE';
    else
      raise Exception.Create('HttpRequest: Invalid request method.');
    end;
    VHttp.Document.CopyFrom(AData, 0);
    VHttp.MimeType := AContentType;
    Result := HttpRequest(VHttp, VMethod, AUrl);
    if VHttp.Document.Size > 0 then
    begin
      AData.Size := 0;
      AData.Seek(0, soFromBeginning);
      AData.CopyFrom(VHttp.Document, 0);
    end;
  finally
    VHttp.Free;
  end;
end;

function HttpRequest(AData: TJSONData; const AUrl: string;
  const AMethod: TRequestMethod; const AContentType: string): THttpResult;
var
  VMethod: string;
  VHttp: THttpSend;
  VParser: TJSONParser;
  VJSON: TJSONStringType;
begin
  VHttp := THttpSend.Create;
  try
    case AMethod of
      rmPost: VMethod := 'POST';
      rmPut: VMethod := 'PUT';
      rmDelete: VMethod := 'DELETE';
    else
      raise Exception.Create('HttpRequest: Invalid request method.');
    end;
    VJSON := AData.AsJSON;
    VHttp.Document.Write(Pointer(VJSON)^, Length(VJSON));
    VHttp.MimeType := AContentType;
    Result := HttpRequest(VHttp, VMethod, AUrl);
    if VHttp.Document.Size > 0 then
    begin
      VHttp.Document.Position := 0;
      VParser := TJSONParser.Create(VHttp.Document);
      try
        FreeAndNil(AData);
        AData := VParser.Parse;
      finally
        VParser.Free;
      end;
    end;
  finally
    VHttp.Free;
  end;
end;

function HttpRequest(const AUrl, AUrlData: string; AData: TStream;
  const AMethod: TRequestMethod; const AContentType: string): THttpResult;
var
  VMethod: string;
  VHttp: THttpSend;
begin
  VHttp := THttpSend.Create;
  try
    case AMethod of
      rmPost: VMethod := 'POST';
      rmPut: VMethod := 'PUT';
      rmDelete: VMethod := 'DELETE';
    else
      raise Exception.Create('HttpRequest: Invalid request method.');
    end;
    WriteStrToStream(VHttp.Document, AUrlData);
    VHttp.MimeType := AContentType;
    Result := HttpRequest(VHttp, VMethod, AUrl);
    if Assigned(AData) and (VHttp.Document.Size > 0) then
      AData.CopyFrom(VHttp.Document, 0);
  finally
    VHttp.Free;
  end;
end;

function HttpRequest(const AUrl, AFieldName, AFileName: string; AData: TStream;
  AResultData: TStrings; const AMethod: TRequestMethod;
  const AContentType: string): THttpResult;
var
  S, VBoundary, VMethod: string;
  VHttp: THttpSend;
begin
  VHttp := THttpSend.Create;
  try
    case AMethod of
      rmPost: VMethod := 'POST';
      rmPut: VMethod := 'PUT';
      rmDelete: VMethod := 'DELETE';
    else
      raise Exception.Create('HttpRequest: Invalid request method.');
    end;
    VBoundary := CreateBoundary;
    S := '--' + VBoundary + #13#10 + 'Content-Disposition: form-data; name="' +
      AFieldName + '";' + ' filename="' + AFileName + '"' + #13#10 +
      'Content-Type: ' + AContentType + #13#10 + #13#10;
    WriteStrToStream(VHttp.Document, S);
    VHttp.Document.CopyFrom(AData, 0);
    S := #13#10 + '--' + VBoundary + '--' + #13#10;
    WriteStrToStream(VHttp.Document, S);
    VHttp.MimeType := 'multipart/form-data; boundary=' + VBoundary;
    Result := HttpRequest(VHttp, VMethod, AUrl);
    if VHttp.Document.Size > 0 then
      AResultData.LoadFromStream(VHttp.Document);
  finally
    VHttp.Free;
  end;
end;

function HttpGetText(const AUrl: string): string;
var
  L: SizeInt;
  VHttp: THttpSend;
begin
  VHttp := THttpSend.Create;
  try
    if HttpRequest(VHttp, 'GET', AUrl).Code = 200 then
    begin
      L := VHttp.Document.Size;
      SetLength(Result, L);
      VHttp.Document.Read(Pointer(Result)^, L);
    end;
  finally
    VHttp.Free;
  end;
end;

function Download(const AUrl: string; const AFileName: TFileName): Boolean;
var
  VHttp: THttpSend;
begin
  VHttp := THttpSend.Create;
  try
    Result := TDownload.InternalDownload(VHttp, AUrl, AFileName);
  finally
    VHttp.Free;
  end;
end;

{ TDownload }

destructor TDownload.Destroy;
begin
  FreeAndNil(FHttp);
  inherited Destroy;
end;

function TDownload.GetHttp: THTTPSend;
begin
  if not Assigned(FHttp) then
    FHttp := THTTPSend.Create;
  Result := FHttp;
end;

class function TDownload.InternalDownload(AHttp: THTTPSend;
  const AUrl: string; const AFileName: TFileName): Boolean;
var
  VFileName: TFileName;
begin
  Result := HttpRequest(AHttp, 'GET', AUrl).Code = 200;
  if Result then
  begin
    if AFileName = '' then
      VFileName := FileNameFromUrl(AUrl)
    else
      VFileName := AFileName;
    AHttp.Document.SaveToFile(VFileName)
  end;
end;

procedure TDownload.HookMonitor(ASender: TObject; AWriting: Boolean;
  const ABuffer: TMemory; ALength: Integer);
begin
  if FHttp.DownloadSize > 0 then
  begin
    Inc(FDownloaded, ALength);
    if Assigned(FOnDownload) then
      FOnDownload(FHttp.DownloadSize, FDownloaded);
    if Assigned(FOnProgress) then
      FOnProgress(FHttp.DownloadSize, FDownloaded);
  end;
  if Assigned(FOnMonitor) then
    FOnMonitor(ASender , AWriting, ABuffer, ALength);
end;

procedure TDownload.HookStatus(ASender: TObject; AReason: THookSocketReason;
  const AValue: string);
begin
  if Assigned(FOnStatus) then
    FOnStatus(ASender, AReason, AValue);
end;

function TDownload.Execute: Boolean;
begin
  FDownloaded := 0;
  Http.Sock.OnMonitor := @HookMonitor;
  FHttp.Sock.OnStatus := @HookStatus;
  Result := TDownload.InternalDownload(FHttp, FUrl, FFileName);
end;

end.
