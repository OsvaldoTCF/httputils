program demo;

{$mode objfpc}{$H+}

uses
  HttpUtils;

procedure Progress(const AMax, APosition: Integer);
begin
  WriteLn(APosition, ' of ', AMax, ' ...');
end;

var
  VDownload: TDownload;
begin
  VDownload := TDownload.Create;
  try
    VDownload.OnProgress := @Progress;
    VDownload.Url := 'https://github.com/silvioprog/httputils/archive/master.zip';
    if not VDownload.Execute then
      WriteLn('Fail: ', VDownload.Http.Sock.LastErrorDesc);
  finally
    VDownload.Free;
  end;
end.

