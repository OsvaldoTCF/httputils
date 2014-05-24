program demo;

{$mode objfpc}{$H+}

uses
  HttpUtils;

begin
  Write(HttpGetText('http://silvioprog.github.io/brookframework/'));
  Download('http://silvioprog.github.io/brookframework/favicon.ico');
end.

