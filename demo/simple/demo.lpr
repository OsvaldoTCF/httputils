program demo;

{$mode objfpc}{$H+}

uses
  HttpUtils;

begin
  Write(HttpGetText('http://silvioprog.com.br/'));
  Download('http://mail.google.com/mail/help/images/screenshots/chat/grin.gif')
end.

