program BarrelTester;
{$ifdef fpc}
  {$mode delphi}
  {$h+}
  {$m+}
{$endif}

uses Barrel, Templater, Generics.Collections;

procedure RootPage(req : TRequest; res : TResponse);
begin
	res.Status := 200;
	res.Body := '<!DOCTYPE html><html><body style="position:fixed;top:20%;left:0%;right:0%;"><center>Hello World!</center></body></html>';
end;

function WriteStringMap(m : TStringMap): String;
var
	i : LongInt;
	s : String;
	TempArray : TArray<TPair<String,String>>;
begin
	TempArray := m.ToArray;
	s := '';
	for i := 0 to Length(TempArray) - 1 do
		s += Concat('"', TempArray[i].Key, '" => "', TempArray[i].Value, '" <br>');  
	WriteStringMap := s;
end;

procedure GetTest(req : TRequest; res : TResponse);
begin
	res.Body := '<!DOCTYPE html><html>' + WriteStringMap(req.Args) + '</html>';
	res.Status := 200;
end;

procedure LoaderTest(req : TRequest; res : TResponse);
begin
	res.Status := 200;
	res.Body := LoadFileAndCache('barreltester.pas');
end;

var
	Server : TApp;

procedure Main();
begin
	Server := TApp.Create;
	Server.AddRoute('/', @RootPage);
	Server.AddRoute('/test', @GetTest);
	Server.AddRoute('/source', @LoaderTest);
	Server.Run('localhost', 5000);
end;

begin
	Main;
end.