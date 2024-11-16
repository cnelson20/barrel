program BarrelTester;
{$ifdef fpc}
  {$mode delphi}
  {$h+}
  {$m+}
{$endif}

{$unitpath barrel}

uses Barrel, Templater, Generics.Collections;

procedure RootPage(req : TRequest; res : TResponse);
begin
	res.Status := 200;
	res.Body := '<!DOCTYPE html><html><body style="position:fixed;top:20%;left:0%;right:0%;"><center>Hello World!</center>Pages:<br><a href="/test">GetTest</a><br><a href="/post">POST</a><br><a href="/source">Source</a><br></body></html>';
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

procedure PostTest(req : TRequest; res : TResponse);
begin
	res.Status := 200;
	res.Body := '<!DOCTYPE html><html><body><div>';
	res.Body += WriteStringMap(req.PostForm);
	res.Body += '</div>';
	res.Body += '<form action="/post" method="POST"><textarea name="user"></textarea><br><input name="pass" type="text"><br><input type="submit" value="Send"></form>';
	res.Body += '</body></html>';
end;

procedure LoaderTest(req : TRequest; res : TResponse);
var
	Params : TDictionary<String,String>;
	RandString : String;
begin
	Params := TDictionary<String,String>.Create;
	
	Str(random(100), RandString);
	Params.Add('rand', RandString);
	
	res.body := RunTemplateAndCache('templates/test.html', Params);
	res.Status := 200;
end;

var
	Server : TApp;

procedure Main();
begin
	Server := TApp.Create;
	Server.AddRoute('/', @RootPage);
	Server.AddRoute('/test', @GetTest);
	Server.AddRoute('/post', @PostTest);
	Server.AddRoute('/template', @LoaderTest);
	Server.Run('localhost', 5000);
end;

begin
	Main;
end.
