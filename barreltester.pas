program BarrelTester;

uses Barrel;

procedure RootPage(req : TRequest; res : TResponse);
begin
	res.Status := 200;
	res.Body := '<!DOCTYPE html><html><body style="position:fixed;top:20%;left:0%;right:0%;"><center>Hello World!</center></body></html>';
end;

var
	Server : TApp;

procedure Main();
begin
	Server := TApp.Create;
	Server.AddRoute('/', @RootPage);
	Server.Run('localhost', 5000);
end;

begin
	Main;
end.