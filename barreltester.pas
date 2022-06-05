program BarrelTester;

uses Barrel;

var
	Server : TApp;

procedure Main();
begin
	Server := TApp.Create;
	Server.Run('localhost', 5000);
end;

begin
	Main;
end.