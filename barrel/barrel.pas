unit Barrel;
{$ifdef fpc}
  {$mode delphi}
  {$h+}
  {$m+}
{$endif}

{@$apptype console}

interface
uses Classes, blcksock, sockets, Synautil, SysUtils, StrUtils, Generics.collections;

type
	TStringMap = TDictionary<String, String>;

	TRequest = class
	public
		uri : String;
		method : String;
		protocol : String;
		Args : TStringMap;
		PostForm : TStringMap;
		Headers : Array of String;

		constructor Create;
		destructor Destroy;
		procedure DeriveArgs(s : String; var Args : TStringMap);
	end;
	TResponse = class
	private
		ResponseHeaders : TStringMap;
	public
		Body : String; 
		Status : Word;

		constructor Create;
		destructor Destroy;
		procedure SetHeader(k, s : String);
	end;
	
	TRouteFunction = procedure(req : TRequest; res : TResponse);
	TRoutesMap = TDictionary<String, TRouteFunction>;
	
	TApp = class
	private
		Routes : TRoutesMap;
		DefaultHandler : TRouteFunction;
		DefaultHeaders : TStringMap;

		procedure WriteHeaders(ASocket: TTCPBlockSocket; ResponseHeaders : TStringMap);
		procedure AttendConnection(ASocket: TTCPBlockSocket);
	public
		
		constructor Create;
		procedure SetDefaultHandler(Fxn : TRouteFunction);
		procedure AddRoute(s : String; Fxn : TRouteFunction);
		procedure SetDefaultHeader(k, h : String);
		procedure Run(Host : String; ListenPort : Word);
	end;
	
implementation

function HexCharToInt(c : AnsiChar): Byte;
begin
	if Ord(c) <= $39 then
		HexCharToInt := Ord(c) - $30
	else if Ord(c) <= $61 then
		HexCharToInt := Ord(c) - $41 + $A
	else
		HexCharToInt := Ord(c) - $61 + $A;
end;

function ParseGetStrings(s : String): String;
var
	Temp : String;
	i : LongInt;
	j : LongInt;
begin
	Temp := '';
	i := 1;
	while i <= Length(s) do begin
		j := PosFrom('%', s, i);
		if j = 0 then begin
			Temp += Copy(s, i);
			break;
		end else begin
			Temp += Copy(s, i, j - i);
			i := j + 1;
			j := 16 * HexCharToInt(s[i]) + HexCharToInt(s[i+1]);
			Temp += Chr(j);
			i += 2;
		end;
	end;
	ParseGetStrings := Temp;
end;


{
  Attends a connection. Reads the headers and gives an
  appropriate response
}
procedure TApp.AttendConnection(ASocket: TTCPBlockSocket);
var
	timeout: integer;
	s: string;
	method, uri, protocol: String;

	RequestURI : String;

	TempInt : LongInt;
	req : TRequest;
	res : TResponse;
	RouteFunction : TRouteFunction;
begin
	//WriteLn(ASocket.RecvPacket(5000));
	//exit; 

	timeout := 120000;
	
	//read request line
	s := ASocket.RecvString(timeout);
	{ WriteLn(s); }
	method := fetch(s, ' ');
	RequestURI := fetch(s, ' ');
	protocol := fetch(s, ' ');

	req := TRequest.Create;
	res := TResponse.Create; 

	//read request headers
	repeat
		s := ASocket.RecvString(Timeout);
		if s <> '' then begin
			SetLength(req.Headers, Length(req.Headers) + 1);
			req.Headers[Length(req.Headers) - 1] := s;
		end;
		//WriteLn(s);
	until s = '';
	s := ASocket.RecvPacket(500);
	req.DeriveArgs(AnsiReplaceStr(s, '+', ' '), req.PostForm);

	TempInt := Pos('?', RequestURI);
	if TempInt <> 0 then begin
		//WriteLn('Running req.DeriveGetArgs');
		req.DeriveArgs(Copy(RequestURI, TempInt + 1), req.Args);
		//WriteLn('done');
		uri := Copy(RequestURI, 1, TempInt - 1);
	end else
		uri := RequestURI;	

	if Routes.ContainsKey(uri) then
		RouteFunction := Routes.Items[uri]
	else 
		RouteFunction := DefaultHandler;
	
	req.uri := uri;
	req.method := method;
	req.protocol := protocol;
	
	RouteFunction(req, res);

	Write('[', Rfc822DateTime(now), ']  ');
	WriteLn(method, ' ', RequestURI, ' ', res.Status);
	ASocket.SendString('HTTP/1.1 ' + IntToStr(res.Status) + CRLF);
	WriteHeaders(ASocket, res.ResponseHeaders);

	ASocket.SendString(CRLF);
	
	ASocket.SendString(res.Body + CRLF);
	
	req.Destroy;
	res.Destroy;
end;

constructor TRequest.Create;
begin
	Args := TStringMap.Create;
	PostForm := TStringMap.Create;
	SetLength(Headers, 0);
end;

destructor TRequest.Destroy;
begin
	Args.Destroy;
	PostForm.Destroy;
end;

procedure TRequest.DeriveArgs(s : String; var Args : TStringMap);
var 
	i, j : LongInt;
	CurrKey, CurrVal : String;
begin

	i := 1;
	while i <= Length(s) do begin
		j := PosFrom('=', s, i);
		if j = 0 then exit;
		CurrKey := Copy(s, i, j - i);
		i := j + 1;
		j := PosFrom('&', s, i);
		if j = 0 then
		  	CurrVal := Copy(s, i)
		else
			CurrVal := Copy(s, i, j - i);
		Args.AddOrSetValue(ParseGetStrings(CurrKey), ParseGetStrings(CurrVal));
		if j = 0 then exit;
		i := j + 1;
	end;
end;

procedure TResponse.SetHeader(k, s : String);
begin
	ResponseHeaders.AddOrSetValue(k, s);
end;

constructor TResponse.Create;
begin
	ResponseHeaders := TStringMap.Create;
end;

destructor TResponse.Destroy;
begin
	ResponseHeaders.Destroy;
end;

procedure Handler404Default(req : TRequest; res : TResponse);
begin
	res.Status := 404;
	res.Body := 'The file at ' + req.uri + ' does not exist.';
end;

constructor TApp.Create;
begin
	DefaultHeaders := TStringMap.Create;
	DefaultHeaders.Add('Connection','close');
	DefaultHeaders.Add('Server','Pascal-Barrel using Synapse');
	
	Routes := TRoutesMap.Create;
	SetDefaultHandler(@Handler404Default);
end;

procedure TApp.AddRoute(s : String; Fxn : TRouteFunction);
begin
	Routes.Add(s, Fxn);
end;

procedure TApp.SetDefaultHandler(Fxn : TRouteFunction);
begin
	DefaultHandler := Fxn;
end;

procedure TApp.WriteHeaders(ASocket: TTCPBlockSocket; ResponseHeaders : TStringMap);
var 
	i : LongInt;
	TempArray : TArray<TPair<String,String>>;
begin
	TempArray := DefaultHeaders.ToArray;
	for i := 0 to Length(TempArray) - 1 do begin
		if not ResponseHeaders.ContainsKey(TempArray[i].Key) then begin
			ASocket.SendString(TempArray[i].Key + ': ' +  TempArray[i].Value + CRLF);
		end;
	end;
	TempArray := ResponseHeaders.ToArray;
	for i := 0 to Length(TempArray) - 1 do
		ASocket.SendString(TempArray[i].Key + ': ' +  TempArray[i].Value + CRLF);
	ASocket.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
end;

procedure TApp.SetDefaultHeader(k, h : String);
begin
	DefaultHeaders.Add(k, h);
end;

procedure TApp.Run(Host : String; ListenPort : Word);
var
  ListenerSocket, ConnectionSocket: TTCPBlockSocket;

begin
	ListenerSocket := TTCPBlockSocket.Create;
	ConnectionSocket := TTCPBlockSocket.Create;

	ListenerSocket.CreateSocket;
	ListenerSocket.setLinger(true,10);
	ListenerSocket.bind(Host,IntToStr(ListenPort));
	ListenerSocket.listen;

	WriteLn('Server running at ', Host, ':', ListenPort, '/');

	repeat
		if ListenerSocket.canread(1000) then begin
			ConnectionSocket.Socket := ListenerSocket.accept;
			
			//WriteLn('Attending Connection. Error code (0=Success): ', ConnectionSocket.lasterror);
			AttendConnection(ConnectionSocket);
			ConnectionSocket.CloseSocket;
		end;
	until false;

	ListenerSocket.Free;
	ConnectionSocket.Free;
end;

end.

{if uri = '/' then begin
			// Write the output document to the stream
			OutputDataString :=
				'<!DOCTYPE html><html><h1>Hello World!</h1></html>' + CRLF;

			// Write the headers back to the client
			ASocket.SendString('HTTP/1.0 200' + CRLF);
			ASocket.SendString('Content-type: Text/Html' + CRLF);
			ASocket.SendString('Content-length: ' + IntTostr(Length(OutputDataString)) + CRLF);
			ASocket.SendString('Connection: close' + CRLF);
			ASocket.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
			ASocket.SendString('Server: Pascal-Barrel using Synapse' + CRLF);
			ASocket.SendString('' + CRLF);

			//  if ASocket.lasterror <> 0 then HandleError;

			// Write the document back to the browser
			ASocket.SendString(OutputDataString);
		end;}