unit Barrel;
{$ifdef fpc}
  {$mode delphi}
  {$h+}
  {$m+}
{$endif}

{$apptype console}

interface
uses Classes, blcksock, sockets, Synautil, SysUtils, fgl, Generics.collections;

type
	TRequest = class
	public
		uri : String;
		method : String;
		protocol : String;
	end;
	TResponse = class
	private
		ResponseHeaderLength : LongInt;
		ResponseHeaders : Array of String;
		TextResponse : String;
	public
		constructor Create;
		destructor Destroy;
		procedure AddHeader(s : String);
	end;
	
	TRouteFunction = reference to procedure(req : TRequest; res : TResponse);
	TRoutesMap = TDictionary<String, TRouteFunction>;
	
	TApp = class
	private
		Routes : TRoutesMap;
		
		procedure AttendConnection(ASocket: TTCPBlockSocket);
	public
		DefaultHeaders : Array of String;
		
		constructor Create;
		procedure SetHeader(h : String);
		procedure Run(Host : String; ListenPort : Word);
	end;
	
implementation

{
  Attends a connection. Reads the headers and gives an
  appropriate response
}
procedure TApp.AttendConnection(ASocket: TTCPBlockSocket);
var
	timeout: integer;
	s: string;
	method, uri, protocol: String;
	OutputDataString: String;
	ResultCode: integer;
begin
	timeout := 120000;
	
	WriteLn('Received headers+document from browser:');
	
	//read request line
	s := ASocket.RecvString(timeout);
	WriteLn(s);
	method := fetch(s, ' ');
	uri := fetch(s, ' ');
	protocol := fetch(s, ' ');

	//read request headers
	repeat
		s := ASocket.RecvString(Timeout);
		WriteLn(s);
	until s = '';

	// Now write the document to the output stream
		
	if uri = '/' then begin
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
	end else
		ASocket.SendString('HTTP/1.0 404' + CRLF);
end;

procedure TResponse.AddHeader(s : String);
begin
	if (Length(ResponseHeaders) = ResponseHeaderLength) then
		SetLength(ResponseHeaders, ResponseHeaderLength * 2);
	ResponseHeaders[ResponseHeaderLength] := s;
	
	Inc(ResponseHeaderLength);
end;

constructor TResponse.Create;
begin
	ResponseHeaderLength := 0;
	SetLength(ResponseHeaders, 0);
end;

destructor TResponse.Destroy;
begin
	SetLength(ResponseHeaders, 0);
end;

constructor TApp.Create;
begin
	SetLength(DefaultHeaders, 1);
	DefaultHeaders[0] := 'Connection: close';
	
	Routes := TRoutesMap.Create;
end;

procedure TApp.SetHeader(h : String);
begin
	SetLength(DefaultHeaders, Length(DefaultHeaders) + 1);
	DefaultHeaders[Length(DefaultHeaders) - 1] := h;
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

	WriteLn('Server running at', Host, ':' ListenPort, '/');

	repeat
		if ListenerSocket.canread(1000) then begin
			ConnectionSocket.Socket := ListenerSocket.accept;
			
			WriteLn('Attending Connection. Error code (0=Success): ', ConnectionSocket.lasterror);
			AttendConnection(ConnectionSocket);
			ConnectionSocket.CloseSocket;
		end;
	until false;

	ListenerSocket.Free;
	ConnectionSocket.Free;
end;

end.