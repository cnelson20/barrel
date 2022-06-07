unit Templater;
{$ifdef fpc}
  {$mode delphi}
  {$h+}
  {$m+}
{$endif}
interface

function LoadFile(Filename : String): String;
function LoadFileAndCache(Filename : String): String;

implementation
uses Sysutils, Generics.Collections;

type
    TStringMap = TDictionary<String,String>;
var
    LoadFileCache : TStringMap;

function LoadFileAndCache(Filename : String): String;
var
    TempString : String;
begin
    if LoadFileCache.ContainsKey(Filename) then
        LoadFileAndCache := LoadFileCache.Items[Filename]  
    else begin
        TempString := LoadFile(Filename);
        LoadFileCache.Add(Filename, TempString);
        LoadFileAndCache := TempString;   
    end;
end;

function LoadFile(Filename : String): String;
var
    Str : String;
    Temp : String;
    F : TextFile;
begin
    if FileExists(Filename) then begin
        Assign(F, Filename);
        Reset(F);
        Str := '';
        while not Eof(F) do begin
            ReadLn(F, Temp);
            Str += Temp;
            Str += Chr($0A);
        end;
        Close(F);
        LoadFile := Str;
    end else 
        LoadFile := '';
end;

initialization
begin
    LoadFileCache := TStringMap.Create;
end;

finalization
begin
    LoadFileCache.Destroy;
end;

end.