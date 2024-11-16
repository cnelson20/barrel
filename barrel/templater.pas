unit Templater;
{$ifdef fpc}
  {$mode delphi}
  {$h+}
  {$m+}
{$endif}
interface
uses Sysutils, Generics.Collections;
type
    TStringMap = TDictionary<String,String>;

function LoadFile(Filename: String; IgnoreFirstHashes: Boolean = true): String;
function LoadFileAndCache(Filename : String): String;

function RunTemplateAndCache(Filename :  String; Params: TStringMap): String;

implementation
uses StrUtils, BarrelHelpers, Unix;

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

function LoadFile(Filename: String; IgnoreFirstHashes: Boolean = true): String;
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
            If (Length(Temp) = 0) or (Pos('# ', Temp) <> 1) then IgnoreFirstHashes := false;
			If not IgnoreFirstHashes then begin
				Str += Temp;
				Str += Chr($0A);
			end;
        end;
        Close(F);
        LoadFile := Str;
    end else
        LoadFile := '';
end;

function GetOrCacheTemplate(Filename: String): String;
var
    TempString : String;
begin
	if LoadFileCache.ContainsKey(Filename) then
        GetOrCacheTemplate := LoadFileCache.Items[Filename]  
    else begin
        TempString := LoadFile(Filename);
        LoadFileCache.Add(Filename, TempString);
        GetOrCacheTemplate := TempString;   
    end;
end;


function RunTemplateAndCache(Filename: String; Params: TStringMap): String;
var
    TemplateStr : String;
	TempFilename, TempFilename2 : String;
	
	StrIndex, EndOffset : LongInt;
	VarName: String;
begin
    TemplateStr := GetOrCacheTemplate(Filename);
	StrIndex := -1;
	while StrIndex <> 0 do begin
		StrIndex := indexOfOffset('$', TemplateStr, StrIndex + 1);
		if StrIndex = 0 then break;
		
		EndOffset := FindNotAlphaNumChar(TemplateStr, StrIndex + 1);
		VarName := Copy(TemplateStr, StrIndex + 1, EndOffset - StrIndex - 1);
		
		if Params.ContainsKey(VarName) then		
			TemplateStr := StuffString(TemplateStr, StrIndex, 1 + Length(VarName), Params.items[VarName])
		else
			WriteLn('Error: Params has no value for key "', VarName, '"');
		StrIndex := StrIndex + Length(Params.items[VarName]);
	end;
	
	RunTemplateAndCache := TemplateStr;
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
