unit BarrelHelpers;
{$ifdef fpc}
  {$mode delphi}
  {$h+}
  {$m+}
{$endif}
interface
uses Sysutils, Generics.Collections;

function IsAlphaNumChar(c: Char): Boolean;

function IndexOfOffset(c: Char; s: String; offset: LongInt): LongInt;
function FindNotAlphaNumChar(s: String; offset: LongInt): LongInt;
procedure SaveString(InString, OutFilePath: string);

implementation

function IndexOfOffset(c: Char; s: String; offset: LongInt): LongInt;
var
	ind : LongInt;
begin
	indexOfOffset := 0;
	
	ind := offset;
	while ind <= Length(s) do begin
		if c = s[ind] then begin
			indexOfOffset := ind;
			break;
		end;
		ind := ind + 1;
	end;
end;

function IsAlphaNumChar(c: Char): Boolean;
var
	OrdVal : LongInt;
begin
	IsAlphaNumChar := false;

	OrdVal := Ord(c);
	if (OrdVal >= $30) and (OrdVal < $39 + 1) then IsAlphaNumChar := true;
	if (OrdVal >= Ord('A')) and (OrdVal < Ord('Z') + 1) then IsAlphaNumChar := true;
	if (OrdVal >= Ord('a')) and (OrdVal < Ord('z') + 1) then IsAlphaNumChar := true;
end;

function FindNotAlphaNumChar(s: String; offset: LongInt): LongInt;
var
	ind : LongInt;
begin
	FindNotAlphaNumChar := 0;
	
	ind := offset;
	while ind <= Length(s) do begin
		if not IsAlphaNumChar(s[ind]) then begin
			FindNotAlphaNumChar := ind;
			break;
		end;
		ind := ind + 1;
	end;
end;

procedure SaveString(InString, OutFilePath: string);
  var
    F: TextFile;
  begin
    AssignFile(F, OutFilePath);
    try
      ReWrite(F);
      Write(F, InString);
    finally
      CloseFile(F);
    end;
  end;

end.