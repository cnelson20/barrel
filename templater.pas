unit Templater;
{$h+}
interface

function LoadFile(Filename : String): String;

implementation
uses Sysutils;

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

end.