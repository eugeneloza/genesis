unit Time;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  OTime = double;

var
  Today: OTime = 0;

function TimeToString(aTime: OTime): string;
implementation

function TimeToString(aTime: OTime): string;
begin
  Result := 'year ' + IntToStr(Round(aTime) div 350) + ' day ' + IntToStr(Round(aTime) mod 350);
end;


end.

