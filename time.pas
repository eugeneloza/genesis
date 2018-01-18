{Copyright (C) 2012-2018 Yevhen Loza

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.}

{---------------------------------------------------------------------------}

(* Time routines, including converting time to string *)

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
  Result := 'год ' + IntToStr(Round(aTime) div 350) + ' день ' + IntToStr(Round(aTime) mod 350);
end;


end.

