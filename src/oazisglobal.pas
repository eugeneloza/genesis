{Copyright (C) 2017-2018 Yevhen Loza

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

(* Global variables and components *)

unit OazisGlobal;

{$INCLUDE compilerconfig.inc}

interface

uses
  CastleRandom, CastleLog, Generics.Collections;

type
  { ID of any entity in-game, this may be character, creature, location, item,
    anything }
  TId = cardinal;
  TIdList = specialize TList<TId>;

type
  OFloat = single;

var Rnd: TCastleRandom;

generic function RandomEnum<T>: T;
implementation

generic function RandomEnum<T>: T;
begin
  Result := T(Rnd.Random(Ord(High(T)) - Ord(Low(T)) + 1));
end;

initialization
  InitializeLog;
  Rnd := TCastleRandom.Create;

finalization
  Rnd.Free;

end.

