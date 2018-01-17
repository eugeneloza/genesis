unit Global;

{$mode objfpc}{$H+}

interface

uses
  CastleRandom, CastleLog;

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

