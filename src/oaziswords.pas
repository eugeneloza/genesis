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

(* Operating words and cases *)

unit OazisWords;

{$INCLUDE compilerconfig.inc}

interface

{ Nominative case
  Prepositional case (говорить o ком – o чём)
  Genitive case (нет кого – чего)
  Accusative case (винить кого – что)
  Dative case (давать кому – чему)
  Instrumental case (орудовать кем - чем)}

type
  TCase = (NOM, PRE, GEN, ACC, DAT, INS, VOC, mul_);
  TMulticase = array [TCase] of string;

function Multicase(const aWord: string): TMulticase;
function MultiCaseToString(const aMulticase: TMultiCase; const aCase: TCase): string;
{ ugly converter for a word into case
  only for direct output of debug }
function WordCase(const aWord: string; aCase: TCase): string;
implementation

uses
  SysUtils, CastleLog;

function MultiCaseToString(const aMulticase: TMultiCase; const aCase: TCase): string;
begin
  Result := aMulticase[aCase];
end;

function isLastLetter(const aLetter, aString: string): boolean;
begin
  Result := Copy(aString, Length(aString) - Length(aLetter) + 1, Length(aLetter)) = aLetter;
end;
function CropLastLetter(const aLetter, aString: string): string;
begin
  Result := Copy(aString, 1, Length(aString) - Length(aLetter));
end;

function Multicase(const aWord: string): TMulticase;
var
  WordBase: string;
begin
  //WriteLnLog(AWord, IntToStr(Length(aWord)));
  Result[NOM] := aWord;
  if isLastLetter('а', aWord) then
  begin
    //as femininum
    WordBase := CropLastLetter('а', aWord);
    Result[PRE] := WordBase + 'е';
    Result[GEN] := WordBase + 'ы';
    Result[ACC] := WordBase + 'у';
    Result[DAT] := WordBase + 'е';
    Result[INS] := WordBase + 'ой';
    Result[VOC] := WordBase + 'о';
  end
  else
  if isLastLetter('я', aWord) then
  begin
    //as femininum
    WordBase := CropLastLetter('я', aWord);
    Result[PRE] := WordBase + 'е';
    Result[GEN] := WordBase + 'и';
    Result[ACC] := WordBase + 'ю';
    Result[DAT] := WordBase + 'е';
    Result[INS] := WordBase + 'ей';
    Result[VOC] := WordBase + 'ё';
  end
  else  if isLastLetter('е', aWord) then
  begin
    //as masculinum + jot + е
    WordBase := CropLastLetter('е', aWord);
    Result[PRE] := WordBase + 'е';
    Result[GEN] := WordBase + 'я';
    Result[ACC] := WordBase + 'я';
    Result[DAT] := WordBase + 'ю';
    Result[INS] := WordBase + 'ем';
    Result[VOC] := WordBase + 'о';
  end
  else
  if isLastLetter('ё', aWord) then
  begin
    //as masculinum + jot + е
    {$HINT todo}
    WordBase := CropLastLetter('ё', aWord);
    Result[PRE] := WordBase + '';
    Result[GEN] := WordBase + '';
    Result[ACC] := WordBase + '';
    Result[DAT] := WordBase + '';
    Result[INS] := WordBase + '';
    Result[VOC] := WordBase + '';
  end
  else
  if isLastLetter('ю', aWord) then
  begin
    //as masculinum + jot + u
    {$HINT todo}
    WordBase := CropLastLetter('ю', aWord);
    Result[PRE] := WordBase + '';
    Result[GEN] := WordBase + '';
    Result[ACC] := WordBase + '';
    Result[DAT] := WordBase + '';
    Result[INS] := WordBase + '';
    Result[VOC] := WordBase + '';
  end
  else
  if isLastLetter('й', aWord) then
  begin
    //as masculinum + jot
    WordBase := CropLastLetter('й', aWord);
    Result[PRE] := WordBase + 'ем';
    Result[GEN] := WordBase + 'я';
    Result[ACC] := WordBase + 'я';
    Result[DAT] := WordBase + 'ю';
    Result[INS] := WordBase + 'ем';
    Result[VOC] := aWord;
  end
  else
  if isLastLetter('и', aWord) then
  begin
    //as masculinum + jot
    {$HINT todo}
    WordBase := CropLastLetter('и', aWord);
    Result[PRE] := WordBase + '';
    Result[GEN] := WordBase + '';
    Result[ACC] := WordBase + '';
    Result[DAT] := WordBase + '';
    Result[INS] := WordBase + '';
    Result[VOC] := aWord;
  end
  else
  if isLastLetter('ь', aWord) then
  begin
    //as masculinum + jot
    {$HINT todo}
    WordBase := CropLastLetter('ь', aWord);
    Result[PRE] := WordBase + '';
    Result[GEN] := WordBase + '';
    Result[ACC] := WordBase + '';
    Result[DAT] := WordBase + '';
    Result[INS] := WordBase + '';
    Result[VOC] := aWord;
  end
  else
  if isLastLetter('ъ', aWord) then
  begin
    //as masculinum + jot
    {$HINT todo}
    WordBase := CropLastLetter('ъ', aWord);
    Result[PRE] := WordBase + '';
    Result[GEN] := WordBase + '';
    Result[ACC] := WordBase + '';
    Result[DAT] := WordBase + '';
    Result[INS] := WordBase + '';
    Result[VOC] := aWord;
  end
  else
  if (isLastLetter('о', aWord)) or
    (isLastLetter('у', aWord)) or
    (isLastLetter('ы', aWord)) then
  begin
    // doesn't change
    Result[PRE] := aWord;
    Result[GEN] := aWord;
    Result[ACC] := aWord;
    Result[DAT] := aWord;
    Result[INS] := aWord;
    Result[VOC] := aWord;
  end
  else
  begin
    //as pure masculinum
    Result[PRE] := aWord + 'е';
    Result[GEN] := aWord + 'а';
    Result[ACC] := aWord + 'а';
    Result[DAT] := aWord + 'у';
    Result[INS] := aWord + 'ом';
    Result[VOC] := aWord + 'э';
  end;
end;

function WordCase(const aWord: string; aCase: TCase): string;
begin
  Result := MultiCaseToString(Multicase(aWord),aCase);
end;

end.

