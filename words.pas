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

(* Operating words and cases *)

unit Words;

{$mode objfpc}{$H+}

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

function Multicase(const aWord: string): TMulticase;
var
  WordBase: string;
begin
  //WriteLnLog(AWord, IntToStr(Length(aWord)));
  Result[NOM] := aWord;
  if Copy(aWord, Length(aWord) - Length('а') + 1, Length('а')) = 'а' then
  begin
    //as femininum
    WordBase := Copy(aWord, 1, Length(aWord) - Length('а'));
    Result[PRE] := WordBase + 'е';
    Result[GEN] := WordBase + 'ы';
    Result[ACC] := WordBase + 'у';
    Result[DAT] := WordBase + 'е';
    Result[INS] := WordBase + 'ой';
    Result[VOC] := WordBase + 'о';
  end
  else
  if (Copy(aWord, Length(aWord) - Length('е') + 1, Length('е')) = 'е') then
  begin
    //as masculinum + jot + е
    WordBase := Copy(aWord, 1, Length(aWord) - Length('е'));
    Result[PRE] := WordBase + 'е';
    Result[GEN] := WordBase + 'я';
    Result[ACC] := WordBase + 'я';
    Result[DAT] := WordBase + 'ю';
    Result[INS] := WordBase + 'ем';
    Result[VOC] := WordBase + 'ё';
  end
  else
  if (Copy(aWord, Length(aWord) - Length('ё') + 1, Length('ё')) = 'ё') then
  begin
    //as masculinum + jot + е
    {$HINT todo}
    WordBase := Copy(aWord, 1, Length(aWord) - Length('ё'));
    Result[PRE] := WordBase + '';
    Result[GEN] := WordBase + '';
    Result[ACC] := WordBase + '';
    Result[DAT] := WordBase + '';
    Result[INS] := WordBase + '';
    Result[VOC] := WordBase + '';
  end
  else
  if (Copy(aWord, Length(aWord) - Length('ю') + 1, Length('ю')) = 'ю') then
  begin
    //as masculinum + jot + u
    {$HINT todo}
    WordBase := Copy(aWord, 1, Length(aWord) - Length('ю'));
    Result[PRE] := WordBase + '';
    Result[GEN] := WordBase + '';
    Result[ACC] := WordBase + '';
    Result[DAT] := WordBase + '';
    Result[INS] := WordBase + '';
    Result[VOC] := WordBase + '';
  end
  else
  if (Copy(aWord, Length(aWord) - Length('й') + 1, Length('й')) = 'й') then
  begin
    //as masculinum + jot
    WordBase := Copy(aWord, 1, Length(aWord) - Length('й'));
    Result[PRE] := WordBase + 'ем';
    Result[GEN] := WordBase + 'я';
    Result[ACC] := WordBase + 'я';
    Result[DAT] := WordBase + 'ю';
    Result[INS] := WordBase + 'ем';
    Result[VOC] := aWord;
  end
  else
  if (Copy(aWord, Length(aWord) - Length('и') + 1, Length('и')) = 'и') then
  begin
    //as masculinum + jot
    {$HINT todo}
    WordBase := Copy(aWord, 1, Length(aWord) - Length('и'));
    Result[PRE] := WordBase + '';
    Result[GEN] := WordBase + '';
    Result[ACC] := WordBase + '';
    Result[DAT] := WordBase + '';
    Result[INS] := WordBase + '';
    Result[VOC] := aWord;
  end
  else
  if (Copy(aWord, Length(aWord) - Length('ь') + 1, Length('ь')) = 'ь') then
  begin
    //as masculinum + jot
    {$HINT todo}
    WordBase := Copy(aWord, 1, Length(aWord) - Length('ь'));
    Result[PRE] := WordBase + '';
    Result[GEN] := WordBase + '';
    Result[ACC] := WordBase + '';
    Result[DAT] := WordBase + '';
    Result[INS] := WordBase + '';
    Result[VOC] := aWord;
  end
  else
  if (Copy(aWord, Length(aWord) - Length('ъ') + 1, Length('ъ')) = 'ъ') then
  begin
    //as masculinum + jot
    {$HINT todo}
    WordBase := Copy(aWord, 1, Length(aWord) - Length('ъ'));
    Result[PRE] := WordBase + '';
    Result[GEN] := WordBase + '';
    Result[ACC] := WordBase + '';
    Result[DAT] := WordBase + '';
    Result[INS] := WordBase + '';
    Result[VOC] := aWord;
  end
  else
  if (Copy(aWord, Length(aWord) - Length('о') + 1, Length('о')) = 'о') or
    (Copy(aWord, Length(aWord) - Length('у') + 1, Length('у')) = 'у') or
    (Copy(aWord, Length(aWord) - Length('ы') + 1, Length('ы')) = 'ы') then
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

