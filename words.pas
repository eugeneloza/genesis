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
  TCase = (NOM, PRE, GEN, ACC, DAT, INS, VOC);
  TMulticase = array [TCase] of string;

function Multicase(const aWord: string): TMulticase;
implementation

uses
  SysUtils, CastleLog;

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
  if Copy(aWord, Length(aWord) - Length('й') + 1, Length('й')) = 'й' then
  begin
    //as masculinum + jot
    WordBase := Copy(aWord, 1, Length(aWord) - Length('й'));
    Result[PRE] := WordBase + 'е';
    Result[GEN] := WordBase + 'я';
    Result[ACC] := WordBase + 'я';
    Result[DAT] := WordBase + 'ю';
    Result[INS] := WordBase + 'ем';
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

end.

