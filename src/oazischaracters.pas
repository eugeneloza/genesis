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

(* Description of living characters *)

unit OazisCharacters;

{$INCLUDE compilerconfig.inc}

interface

uses
  Generics.Collections,
  Classes, SysUtils,
  OazisGlobal, OazisTime, OazisWords, OazisFacts;

type
  TChirality = (Felc, Girc);
  TGender = (Male, Female);
  TNationality = (Socia, Venada, Norda,
                  Nocta, Mediana, Magmata,
                  Scienta, Vitana, Somnia,

                  Transparenta, Talpa, Serpenta,
                  Aspecta, Arachna, Glacia,
                  Amphibia, Ichta, Electra);

const
  GircNationality = [Socia, Venada, Norda,
           Nocta, Mediana, Magmata,
           Scienta, Vitana, Somnia];
  FelcNationality = [Transparenta, Talpa, Serpenta,
           Aspecta, Arachna, Glacia,
           Amphibia, Ichta, Electra];
  {these nationalities do not appear regularly in the world}
  RareNationality = [Magmata, Glacia, Ichta, Electra];

{type
  TGeneType = (BoolGene, FloatGene);}

type
  OGeneInfo = record
    GeneName: string;
    //GeneType: TGeneType;
    GeneMaxValue: integer;
  end;

type
  OGene = record
    Value: integer;
    { source character of the gene (0 if random)
      only the character at which the gene was picked is listed,
      e.g. if the gene is mother's, then it'll list mother's ID,
      not grandmother's (whose this gene originates from)
      Even if the gene coincides with one of direct parent's genes,
      but was picked from different source, the source is listed }
    Source: TID;
  end;

type OCharacter = class(TObject)
  private
    const
      GeneCount = 10;
    class var
      GeneInfo: array of OGeneInfo;
    class procedure InitGenes;
  private
    Genes: array of OGene;
    procedure MakeName;
    function RandomNationality: TNationality;
    procedure MakeRandomCharacter(const aNationality: TNationality);
  public
    isValid: boolean;
    ID: TID;
    Name, ShortName: TMulticase;
    //birthday, deathday: OTime;
    Chirality: TChirality;
    Gender: TGender;
    Nationality: TNationality;
    Father, Mother: OCharacter;
    Spouse: OCharacter;
    procedure Marry(const aSpouse: Ocharacter);
    procedure Divorce;
    procedure Birth;
    procedure WriteDebug;
  public
    Facts: OFactList;
  strict private
    procedure Init;
  public
    { birth of a character }
    constructor Create(const aParent1, aParent2: OCharacter);
    constructor Create(const aNationality: TNationality);
    { completely random character }
    constructor Create;
    destructor Destroy; override;
end;

type
  TNameDictionary = specialize TDictionary<string, string>;
  TCharacterList = specialize TObjectList<OCharacter>;

var
  GlobalID: TID = 0;

  Population: TCharacterList;

  NameSpace: TNameDictionary;


implementation
uses
  OazisMain;

function ChiralityToString(const aChirality: TChirality): string;
begin
  case aChirality of
    Felc: Result := 'Фэлк';
    Girc: Result := 'Гирк';
    else Result := 'ERROR';
  end;
end;
function GenderToString(const aGender: TGender): string;
begin
  case aGender of
    Male: Result := 'Мужчина';
    Female: Result := 'Женщина';
    else Result := 'ERROR';
  end;
end;
function NationalityToMulticase(const aNationality: TNationality): TMulticase;
begin
  case aNationality of
    Socia       : Result := Multicase('Социа');
    Venada      : Result := Multicase('Венада');
    Norda       : Result := Multicase('Норда');
    Nocta       : Result := Multicase('Нокта');
    Mediana     : Result := Multicase('Медиана');
    Magmata     : Result := Multicase('Магмата');
    Scienta     : Result := Multicase('Саента');
    Vitana      : Result := Multicase('Витана');
    Somnia      : Result := Multicase('Сомниа');
    Transparenta: Result := Multicase('Траснпарэнта');
    Talpa       : Result := Multicase('Талпа');
    Serpenta    : Result := Multicase('Сэрпэнта');
    Aspecta     : Result := Multicase('Аспекта');
    Arachna     : Result := Multicase('Аракна');
    Glacia      : Result := Multicase('Гляциа');
    Amphibia    : Result := Multicase('Амфибиа');
    Ichta       : Result := Multicase('Ихта');
    Electra     : Result := Multicase('Электра');
    else Result := Multicase('ERROR');
  end;
end;

procedure OCharacter.MakeName;
  function GetFirstSyllable: string;
  begin
    case Rnd.Random(32+1) of
       0: Result := 'Ар';
       1: Result := 'Эл';
       2: Result := 'Фан';
       3: Result := 'Ен';
       4: Result := 'Бас';
       5: Result := 'Кал';
       6: Result := 'Рам';
       7: Result := 'Дот';
       8: Result := 'Сим';
       9: Result := 'Ран';
      10: Result := 'Кор';
      11: Result := 'Дис';
      12: Result := 'Вил';
      13: Result := 'Пол';
      14: Result := 'Эр';
      15: Result := 'Ом';
      16: Result := 'Ак';
      17: Result := 'Пан';
      18: Result := 'Вог';
      19: Result := 'Ям';
      20: Result := 'Кит';
      21: Result := 'Рес';
      22: Result := 'Бат';
      23: Result := 'Им';
      24: Result := 'Ол';
      25: Result := 'Аг';
      26: Result := 'Ах';
      27: Result := 'Як';
      28: Result := 'Вер';
      29: Result := 'Лов';
      30: Result := 'Ром';
      31: Result := 'Пир';
      32: Result := 'Рев';
      else Result := 'ERROR';
    end;
  end;
  function GetMidSyllable: string;
  begin
    case Rnd.Random(32+1) of
       0: Result := 'ас';
       1: Result := 'ор';
       2: Result := 'ис';
       3: Result := 'ам';
       4: Result := 'эль';
       5: Result := 'од';
       6: Result := 'ав';
       7: Result := 'эс';
       8: Result := 'ан';
       9: Result := 'эв';
      10: Result := 'ап';
      11: Result := 'ат';
      12: Result := 'от';
      13: Result := 'арг';
      14: Result := 'ол';
      15: Result := 'аф';
      16: Result := 'ох';
      17: Result := 'ам';
      18: Result := 'ог';
      19: Result := 'эт';
      20: Result := 'од';
      21: Result := 'ин';
      22: Result := 'ах';
      23: Result := 'эк';
      24: Result := 'ур';
      25: Result := 'арг';
      26: Result := 'аг';
      27: Result := 'аш';
      28: Result := 'ор';
      29: Result := 'аф';
      30: Result := 'эф';
      31: Result := 'ил';
      32: Result := 'эр';
      else Result := 'ERROR';
    end;
  end;
var
  i: integer;
  RandomRoot: string;
  FirstSyllable: string;
begin
  //based on Chirality, Nationality and Gender
  repeat
    FirstSyllable := GetFirstSyllable;
    RandomRoot := FirstSyllable;
    for i := 0 to Rnd.Random(2) do
      RandomRoot := RandomRoot + GetMidSyllable;
  until not NameSpace.ContainsValue(RandomRoot);
  NameSpace.Add(RandomRoot,RandomRoot);

  if Self.Gender = Male then
  begin
    Name := Multicase(RandomRoot);
    ShortName[NOM] := FirstSyllable+'у';
    ShortName[PRE] := FirstSyllable+'е';
    ShortName[GEN] := FirstSyllable+'а';
    ShortName[ACC] := FirstSyllable+'у';
    ShortName[DAT] := FirstSyllable+'у';
    ShortName[INS] := FirstSyllable+'ом';
    ShortName[VOC] := FirstSyllable+'у';
  end
  else
  begin
    RandomRoot := RandomRoot + 'а';
    Name := Multicase(RandomRoot);
    ShortName[NOM] := FirstSyllable+'и';
    ShortName[PRE] := FirstSyllable+'е';
    ShortName[GEN] := FirstSyllable+'и';
    ShortName[ACC] := FirstSyllable+'у';
    ShortName[DAT] := FirstSyllable+'е';
    ShortName[INS] := FirstSyllable+'ой';
    ShortName[VOC] := FirstSyllable+'и';
  end;

end;
function OCharacter.RandomNationality: TNationality;
begin
  //get nationality based on chirality
  repeat
    Result := specialize RandomEnum<TNationality>;
  until not (Result in RareNationality);
end;

procedure OCharacter.MakeRandomCharacter(const aNationality: TNationality);
begin

  Gender := specialize RandomEnum<TGender>;
  Nationality := aNationality;
  if Nationality in FelcNationality then  //this is redundant, but I don't care :D
    Chirality := Felc
  else
    Chirality := Girc;

  Father := nil; //no mother and no father, poor lad...
  Mother := nil;

  MakeName;

end;

procedure OCharacter.Init;
begin
  Facts := OFactList.Create(True);
  SetLength(Genes, GeneCount);
  isValid := true;
  Inc(GlobalID);
  ID := GlobalID;
end;

constructor OCharacter.Create;
begin
  Init;
  MakeRandomCharacter(RandomNationality);
end;

constructor OCharacter.Create(const aNationality: TNationality);
begin
  Init;
  MakeRandomCharacter(aNationality);
end;

constructor OCharacter.Create(const aParent1, aParent2: OCharacter);
begin
  Init;
  isValid := true;
  Inc(GlobalID);
  ID := GlobalID;

  if aParent1.Gender = Male then
  begin
    Father := aParent1;
    Mother := aParent2;
  end
  else
  begin
    Father := aParent2;
    Mother := aParent1;
  end;

  Chirality := Mother.Chirality;
  Gender := specialize RandomEnum<TGender>;
  if Rnd.RandomBoolean then
    Nationality := Father.Nationality
  else
    Nationality := Mother.Nationality;

  {fool's check}
  if (Father.Gender <> Male) or (Mother.Gender <> Female) then
    isValid := false;
  {secret check}
  if (Nationality in FelcNationality) and (Mother.Chirality = Girc) then
    Nationality := Mother.Nationality;
  if (Nationality in GircNationality) and (Mother.Chirality = Felc) then
    Nationality := Mother.Nationality;


  Birth;   //will happen later
end;

procedure OCharacter.Birth;
begin
  MakeName;
  Facts.Add(OBirthdayFact.Create(Self.Id, Self.Father.Id, Self.Mother.Id, Today));
end;

destructor OCharacter.Destroy;
begin
  Facts.Free;
  inherited Destroy;
end;

procedure OCharacter.Marry(const aSpouse: Ocharacter);
var
  sstring: string;
begin
  Spouse := aSpouse;
  Spouse.Spouse := Self;
  if Self.Gender = Male then
    sstring := ' вступил '
  else
    sstring := ' вступила ';
  Form1.Memo1.Lines.Add(MultiCaseToString(NationalityToMulticase(Self.Nationality), NOM) +
    ' ' + MultiCaseToString(Self.Name, NOM) +
    sstring + 'в брак с ' +
    MultiCaseToString(NationalityToMulticase(Spouse.Nationality), INS) +
    ' ' + MultiCaseToString(Spouse.Name, INS) +
    ' - ' + TimeToString(Today, GEN));
end;

procedure OCharacter.Divorce;
begin
  Form1.Memo1.LInes.Add(MultiCaseToString(Self.Name, NOM) + ' и ' + MultiCaseToString(Spouse.Name, NOM) + ' более не состоят в браке ' + TimeToString(Today, GEN));
  Spouse.Spouse := nil;
  Spouse := nil;
end;

procedure OCharacter.WriteDebug;
  procedure W(s: string);
  begin
    Form1.Memo1.Lines.Add(IntToStr(Self.ID) + ':' + s);
  end;
var
  f: OFact;
begin
  W(MultiCaseToString(Self.Name, NOM) + ' (просто ' + MultiCaseToString(Self.ShortName, NOM) + ')');
  W('Хиральность: ' + ChiralityToString(Self.Chirality));
  W('Пол: ' + GenderToString(Self.Gender));
  W('Национальность: ' + MulticaseToString(NationalityToMulticase(Self.Nationality), NOM));

  for f in Facts do
    Form1.Memo1.Lines.Add(f.Say);

  if (Self.Father <> nil) and (Self.Mother <> nil) then begin
    if Self.Chirality = Girc then
    begin
      case Self.Gender of
        Male: W('Сын ' + MultiCaseToString(Self.Father.Name, GEN) + ' и ' + MultiCaseToString(Self.Mother.Name, GEN));
        Female: W('Дочь ' + MultiCaseToString(Self.Father.Name, GEN) + ' и ' + MultiCaseToString(Self.Mother.Name, GEN));
      end;
    end
    else
    begin
      case Self.Gender of
        Male: W('Сын ' + MultiCaseToString(Self.Mother.Name, GEN));
        Female: W('Дочь ' + MultiCaseToString(Self.Mother.Name, GEN));
      end;
    end;
  end
  else
    W('Родители неизвестны');
  Form1.Memo1.Lines.Add(':::::::::::::::::::::::::');
end;

class procedure OCharacter.InitGenes;
begin
  SetLength(GeneInfo, GeneCount);
  with GeneInfo[0] do begin
    GeneName := 'Глаза';
    GeneMaxValue := 10;
  end;
  with GeneInfo[0] do begin
    GeneName := 'Нос';
    GeneMaxValue := 10;
  end;
end;

initialization
  NameSpace := TNameDictionary.Create;
  Population := TCharacterList.Create(true);

finalization
  NameSpace.Free;
  Population.Free;


end.

