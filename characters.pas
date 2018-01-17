unit Characters;

{$mode objfpc}{$H+}

interface

uses
  Generics.Collections,
  Classes, SysUtils,
  Global, Time;

type
  TCase = (NOM, PRE, GEN, ACC, DAT, INS, VOC);
  TMulticase = array [TCase] of string;

type
  TChirality = (Felc, Girc);
  TGender = (Male, Female);
  TNationality = (Socia, Venada, Norda,
                  Nocta, Mediana, Magmata,
                  Scienta, Vitana, Somnia,

                  Transparenta, Talpa, Serpenta,
                  Aspecta, Arachna, Glacia,
                  Amphibia, Ichta, Electra);
  TID = cardinal;

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
    procedure WriteDebug;
  public
    { birth of a character }
    constructor Create(const aParent1, aParent2: OCharacter);
    constructor Create(const aNationality: TNationality);
    { completely random character }
    constructor Create;
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
  CastleLog;

function ChiralityToString(const aChirality: TChirality): string;
begin
  case aChirality of
    Felc: Result := 'Фэлк';
    Girc: Result := 'Гирк';
  end;
end;
function GenderToString(const aGender: TGender): string;
begin
  case aGender of
    Male: Result := 'Мужчина';
    Female: Result := 'Женщина';
  end;
end;
function NationalityToString(const aNationality: TNationality): string;
begin
  case aNationality of
    Socia       : Result := 'Социа';
    Venada      : Result := 'Венада';
    Norda       : Result := 'Норда';
    Nocta       : Result := 'Нокта';
    Mediana     : Result := 'Медиана';
    Magmata     : Result := 'Магмата';
    Scienta     : Result := 'Саента';
    Vitana      : Result := 'Витана';
    Somnia      : Result := 'Сомниа';
    Transparenta: Result := 'Траснпарэнта';
    Talpa       : Result := 'Талпа';
    Serpenta    : Result := 'Сэрпэнта';
    Aspecta     : Result := 'Аспекта';
    Arachna     : Result := 'Аракна';
    Glacia      : Result := 'Гляциа';
    Amphibia    : Result := 'Амфибиа';
    Ichta       : Result := 'Ихта';
    Electra     : Result := 'Электра';
  end;
end;
function MultiCaseToString(const aMulticase: TMultiCase; const aCase: TCase): string;
begin
  Result := aMulticase[aCase];
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
      29: Result := 'Лоз';
      30: Result := 'Ром';
      31: Result := 'Пир';
      32: Result := 'Рев';
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
    Name[NOM] := RandomRoot;
    Name[PRE] := RandomRoot+'е';
    Name[GEN] := RandomRoot+'а';
    Name[ACC] := RandomRoot+'а';
    Name[DAT] := RandomRoot+'у';
    Name[INS] := RandomRoot+'ом';
    Name[VOC] := RandomRoot+'э';
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
    Name[NOM] := RandomRoot+'а';
    Name[PRE] := RandomRoot+'е';
    Name[GEN] := RandomRoot+'ы';
    Name[ACC] := RandomRoot+'у';
    Name[DAT] := RandomRoot+'е';
    Name[INS] := RandomRoot+'ой';
    Name[VOC] := RandomRoot+'о';
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
  SetLength(Genes, GeneCount);
  isValid := true;
  Inc(GlobalID);
  ID := GlobalID;

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

constructor OCharacter.Create;
begin
  MakeRandomCharacter(RandomNationality);
end;

constructor OCharacter.Create(const aNationality: TNationality);
begin
  MakeRandomCharacter(aNationality);
end;

constructor OCharacter.Create(const aParent1, aParent2: OCharacter);
begin
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

  MakeName;

  //mix genes
end;

procedure OCharacter.Marry(const aSpouse: Ocharacter);
begin
  Spouse := aSpouse;
  Spouse.Spouse := Self;
  WriteLnLog(NationalityToString(Self.Nationality) + ' ' + MultiCaseToString(Self.Name, NOM) + ' вступил(а) в брак с ' + NationalityToString(Spouse.Nationality) + ' ' + MultiCaseToString(Spouse.Name, INS) + '  ' + TimeToString(Today));
end;

procedure OCharacter.Divorce;
begin
  WriteLnLog(MultiCaseToString(Self.Name, NOM) + ' и ' + MultiCaseToString(Spouse.Name, NOM) + ' более не состоят в браке ' + TimeToString(Today));
  Spouse.Spouse := nil;
  Spouse := nil;
end;

procedure OCharacter.WriteDebug;
  procedure W(s: string);
  begin
    WriteLnLog(IntToStr(Self.ID), s);
  end;
begin
  W(MultiCaseToString(Self.Name, NOM) + ' (сокращённо ' + MultiCaseToString(Self.ShortName, NOM) + ')');
  W('Хиральность: ' + ChiralityToString(Self.Chirality));
  W('Пол: ' + GenderToString(Self.Gender));
  W('Национальность: ' + NationalityToString(Self.Nationality));
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
  WriteLnLog(':::::::::::::::::::::::::','');
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

