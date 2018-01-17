unit Characters;

{$mode objfpc}{$H+}

interface

uses
  Generics.Collections,
  Classes, SysUtils,
  Global;

type
  TMulticase = string;
  TCase = (NOM, PRE, GEN, ACC, DAT, INS, VOC);

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
  TGeneType = (BoolGene, FloatGene);

const
  GircNationality = [Socia, Venada, Norda,
           Nocta, Mediana, Magmata,
           Scienta, Vitana, Somnia];
  FelcNationality = [Transparenta, Talpa, Serpenta,
           Aspecta, Arachna, Glacia,
           Amphibia, Ichta, Electra];
  {these nationalities do not appear regularly in the world}
  RareNationality = [Magmata, Glacia, Ichta, Electra];

type
  OGeneType = record
    GeneName: string;
  end;

type
  OGene = record
    Value: OFloat;
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
    function MakeName: TMulticase;
    function RandomNationality: TNationality;
    procedure MakeRandomCharacter(const aNationality: TNationality);
  public
    ID: TID;
    Name: TMulticase;
    //birthday, deathday: OTime;
    Chirality: TChirality;
    Gender: TGender;
    Nationality: TNationality;
    procedure WriteDebug;
  public
    { birth of a character }
    constructor Create(const Father, Mother: OCharacter);
    constructor Create(const aNationality: TNationality);
    { completely random character }
    constructor Create;
end;

var
  GlobalID: TID = 0;


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
    Transparenta: Result := 'Транспарэнта';
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
  Result := aMulticase;
end;

function OCharacter.MakeName: TMulticase;
begin
  //based on Chirality, Nationality and Gender
  Result := 'Имя-' + IntToStr(Self.ID);
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
  Inc(GlobalID);
  ID := GlobalID;

  Gender := specialize RandomEnum<TGender>;
  Nationality := aNationality;
  if Nationality in FelcNationality then  //this is redundant, but I don't care :D
    Chirality := Felc
  else
    Chirality := Girc;

  Name := MakeName;

end;

constructor OCharacter.Create;
begin
  MakeRandomCharacter(RandomNationality);
end;

constructor OCharacter.Create(const aNationality: TNationality);
begin
  MakeRandomCharacter(aNationality);
end;

constructor OCharacter.Create(const Father, Mother: OCharacter);
begin
//  Inc(GlobalID)
//  ID := GlobalID;
  Create;
  Chirality := TChirality(Rnd.Random(Ord(High(TChirality))-Ord(Low(TChirality))));
  Name := MakeName;
end;

procedure OCharacter.WriteDebug;
begin
  WriteLnLog(IntToStr(Self.ID),MultiCaseToString(Self.Name, NOM));
  WriteLnLog(IntToStr(Self.ID),ChiralityToString(Self.Chirality));
  WriteLnLog(IntToStr(Self.ID),GenderToString(Self.Gender));
  WriteLnLog(IntToStr(Self.ID),NationalityToString(Self.Nationality));
  WriteLnLog(':::::::::::::::::::::::::','');
end;


end.

