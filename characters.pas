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
    Felc: Result := 'Felc';
    Girc: Result := 'Girc';
  end;
end;
function GenderToString(const aGender: TGender): string;
begin
  case aGender of
    Male: Result := 'Male';
    Female: Result := 'Female';
  end;
end;
function NationalityToString(const aNationality: TNationality): string;
begin
  case aNationality of
    Socia       : Result := 'Socia';
    Venada      : Result := 'Venada';
    Norda       : Result := 'Norda';
    Nocta       : Result := 'Nocta';
    Mediana     : Result := 'Mediana';
    Magmata     : Result := 'Magmata';
    Scienta     : Result := 'Scienta';
    Vitana      : Result := 'Vitana';
    Somnia      : Result := 'Somnia';
    Transparenta: Result := 'Transparenta';
    Talpa       : Result := 'Talpa';
    Serpenta    : Result := 'Serpenta';
    Aspecta     : Result := 'Aspecta';
    Arachna     : Result := 'Arachna';
    Glacia      : Result := 'Glacia';
    Amphibia    : Result := 'Amphibia';
    Ichta       : Result := 'Ichta';
    Electra     : Result := 'Electra';
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
       0: Result := 'Ar';
       1: Result := 'Ell';
       2: Result := 'Fan';
       3: Result := 'Yen';
       4: Result := 'Bas';
       5: Result := 'Cal';
       6: Result := 'Ram';
       7: Result := 'Dot';
       8: Result := 'Sim';
       9: Result := 'Ran';
      10: Result := 'Cor';
      11: Result := 'Dis';
      12: Result := 'Vil';
      13: Result := 'Pol';
      14: Result := 'Er';
      15: Result := 'Om';
      16: Result := 'Ak';
      17: Result := 'Pan';
      18: Result := 'Wog';
      19: Result := 'Yam';
      20: Result := 'Kit';
      21: Result := 'Res';
      22: Result := 'But';
      23: Result := 'Im';
      24: Result := 'Ol';
      25: Result := 'Ag';
      26: Result := 'Ax';
      27: Result := 'Jaq';
      28: Result := 'Ver';
      29: Result := 'Loz';
      30: Result := 'Rom';
      31: Result := 'Pyr';
      32: Result := 'Rew';
    end;
  end;
  function GetMidSyllable: string;
  begin
    case Rnd.Random(32+1) of
       0: Result := 'ac';
       1: Result := 'or';
       2: Result := 'is';
       3: Result := 'am';
       4: Result := 'el';
       5: Result := 'od';
       6: Result := 'av';
       7: Result := 'es';
       8: Result := 'un';
       9: Result := 'ew';
      10: Result := 'up';
      11: Result := 'at';
      12: Result := 'ot';
      13: Result := 'arg';
      14: Result := 'ol';
      15: Result := 'aj';
      16: Result := 'oz';
      17: Result := 'um';
      18: Result := 'og';
      19: Result := 'et';
      20: Result := 'od';
      21: Result := 'in';
      22: Result := 'ax';
      23: Result := 'ek';
      24: Result := 'urq';
      25: Result := 'arq';
      26: Result := 'ar';
      27: Result := 'as';
      28: Result := 'op';
      29: Result := 'af';
      30: Result := 'ef';
      31: Result := 'il';
      32: Result := 'er';
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
    Name[PRE] := RandomRoot+'a';
    Name[GEN] := RandomRoot+'a';
    Name[ACC] := RandomRoot+'a';
    Name[DAT] := RandomRoot+'u';
    Name[INS] := RandomRoot+'om';
    Name[VOC] := RandomRoot+'e';
    ShortName[NOM] := FirstSyllable+'y';
    ShortName[PRE] := FirstSyllable+'e';
    ShortName[GEN] := FirstSyllable+'a';
    ShortName[ACC] := FirstSyllable+'y';
    ShortName[DAT] := FirstSyllable+'u';
    ShortName[INS] := FirstSyllable+'om';
    ShortName[VOC] := FirstSyllable+'y';
  end
  else
  begin
    Name[NOM] := RandomRoot+'a';
    Name[PRE] := RandomRoot+'u';
    Name[GEN] := RandomRoot+'y';
    Name[ACC] := RandomRoot+'u';
    Name[DAT] := RandomRoot+'e';
    Name[INS] := RandomRoot+'oy';
    Name[VOC] := RandomRoot+'o';
    ShortName[NOM] := FirstSyllable+'i';
    ShortName[PRE] := FirstSyllable+'e';
    ShortName[GEN] := FirstSyllable+'i';
    ShortName[ACC] := FirstSyllable+'i';
    ShortName[DAT] := FirstSyllable+'e';
    ShortName[INS] := FirstSyllable+'oj';
    ShortName[VOC] := FirstSyllable+'i';
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
  WriteLnLog(NationalityToString(Self.Nationality) + ' ' + MultiCaseToString(Self.Name, NOM) + ' married ' + NationalityToString(Spouse.Nationality) + ' ' + MultiCaseToString(Spouse.Name, ACC) + ' on ' + TimeToString(Today));
end;

procedure OCharacter.Divorce;
begin
  WriteLnLog(MultiCaseToString(Self.Name, NOM) + ' and ' + MultiCaseToString(Spouse.Name, NOM) + ' are no longer married on ' + TimeToString(Today));
  Spouse.Spouse := nil;
  Spouse := nil;
end;

procedure OCharacter.WriteDebug;
  procedure W(s: string);
  begin
    WriteLnLog(IntToStr(Self.ID), s);
  end;
begin
  W(MultiCaseToString(Self.Name, NOM) + ' (aka ' + MultiCaseToString(Self.ShortName, NOM) + ')');
  W('Chirality: ' + ChiralityToString(Self.Chirality));
  W('Gender: ' + GenderToString(Self.Gender));
  W('Nationality: ' + NationalityToString(Self.Nationality));
  if (Self.Father <> nil) and (Self.Mother <> nil) then begin
    if Self.Chirality = Girc then
    begin
      case Self.Gender of
        Male: W('Son of ' + MultiCaseToString(Self.Father.Name, GEN) + ' and ' + MultiCaseToString(Self.Mother.Name, GEN));
        Female: W('Daughter of ' + MultiCaseToString(Self.Father.Name, GEN) + ' and ' + MultiCaseToString(Self.Mother.Name, GEN));
      end;
    end
    else
    begin
      case Self.Gender of
        Male: W('Son of ' + MultiCaseToString(Self.Mother.Name, GEN));
        Female: W('Daughter of ' + MultiCaseToString(Self.Mother.Name, GEN));
      end;
    end;
  end
  else
    W('Parents unknown');
  WriteLnLog(':::::::::::::::::::::::::','');
end;

class procedure OCharacter.InitGenes;
begin
  SetLength(GeneInfo, GeneCount);
  with GeneInfo[0] do begin
    GeneName := 'Eyes';
    GeneMaxValue := 10;
  end;
  with GeneInfo[0] do begin
    GeneName := 'Nose';
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

