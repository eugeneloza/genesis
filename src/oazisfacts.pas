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

(* Facts are events in character's life *)

unit OazisFacts;

{$INCLUDE compilerconfig.inc}

interface

uses
  Generics.Collections,
  OazisWords, OazisGlobal, OazisTime;

type
  OFact = class(TObject)
  public
    Parent: TId;
    About: TIdList;
    Time: OTime;
    function Say: string; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
  end;
  OFactList = specialize TObjectList<OFact>;

type
  OBirthdayFact = class(OFact)
  public
    function Say: string; override;
    constructor Create(const aChild, aMother, aFather: TId; const aTime: OTime);
  end;

type
  OMeetFact = class(OFact)
  public
    function Say: string; override;
    constructor Create(const aChar1, aChar2: TId; const aTime: OTime);
  end;

implementation
uses
  OazisCharacters;

constructor OFact.Create;
begin
  //nothing to inherit
  About := TIdList.Create;
end;

destructor OFact.Destroy;
begin
  About.Free;
  inherited Destroy;
end;

function OBirthdayFact.Say: string;
begin
  case Rnd.Random(4) of
    0: Result := 'Я родился(ась) ' + TimeToString(Self.Time, GEN);
    1: Result := 'Мой день рождения ' + TimeToString(Self.Time, GEN);
    2: Result := TimeToString(Self.Time, GEN) + ' я появился(ась) на свет.';
    else Result := 'Моя история началась ' + TimeToString(Self.Time, GEN);
  end;
end;

constructor OBirthdayFact.Create(const aChild, aMother, aFather: TId; const aTime: OTime);
begin
  inherited Create;

  Parent := aChild;
  About.Add(aChild); //0
  About.Add(aMother);
  About.Add(aFather); //2
  Time := aTime;
end;

function OMeetFact.Say: string;
begin
  case Rnd.Random(2) of
    0: Result := 'Встретил(а) ' + MulticaseToString(GetCharacterById(About[1]).Name, ACC) + ' ' + TimeToString(Self.Time, GEN);
    else Result := 'Виделся(ась) с ' + MulticaseToString(GetCharacterById(About[1]).Name, ACC) + ' ' + TimeToString(Self.Time, GEN);
  end;
end;

constructor OMeetFact.Create(const aChar1, aChar2: TId; const aTime: OTime);
begin
  inherited Create;
  Parent := aChar1;
  About.Add(aChar1); //0
  About.Add(aChar2);
  Time := aTime;
end;


end.

