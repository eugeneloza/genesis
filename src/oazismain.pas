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

(* Main file *)

unit OazisMain;

{$INCLUDE compilerconfig.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  OazisCharacters, OazisGlobal;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation
uses
  CastleLog,
  OazisTime, OazisWords;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
const
  StepsToModel = 1000;
  DeltaTime = 1; //one day step
var
  C: OCharacter;
  i, j, k: integer;
  RandomCharacter: integer;
  p1, p2: OCharacter;
begin
  //seed the world
  for i := 0 to 100 do
  begin
    C := OCharacter.Create;
    Population.Add(C.Id, C);
  end;

  for j := 0 to StepsToModel do
  begin

    doTime(DeltaTime); //skip 1 day
    Memo1.Lines.Add('Сегодня ' + TimeToString(Today, NOM));

    for p1 in Population.Values do
    if p1.isAlive then
    begin
      //p1 has up to 10 social interactions
      for k := 0 to Rnd.Random(10) do
      begin
        RandomCharacter := Rnd.Random(Population.Count) + 1;
        //WriteLnLog(IntToStr(RandomCharacter));
        p2 := Population.Items[RandomCharacter];
        if (p2.ID <> p1.ID) and (p2.isAlive) then
        begin
          p1.Meet(p2.ID)
        end;
      end;
    end;
  end;

  {for j := 0 to 20 do
  begin
    p1 := Population.Items[];
    for k := 0 to Rnd.Random(4) do
    begin
      if (p1.Spouse = nil) or (p1.Chirality = Felc) then
      begin
        repeat
          p2 :=
        until (p1 <> p2) and (p1.Chirality = p2.Chirality) and
              (p1.Gender <> p2.Gender);
        if p1.Chirality = Girc then
          p1.Marry(p2);
      end
      else
        p2 := p1.Spouse;


      C := OCharacter.Create(p1, p2);
      if C.isValid then begin
        Population.Add(C.Id, C);
        C.WriteDebug;
      end
      else
        C.Free;
    end;
  end;}
end;

end.

