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
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  C: OCharacter;
  i, j, k: integer;
  p1, p2: OCharacter;
begin
  Button1.Caption := 'Button' + Copy(Button1.Caption, Length(Button1.Caption), 1);

  for i := 0 to 100 do
  begin
    C := OCharacter.Create;
    Population.Add(C);
  end;

  for j := 0 to 20 do
  begin
    p1 := Population[Rnd.Random(Population.Count)];
    for k := 0 to Rnd.Random(4) do
    begin
      if (p1.Spouse = nil) or (p1.Chirality = Felc) then
      begin
        repeat
          p2 := Population[Rnd.Random(Population.Count)];
        until (p1 <> p2) and (p1.Chirality = p2.Chirality) and
              (p1.Gender <> p2.Gender);
        if p1.Chirality = Girc then
          p1.Marry(p2);
      end
      else
        p2 := p1.Spouse;


      C := OCharacter.Create(p1, p2);
      if C.isValid then begin
        Population.Add(C);
        C.WriteDebug;
      end
      else
        C.Free;
    end;
  end;
end;

end.

