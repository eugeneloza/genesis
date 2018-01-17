unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Characters, Global;

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

