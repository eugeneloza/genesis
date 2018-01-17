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
var C1, C2, C3: OCharacter;
begin
  C1 := OCharacter.Create;
  C2 := OCharacter.Create;
  C3 := OCharacter.Create(C1, C2);

  C1.WriteDebug;
  C2.WriteDebug;
  C3.WriteDebug;

  C1.Free;
  C2.Free;
  C3.Free;
end;

end.

