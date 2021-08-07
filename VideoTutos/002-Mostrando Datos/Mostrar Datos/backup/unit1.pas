unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);

  private

  public

  end;

var
  Form1: TForm1;
  a : Integer;
  b : Integer;
  c : Integer;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  {Vamos de a definir vaalores en las variables}
  a := 8;
  b := 5;
  {realizar la suma}
  c := a + b;
  Label1.Caption := 'El resultado es: '+ IntToStr(c);
end;

end.

