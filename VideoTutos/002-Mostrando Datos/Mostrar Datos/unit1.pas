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
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);

  private

  public

  end;

var
  Form1: TForm1;
  a : Integer;
  b : Integer;
  c : Integer;

  f : Real;
  x : Real;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  {Vamos de a definir vaalores en las variables}
  a := 50;
  b := 43;

  {dar un valor a x}
  x := 0.001;
  {hacer el caclulo de la funcion x^2+2x+1}
  f := x*x+2*x+1;

  {realizar la suma}
  c := a + b;
  {mostrar resultado}
  Label1.Caption := 'El resultado es: '+ IntToStr(c);
  Label2.Caption := 'El resultado de la f(x) = ' + FloatToStr(f);
end;

end.

