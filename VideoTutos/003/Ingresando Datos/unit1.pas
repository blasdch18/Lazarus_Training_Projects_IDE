unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MostrarResultado: TLabel;
    procedure FloatSpinEdit1Change(Sender: TObject);
    procedure ProcesoSumarNumeros(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  a, b : Real;
  c : Real;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FloatSpinEdit1Change(Sender: TObject);
begin

end;

procedure TForm1.ProcesoSumarNumeros(Sender: TObject);
begin
  {tomar los numeros de las cajas de texto}
  a:= FloatSpinEdit1.Value;
  b:= FloatSpinEdit2.Value;
  {realizar suma}
  c:= a+b;
  {mostrar resultados}
  MostrarResultado.Caption:= 'Suma: ' + FloatToStr(c);


end;

end.

