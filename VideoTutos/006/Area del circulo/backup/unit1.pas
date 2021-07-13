unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button_Calcular: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Spin_Datos_ingreso: TSpinEdit;
    procedure Button_CalcularClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  radio, area, perimetro : Real;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button_CalcularClick(Sender: TObject);
begin
  radio := Spin_Datos_ingreso.Value;
  {area}
  area := pi * power( radio, 2);
  {perimetro}
  perimetro := pi * 2 * radio;
  {imprimir datos en memobox}
  Memo1.Lines.Add(' Area = ' + FloatToStr(area) +
                 ', perimetro = ' + FloatToStr(perimetro));

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

