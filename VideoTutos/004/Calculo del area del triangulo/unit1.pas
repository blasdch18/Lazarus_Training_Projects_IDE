unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { TForm1 }

  TForm1 = class(TForm)
    b_CalcularArea: TButton;
    fsp_altura: TFloatSpinEdit;
    fps_base: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1_Respuestas: TMemo;
    procedure CalculoArea(Sender: TObject);
    procedure Memo1_RespuestasChange(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  {variables globales}
  base, altura : Real;
  area : Real;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CalculoArea(Sender: TObject);
begin
  base := fps_base.Value;
  altura := fsp_altura.Value;
  {calcular area}
  area := (base*altura)/2;
  {mostrar datos}
  Memo1_Respuestas.Lines.Add('El area del triangulo es : ' + FloatToStr(area));

end;

procedure TForm1.Memo1_RespuestasChange(Sender: TObject);
begin

end;

end.

