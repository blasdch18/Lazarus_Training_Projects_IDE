unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, ComCtrls, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button_calcular: TButton;
    ImageList1: TImageList;
    tv_resultados: TTreeView;
    ValorC: TGroupBox;
    ValorB: TGroupBox;
    valor1: TFloatSpinEdit;
    ValorA: TGroupBox;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    valor2: TFloatSpinEdit;
    valor3: TFloatSpinEdit;
    procedure CalcularSolucion(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CalcularSolucion(Sender: TObject);
var
  x1, x2, a, b, c : Real;
  disc : Real;
  nodo : TtreeNode;
begin
  a := valor1.Value;
  b := valor2.Value;
  c := valor3.Value;

  disc := power(b, 2)-4*a*c;

  if disc >=0 then
  begin
    {calcular las raices}
    x1 := (-1*b + sqrt( disc ))/ (2*a);
    x2 := (-1*b - sqrt( disc ))/ (2*a);

    nodo := TTreeNode.Create(nil);
    if x1 = x2 then
    begin
         nodo := TTreeNode.Create(nil);
         nodo := tv_resultados.Items.Add(nodo, 'Solucion unica, disc='+ FloatToStr(disc)+ ', x=' + FloatToStr(x1));
         nodo.ImageIndex := 1;
         nodo.SelectedIndex := 1;
    end
    else
    begin
         nodo := tv_resultados.Items.Add(nodo,'x1='+FloatToStr(x1) +'; x2='+FloatToStr(x2));
         nodo.ImageIndex := 1;
         nodo.SelectedIndex := 1;
    end;
  end
  else
  begin
    {mostrar un error}
    nodo := TTreeNode.Create(nil);
    nodo := tv_resultados.Items.Add(nodo,'Error: No tiene solucion, disc ='+ FloatToStr(disc));
    nodo.ImageIndex := 0;
    nodo.SelectedIndex := 0;
  end;

end;

end.

