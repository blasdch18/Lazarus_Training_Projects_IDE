unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, ComCtrls, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    FloatSpinEdit3: TFloatSpinEdit;
    FloatSpinEdit4: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Splitter1: TSplitter;
    Resultados: TTreeView;
    procedure CalcularDistancia(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Splitter1CanOffset(Sender: TObject; var NewOffset: Integer;
      var Accept: Boolean);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin

end;

procedure TForm1.CalcularDistancia(Sender: TObject);
var
  x1, y1, x2, y2 : Real;
  dist : Real;
begin
  {capturar los datos}
  x1 := FloatSpinEdit1.Value;
  y1 := FloatSpinEdit2.Value;
  x2 := FloatSpinEdit3.Value;
  y2 := FloatSpinEdit4.Value;
  {calculo de la distancia}
  dist := sqrt( power( x2-x1, 2 ) + power( y2-y1, 2 ) );
  {mostrar resultados}
  Resultados.Items.Add( nil, 'La distancia es = ' + FloatToSTr(dist));

end;

procedure TForm1.Splitter1CanOffset(Sender: TObject; var NewOffset: Integer;
  var Accept: Boolean);
begin

end;

end.

