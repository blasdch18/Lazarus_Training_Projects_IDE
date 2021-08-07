unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls;

type

  { TForm_Gestion_Errores }

  TForm_Gestion_Errores = class(TForm)
    bt_Division: TButton;
    bt_tipo1: TButton;
    bt_tipo2: TButton;
    bt_tipo3: TButton;
    bt_tipo4: TButton;
    gb_mensajes: TGroupBox;
    ImageList1: TImageList;
    l_resultado: TLabel;
    le_numerador: TLabeledEdit;
    le_denominador: TLabeledEdit;
    lv_mensajes: TTreeView;
    Splitter1: TSplitter;
    barraEstado: TStatusBar;
    procedure bt_DivisionClick(Sender: TObject);
    procedure bt_DivisionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bt_DivisionMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure bt_tipo1Click(Sender: TObject);
    procedure bt_tipo2Click(Sender: TObject);
    procedure bt_tipo3Click(Sender: TObject);
    procedure bt_tipo4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure escribirMensaje(msg : String; tipo :Integer);
    procedure escribirCoordenadas (x,y : Integer);
    procedure escribirBarra (msg : String ; posicion : Integer);

  end;
  const
    MSG_INFO=0;
    MSG_AYUDA=1;
    MSG_ADVERTENCIA=2;
    MSG_ERROR=3;

var
  Form_Gestion_Errores: TForm_Gestion_Errores;
  mx, my :Integer;
  suma : Integer;

implementation

{$R *.lfm}

{ TForm_Gestion_Errores }

procedure TForm_Gestion_Errores.FormCreate(Sender: TObject);
begin
  //gb_mensajes.Visible:=False;
end;

procedure TForm_Gestion_Errores.escribirMensaje(msg: String; tipo: Integer);
var
  nodo : TTreeNode;
begin
  nodo := TTreeNode.Create(nil);
  nodo := lv_mensajes.Items.Add(nodo,msg);
  nodo.SelectedIndex:= tipo;
  nodo.ImageIndex:=tipo;
end;

procedure TForm_Gestion_Errores.escribirCoordenadas(x, y: Integer);
begin
  barraEstado.Panels[0].Text := IntToStr(x)+','+IntToStr(y);
end;

procedure TForm_Gestion_Errores.escribirBarra(msg: String; posicion: Integer);
begin
  barraEstado.Panels[posicion].Text:=msg;
end;

procedure TForm_Gestion_Errores.bt_DivisionClick(Sender: TObject);
var
  a,b,c :Real;
begin
  try
  a := StrToFloat(le_numerador.Text);
  b := StrToFloat(le_denominador.Text);
  {division}
  c := a/b;
  l_resultado.Caption:= 'El cociente es :'+ FloatToStr(c);

  except
    on E : Exception do
    escribirMensaje('Error :'+ E.Message +
                    'posiblemente el cociente entre cero no esta definido',MSG_ERROR);

  end;
end;

procedure TForm_Gestion_Errores.bt_DivisionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := x;
  my := y;
end;

procedure TForm_Gestion_Errores.bt_DivisionMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
     bt_Division.Left := bt_Division.Left + x -mx;
     bt_Division.Top := bt_Division.Top + y - my;

     escribirCoordenadas(bt_Division.Left,bt_Division.Top);

     Inc(suma);
     escribirBarra('Suma := '+ IntToStr(suma),2);
     escribirBarra('Probando aplicacion ',1);
     //barraEstado.Panels[1].Text:= IntToStr(suma);
  end;
end;

procedure TForm_Gestion_Errores.bt_tipo1Click(Sender: TObject);
begin
  escribirMensaje('Mensaje del tipo Informacion',MSG_INFO);
end;

procedure TForm_Gestion_Errores.bt_tipo2Click(Sender: TObject);
begin
  escribirMensaje('Mensaje del tipo ayuda',MSG_AYUDA);
end;

procedure TForm_Gestion_Errores.bt_tipo3Click(Sender: TObject);
begin
  escribirMensaje('Mensaje del tipo advertencia',MSG_ADVERTENCIA);
end;

procedure TForm_Gestion_Errores.bt_tipo4Click(Sender: TObject);
begin
  escribirMensaje('Mensaje del tipo error',MSG_ERROR);
end;

end.

