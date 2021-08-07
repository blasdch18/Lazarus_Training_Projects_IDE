unit reporte;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TRegistros_form }

  TRegistros_form = class(TForm)
    e_nombre: TEdit;
    e_codigo: TEdit;
    e_precio: TEdit;
    RegistrarBt: TButton;
    Nombre: TGroupBox;
    Panel1: TPanel;
    Codigo: TGroupBox;
    Precio: TGroupBox;
    procedure RegistrarBtClick(Sender: TObject);
  private

  public
    bHizoRegistro: Boolean;
    function obtenerNombre : String;
    function obtenerCodigo: String;
    function obtenerPrecio: String;

    procedure limpiar;

  end;

var
  Registros_form: TRegistros_form;

implementation

{$R *.lfm}

{ TRegistros_form }

procedure TRegistros_form.RegistrarBtClick(Sender: TObject);
begin
  if (e_nombre.Text<>'')and(e_precio.Text<>'') and(e_codigo.Text<>'')then
  begin
    {se puede registrar}
    bHizoRegistro := True;
    Close;
    end
  else
  begin
    MessageDlg('Informacion','Por favor Rellene todos los campos',mtWarning,[mbYes],0);
  end;

end;

function TRegistros_form.obtenerNombre: String;
begin
  Result:= e_nombre.Text;
end;

function TRegistros_form.obtenerCodigo: String;
begin
  Result:= e_codigo.Text;
end;

function TRegistros_form.obtenerPrecio: String;
begin
  Result:= e_precio.Text;

end;

procedure TRegistros_form.limpiar;
begin
  e_nombre.Text:='';
  e_codigo.Text:='';
  e_precio.Text:='';

end;

end.

