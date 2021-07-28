unit registros;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TRegistroEstudiantesForm }

  TRegistroEstudiantesForm = class(TForm)
    cb_genero: TComboBox;
    insertarBt: TButton;
    e_nombre: TEdit;
    e_codigo: TEdit;
    e_grado: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Panel1: TPanel;

    procedure insertarBtClick(Sender: TObject);
  private

  public
    bingresoEstudiante: Boolean;
    {tomar los datos}
    function obtenerNombre : String;
    function obtenerCodigo : String;
    function obtenerGrado : String;
    function obtenerGenero : String;

  end;

var
  RegistroEstudiantesForm: TRegistroEstudiantesForm;

implementation

{$R *.lfm}

{ TRegistroEstudiantesForm }

procedure TRegistroEstudiantesForm.insertarBtClick(Sender: TObject);
begin
   if (e_nombre.Text<>'') and
      (e_codigo.Text<>'') and
      (e_grado.Text<>'') and
      (cb_genero.Text<>'') then
      begin
        bingresoEstudiante := True;
        Close;
      end
   else
     begin
     MessageDlg('Informacion','Debe Llenar todos los campos',
     mtWarning, [mbYes], 0);
     end;
end;

function TRegistroEstudiantesForm.obtenerNombre: String;
begin
  Result := e_nombre.Text;
end;

function TRegistroEstudiantesForm.obtenerCodigo: String;
begin
 Result := e_codigo.Text;
end;

function TRegistroEstudiantesForm.obtenerGrado: String;
begin
 Result := e_grado.Text;
end;

function TRegistroEstudiantesForm.obtenerGenero: String;
begin
 Result := cb_genero.Text;
end;


end.

