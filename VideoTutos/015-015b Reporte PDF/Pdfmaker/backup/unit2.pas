unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TRegistroEstudiantesForm }

  TRegistroEstudiantesForm = class(TForm)
    insertarBt: TButton;
    e_nombre: TEdit;
    e_codigo: TEdit;
    e_grado: TEdit;
    e_genero: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Panel1: TPanel;
    procedure insertarBtClick(Sender: TObject);
  private

  public
    bingresoEstudiante: Boolean;

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
      (e_genero.Text<>'') then
      begin
        bingresoEstudiante := True;
        Close;
      end;
end;

end.

