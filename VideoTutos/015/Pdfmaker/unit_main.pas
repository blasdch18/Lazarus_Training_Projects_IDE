unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  registros, IniFiles;

type

  { TRegistrosForm }

  TRegistrosForm = class(TForm)
    lv_lista: TListView;
    Registro: TMenuItem;
    IngresarEstudiantesSM: TMenuItem;
    MenuPrincipal: TMainMenu;
    procedure FormCreate(Sender: TObject);
    procedure IngresarEstudiantesSMClick(Sender: TObject);

  private

  public
    procedure leerInformacionEstudiantes;

  end;

var
  RegistrosForm: TRegistrosForm;
  archivoEstudiantes : TIniFile;

implementation

{$R *.lfm}

{ TRegistrosForm }


procedure TRegistrosForm.IngresarEstudiantesSMClick(Sender: TObject);
var
  cant : Integer;
begin
    RegistroEstudiantesForm.bingresoEstudiante := False;
    RegistroEstudiantesForm.ShowModal;

    {tomar los datos}
    cant := archivoEstudiantes.ReadInteger('Datos','Cantidad',0);
    Inc(cant);
    //RegistroEstudiantesForm.obtenerCodigo;
    archivoEstudiantes.WriteString('Estudiante'+IntToStr(cant),'Nombre',
    RegistroEstudiantesForm.obtenerNombre);

    archivoEstudiantes.WriteString('Estudiante'+IntToStr(cant),'Codigo',
    RegistroEstudiantesForm.obtenerCodigo);

    archivoEstudiantes.WriteString('Estudiante'+IntToStr(cant),'Grado',
    RegistroEstudiantesForm.obtenerGrado);

    archivoEstudiantes.WriteString('Estudiante'+IntToStr(cant),'Genero',
    RegistroEstudiantesForm.obtenerGenero);

    archivoEstudiantes.WriteInteger('Datos','Cantidad', cant);
end;

procedure TRegistrosForm.leerInformacionEstudiantes;
var
  cant, i : Integer;
  nombre, codigo, grado, genero: String;
begin
    cant := archivoEstudiantes.ReadInteger('Datos','Cantidad',0);
    for i:= 1 to cant do
    begin
      nombre := archivoEstudiantes.ReadString(
      'Estudiante'+ IntToStr(i), 'Nombre', '');

      codigo := archivoEstudiantes.ReadString(
      'Estudiante'+ IntToStr(i), 'Codigo', '');

      grado := archivoEstudiantes.ReadString(
      'Estudiante'+ IntToStr(i), 'Grado', '');

      genero := archivoEstudiantes.ReadString(
      'Estudiante'+ IntToStr(i), 'Genero', '');

      with lv_lista.Items.Add do
      begin
       SubItems.Add(IntToStr(i));
       SubItems.Add(nombre);
       SubItems.Add(codigo);
       SubItems.Add(grado);
       SubItems.Add(genero);
      end;
end;

end;

procedure TRegistrosForm.FormCreate(Sender: TObject);
begin
  archivoEstudiantes := TIniFile.create('estudiantes.txt');

  leerInformacionEstudiantes;
  end;
end.
