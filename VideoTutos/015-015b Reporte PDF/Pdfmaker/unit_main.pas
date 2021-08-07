unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  PReport, registros, IniFiles;

type

  { TRegistrosForm }

  TRegistrosForm = class(TForm)
    lb_nombre: TPRLabel;
    lb_codigo: TPRLabel;
    lb_grado: TPRLabel;
    lv_lista: TListView;
    Archivo: TMenuItem;
    Exportar: TMenuItem;
    PaginaControl: TPageControl;
    pagina: TPRPage;
    impresora: TPReport;
    PRImage1: TPRImage;
    PRLabel1: TPRLabel;
    PRLabel2: TPRLabel;
    lb_id: TPRLabel;
    PRLayoutPanel1: TPRLayoutPanel;
    PRRect1: TPRRect;
    PRRect2: TPRRect;
    Registro: TMenuItem;
    IngresarEstudiantesSM: TMenuItem;
    MenuPrincipal: TMainMenu;
    tab_tabla: TTabSheet;
    tab_reporte: TTabSheet;
    procedure ExportarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IngresarEstudiantesSMClick(Sender: TObject);
    procedure PaginaControlChange(Sender: TObject);
    procedure RegistroClick(Sender: TObject);

  private

  public
    procedure leerInformacionEstudiantes;
    procedure crearEtiquetas;
    procedure crearID(id : Integer; l,t : Integer);
    procedure crearNombre(nombre : String; l,t :Integer);
    procedure crearCodigo(codigo : String; l,t :Integer);
    procedure crearGrado(grado : String; l,t :Integer);

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

procedure TRegistrosForm.PaginaControlChange(Sender: TObject);
begin

end;

procedure TRegistrosForm.RegistroClick(Sender: TObject);
begin

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

procedure TRegistrosForm.crearEtiquetas;
var
  cant,i: Integer;
  nombre, grado, codigo : String;
begin
  cant := archivoEstudiantes.ReadInteger('Datos','Cantidad',0);

  for i:=1 to cant do
  begin
    nombre := archivoEstudiantes.ReadString(
    'Estudiante'+IntToStr(i),'Nombre','');

    codigo := archivoEstudiantes.ReadString(
    'Estudiante'+IntToStr(i),'Codigo','');

    grado := archivoEstudiantes.ReadString(
    'Estudiante'+IntToStr(i),'Grado','');

    crearID(i,26,249+(i-1)*20);
    crearNombre(nombre,116,249+(i-1)*20);
    crearCodigo(codigo,276,249+(i-1)*20);
    crearGrado(grado, 430 , 249 +(i-1)*20);
  end;
end;

procedure TRegistrosForm.crearID(id: Integer; l, t: Integer);
var
  etID: TPRLabel;
begin
    etID := TPRLabel.Create(nil);

    with etID do
    begin
       Parent := PRLayoutPanel1;
       Left:=l;
       Top:=t;

       Caption:= IntToStr(id);
       FontColor:=cl3DDkShadow;
    end;

end;

procedure TRegistrosForm.crearNombre(nombre: String; l, t: Integer);
var
  etNombre: TPRLabel;
begin
    etNombre := TPRLabel.Create(nil);

    with etNombre do
    begin
       Parent := PRLayoutPanel1;
       Left:=l;
       Top:=t;

       Caption:= nombre;
       FontColor:=cl3DDkShadow;
    end;

end;

procedure TRegistrosForm.crearCodigo(codigo: String; l, t: Integer);
var
  etCodigo: TPRLabel;
begin
    etCodigo := TPRLabel.Create(nil);

    with etCodigo do
    begin
       Parent := PRLayoutPanel1;
       Left:=l;
       Top:=t;

       Caption:= codigo;
       FontColor:=cl3DDkShadow;
    end;

end;

procedure TRegistrosForm.crearGrado(grado: String; l, t: Integer);
var
  etGrado: TPRLabel;
begin
    etGrado := TPRLabel.Create(nil);

    with etGrado do
    begin
       Parent := PRLayoutPanel1;
       Left:=l;
       Top:=t;

       Caption:= grado;
       FontColor:=cl3DDkShadow;
    end;

end;


procedure TRegistrosForm.FormCreate(Sender: TObject);
begin
  archivoEstudiantes := TIniFile.create('estudiantes.txt');

  leerInformacionEstudiantes;

  paginaControl.ActivePage := tab_tabla;
  paginaControl.ShowTabs := False;
  end;

procedure TRegistrosForm.ExportarClick(Sender: TObject);
begin
  crearEtiquetas;
  {imprimir}
  impresora.FileName:='ReportePDF.pdf';
  impresora.BeginDoc;
  impresora.Print(pagina);
  impresora.EndDoc;

  ShowMessage('El reporte ha sido guardado exitosamente');
end;

end.
