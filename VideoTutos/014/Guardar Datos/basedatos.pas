unit baseDatos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, Menus, ComCtrls, reporte, IniFiles;

type

  { TForm_BaseDatos }

  TForm_BaseDatos = class(TForm)
    lv_tabla: TListView;
    MainMenu1: TMainMenu;
    Archivo: TMenuItem;
    registroProd: TMenuItem;
    Registro: TMenuItem;
    Acerca: TMenuItem;
    procedure ArchivoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure registroProdClick(Sender: TObject);
  private

  public
    procedure leerBaseDatosSuperMercado;


  end;

var
  Form_BaseDatos: TForm_BaseDatos;
  archivoProductos: TiniFile;

implementation

{$R *.lfm}

{ TForm_BaseDatos }

procedure TForm_BaseDatos.ArchivoClick(Sender: TObject);
begin

end;

procedure TForm_BaseDatos.FormCreate(Sender: TObject);
begin
   archivoProductos:= TiniFile.Create('productos.txt');

   leerBaseDatosSupermercado;
end;

procedure TForm_BaseDatos.registroProdClick(Sender: TObject);
var
  cant: Integer;
begin
   Registros_form.bHizoRegistro:= False;
   Registros_form.limpiar;
   Registros_form.ShowModal;
   if Registros_form.bHizoRegistro then
   begin
     {guarde datos}
     cant:= archivoProductos.ReadInteger(
     'Datos','Cantidad',0);
     Inc(cant);
     archivoProductos.WriteString(
     'Producto'+IntToStr(cant),
     'Nombre',Registros_form.obtenerNombre);

     archivoProductos.WriteString(
     'Producto'+IntToStr(cant),
     'Codigo',Registros_form.obtenerCodigo);

     archivoProductos.WriteString(
     'Producto'+IntToStr(cant),
     'Precio',Registros_form.obtenerPrecio);

     archivoProductos.WriteInteger('Datos','Cantidad',cant);
     lv_tabla.Items.Clear;
     leerBaseDatosSupermercado;
   end;
end;

procedure TForm_BaseDatos.leerBaseDatosSuperMercado;
var
  cant, i : Integer;
  nombre, codigo, precio: String;
begin
   cant:= archivoProductos.ReadInteger(
          'Datos','Cantidad',0) ;
   for i:=1 to cant do
   begin
     nombre:= archivoProductos.ReadString(
     'Producto' + IntToStr(i), 'Nombre', '');
     codigo:= archivoProductos.ReadString(
     'Producto' + IntToStr(i), 'Codigo', '');
     precio:= archivoProductos.ReadString(
     'Producto' + IntToStr(i), 'Precio', '');

     with lv_tabla.Items.Add do
     begin
        SubItems.Add(IntToStr(i));
        SubItems.Add(nombre);
        SubItems.Add(codigo);
        SubItems.Add(precio);
     end;
   end;



end;

end.

