unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, Menus,
  TAGraph, TASeries;

type

  { TGraficador }

  TGraficador = class(TForm)
    Chart1: TChart;
    GraficoArea: TAreaSeries;
    GraficoSeries: TBarSeries;
    GraficoLineal: TLineSeries;
    GraficoPie: TPieSeries;
    Datos: TGroupBox;
    Graficos_Estadisticos: TGroupBox;
    MainMenu1: TMainMenu;
    Archivo: TMenuItem;
    Editar: TMenuItem;
    Acerca: TMenuItem;
    Guardar: TMenuItem;
    a_: TMenuItem;
    Abrir: TMenuItem;
    Graficos: TMenuItem;
    CambiarColor: TMenuItem;
    Gbarras: TMenuItem;
    GLineal: TMenuItem;
    Garea: TMenuItem;
    Gpastel: TMenuItem;
    Autor: TMenuItem;
    Salir: TMenuItem;
    celdas: TStringGrid;
    ColorDialog1: TColorDialog;
    procedure AreaClick(Sender: TObject);
    procedure BarrasClick(Sender: TObject);
    procedure CloseProgram(Sender: TObject);
    procedure Colorear(Sender: TObject);
    procedure EditingDone(Sender: TObject);
    procedure LinealClick(Sender: TObject);
    procedure LoadFile(Sender: TObject);
    procedure MostrarAutor(Sender: TObject);
    procedure PastelClick(Sender: TObject);
    procedure SaveFile(Sender: TObject);
  private

  public
    procedure HaciendoGrafica;


  end;

var
  Graficador: TGraficador;
  colorGrafico :Tcolor;

implementation

{$R *.lfm}

{ TGraficador }

procedure TGraficador.MostrarAutor(Sender: TObject);
begin
  ShowMessage(' Autor : Blas Cruz ');
end;

procedure TGraficador.PastelClick(Sender: TObject);
begin
   Gpastel.Checked := not Gpastel.Checked;
   if Gpastel.Checked = True then
   begin
     Garea.Checked := False;
     GLineal.Checked := False;
     Gpastel.Checked := False;
   end;
end;

procedure TGraficador.SaveFile(Sender: TObject);
begin
  celdas.SaveToCSVFile('Datos.xlsx');
  celdas.SaveToFile('datos.xml');
end;

procedure TGraficador.EditingDone(Sender: TObject);
begin
  HaciendoGrafica;
end;

procedure TGraficador.LinealClick(Sender: TObject);
begin
  Glineal.Checked := not Glineal.Checked;
  if Glineal.Checked = True then
  begin
    Gbarras.Checked := False;
    Garea.Checked := False;
    Gpastel.Checked := False;
  end;
end;

procedure TGraficador.LoadFile(Sender: TObject);
begin
  celdas.LoadFromFile('datos.xml');
end;

procedure TGraficador.AreaClick(Sender: TObject);
begin
   Garea.Checked := not Garea.Checked;
   if Garea.Checked = True then
   begin
     Gbarras.Checked := False;
     GLineal.Checked := False;
     Gpastel.Checked := False;
   end;

end;

procedure TGraficador.BarrasClick(Sender: TObject);
begin
   Gbarras.Checked := not Gbarras.Checked;
   if Gbarras.Checked = True then
   begin
     Garea.Checked := False;
     GLineal.Checked := False;
     Gpastel.Checked := False;
   end;

end;


procedure TGraficador.CloseProgram(Sender: TObject);
begin
   Close;
end;

procedure TGraficador.Colorear(Sender: TObject);
begin
   if ColorDialog1.Execute then
   begin
     colorGrafico := ColorDialog1.Color;
     HaciendoGrafica;
   end;

end;

procedure TGraficador.HaciendoGrafica;
  var
  i, nFilas: Integer;
  n: Integer;
begin
  nFilas := celdas.RowCount;
  {borrar las graficas}
  GraficoLineal.Clear;
  GraficoArea.Clear;
  GraficoSeries.Clear;
  GraficoPie.Clear;
  for i:=1 to nFilas-1 do
  begin
    if celdas.Cells[0,i] <> '' then
    begin
      n := StrToInt(celdas.Cells[0,i]);
      if GLineal.Checked then
      begin
        GraficoLineal.Add(n,'',clRed);
        end;
      if Garea.Checked then
      begin
        GraficoArea.Add(n,'',clRed);
        end;
      if Gbarras.Checked then
      begin
        GraficoSeries.Add(n,'',clRed);
        end;
      if Gpastel.Checked then
      begin
        GraficoPie.Add(n,'',clRed);
      end;
   end;
  end;
end;

end.



