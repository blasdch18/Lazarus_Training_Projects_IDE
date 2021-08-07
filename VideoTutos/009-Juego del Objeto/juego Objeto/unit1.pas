unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TAppJuegoObjeto }

  TAppJuegoObjeto = class(TForm)
    Image_cancha: TImage;
    Image_balon: TImage;
    procedure dameClick(Sender: TObject);
    procedure IniciarAplicacion(Sender: TObject);
  private

  public

  end;

var
  AppJuegoObjeto: TAppJuegoObjeto;

implementation

{$R *.lfm}

{ TAppJuegoObjeto }

procedure TAppJuegoObjeto.IniciarAplicacion(Sender: TObject);
begin
  {programar todo lo que se inicia en la aplicacion}
  AppJuegoObjeto.Left :=0;
  AppJuegoObjeto.Top := 0;
  AppJuegoObjeto.Width := Screen.Width;
  AppJuegoObjeto.Height:= Screen.Height;
  AppJuegoObjeto.BorderStyle:= bsNone;

end;

procedure TAppJuegoObjeto.dameClick(Sender: TObject);
var
  ancho, alto : Integer;
  posX, posY: Integer;
  begin
    ancho := Screen.Width-65;
    alto := Screen.Height-65;
    {generar una nueva posicion para el balon}
    Randomize;
    posX := round(Random(ancho));
    posY := round(Random(alto));

    Image_balon.Left := posX;
    Image_balon.Top := posY;



end;

end.

