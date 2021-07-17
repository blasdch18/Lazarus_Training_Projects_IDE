unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TMaquina }

  TMaquina = class(TForm)
    DownP: TImage;
    Maquina: TImage;
    UpP: TImage;
    ficha1: TImage;
    ficha2: TImage;
    ficha3: TImage;
    procedure TirarPalanca(Sender: TObject);
  private

  public
    procedure tirarF1;
    procedure tirarF2;
    procedure tirarF3;


  end;

var
  Maquina: TMaquina;
  p1, p2, p3 : Integer;

implementation

{$R *.lfm}

{ TMaquina }

procedure TMaquina.TirarPalanca(Sender: TObject);
begin
  UpP.Picture.LoadFromFile('imagenes/blank.png');
  DownP.Picture.LoadFromFile('imagenes/palDown.png');
  tirarF1;
  tirarF2;
  tirarF3;
  UpP.Picture.LoadFromFile('imagenes/palUP.png');
  DownP.Picture.LoadFromFile('imagenes/blank.png');
  {validar si gano}
  if p1 = p2 and p2 = p3 then
  begin
    ShowMessage('Ganaste Bro!');
    end
  else
  begin
    ShowMessage('Vuelve a Nacer !! Perdiste!!');
  end;


end;

procedure TMaquina.tirarF1;
var
  n, i: Integer ;
begin
   Randomize;
  for i:=1 to 100 do
  begin
    n := round(Random(7))+1;
    Application.ProcessMessages;
    ficha1.Picture.LoadFromFile('imagenes/ficha'+IntToStr(n)+'.png');
  end;
  {guardar el ultimo valor}
  p1 := n;

end;

procedure TMaquina.tirarF2;
var
  n, i: Integer ;
begin
   Randomize;
  for i:=1 to 100 do
  begin
    n := round(Random(7))+1;
    Application.ProcessMessages;
    ficha2.Picture.LoadFromFile('imagenes/ficha'+IntToStr(n)+'.png');
  end;
    {guardar el ultimo valor}
    p2 := n;

end;

procedure TMaquina.tirarF3;
var
  n, i: Integer ;
begin
   Randomize;
  for i:=1 to 100 do
  begin
    n := round(Random(7))+1;
    Application.ProcessMessages;
    ficha3.Picture.LoadFromFile('imagenes/ficha'+IntToStr(n)+'.png');
  end;
    {guardar el ultimo valor}
  p3 := n;

end;




end.

