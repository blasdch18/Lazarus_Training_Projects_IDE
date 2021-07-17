unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls;

type

  { TTablasVerdad }

  TTablasVerdad = class(TForm)
    Image1: TImage;
    focoOff: TImage;
    Bmodo: TImage;
    modoOperator: TLabel;
    r: TLabel;
    p: TLabel;
    q: TLabel;
    modoOff1: TImage;
    modoOff2: TImage;
    PageControl1: TPageControl;
    Interseccion: TTabSheet;
    procedure CambiarModo(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure mode_B(Sender: TObject);
    procedure mode_A(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private

  public
    procedure onoff;

  end;

var
  TablasVerdad: TTablasVerdad;
  valorA, valorB ,operatorW: Boolean;

implementation

{$R *.lfm}

{ TTablasVerdad }

procedure TTablasVerdad.PageControl1Change(Sender: TObject);
begin

end;

procedure TTablasVerdad.onoff;
begin
  if operatorW then
  begin
     if valorA and valorB then
     begin
       focoOff.Picture.LoadFromFile('imagenes/bon.jpg');
       r.Caption:='V';
       end
     else
     begin
       focoOff.Picture.LoadFromFile('imagenes/boff.jpg');
       r.Caption:='F';
     end;
  end
  else
  begin
     if valorA or valorB then
     begin
       focoOff.Picture.LoadFromFile('imagenes/bon.jpg');
       r.Caption:='V';
       end
     else
     begin
       focoOff.Picture.LoadFromFile('imagenes/boff.jpg');
       r.Caption:='F';
     end;
  end;
end;

procedure TTablasVerdad.mode_A(Sender: TObject);
begin
  valorA:= not valorA;
  if valorA  then
  begin
    modoOff1.Picture.LoadFromFile('imagenes/on.png');
    p.Caption:='V';
  end
  else
  begin
    modoOff1.Picture.LoadFromFile('imagenes/off.jpg');
    p.Caption:='F';
  end;
  onoff;
end;

procedure TTablasVerdad.mode_B(Sender: TObject);
begin
  valorB := not valorB;
  if valorB then
  begin
    modoOff2.Picture.LoadFromFile('imagenes/on.png');
    q.Caption:='V';
  end
  else
  begin
    modoOff2.Picture.LoadFromFile('imagenes/off.jpg');
    q.Caption:='F';
  end;
  onoff;

end;

procedure TTablasVerdad.Image1Click(Sender: TObject);
begin

end;

procedure TTablasVerdad.CambiarModo(Sender: TObject);
begin
  operatorW := not operatorW ;
  if operatorW then
  begin
    modoOperator.Caption:='Intersección';
  end
  else
  begin
    modoOperator.Caption:='Unión';
  end;


end;

end.

