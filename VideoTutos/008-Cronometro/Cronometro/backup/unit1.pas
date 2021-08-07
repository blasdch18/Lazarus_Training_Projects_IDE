unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Cronometro_idx: TLabel;
    Miliseconds_idx: TLabel;
    Cronometro: TTimer;
    Miliseconds: TTimer;
    Start: TSpeedButton;
    Stop: TSpeedButton;
    Restart: TSpeedButton;
    procedure crono(Sender: TObject);
    procedure MIliSecondsDoing(Sender: TObject);
    procedure RestartClick(Sender: TObject);
    procedure StarCrono(Sender: TObject);
    procedure StopClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  hora, minuto, segundo : Integer;
  milisegundo : Integer;
  permiteIniciar : Boolean;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.crono(Sender: TObject);
begin
  if permiteIniciar then
    begin
    segundo := segundo + 1;
    if segundo > 59 then
    begin
         minuto := minuto + 1;
         segundo := 0;
         if minuto > 59 then
         begin
         hora := hora + 1;
         minuto := 0;
         end;
    end;
    Cronometro_idx.Caption := IntToStr(hora) +':'
                         + IntToStr(minuto) +':'
                         + IntToStr(segundo);
    Miliseconds_idx.Caption := IntToStr(milisegundo);
  end;
end;

procedure TForm1.MIliSecondsDoing(Sender: TObject);
begin
  if permiteIniciar then
  begin
    milisegundo := milisegundo + 10;
    if milisegundo > 999 then
    begin
      milisegundo := 0;
    end;

  end;
end;

procedure TForm1.StarCrono(Sender: TObject);
begin
  permiteIniciar := True;
end;

procedure TForm1.StopClick(Sender: TObject);
begin
  permiteIniciar := False;
end;

procedure TForm1.RestartClick(Sender: TObject);
begin
  hora := 0;
  minuto := 0;
  segundo := 0;
  milisegundo := 0;
  Cronometro_idx.Caption := IntToStr(hora) +':'
                         + IntToStr(minuto) +':'
                         + IntToStr(segundo);
  Miliseconds_idx.Caption := IntToStr(milisegundo);

end;

end.

