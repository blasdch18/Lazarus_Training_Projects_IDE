unit uFormProgram1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TPrograma01 }

  TPrograma01 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Saludar(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Programa01: TPrograma01;

implementation

{$R *.lfm}

{ TPrograma01 }

procedure TPrograma01.FormCreate(Sender: TObject);
begin

end;

procedure TPrograma01.Button1Saludar(Sender: TObject);
begin
  Label1.Caption:='Hola , Blas !' ;

end;

end.

