program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, pack_powerpdf, unit_main, registros
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TRegistrosForm, RegistrosForm);
  Application.CreateForm(TRegistroEstudiantesForm, RegistroEstudiantesForm);
  Application.Run;
end.

