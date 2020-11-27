program canvasser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, runtimetypeinfocontrols, memdslaz, cart, CartModel,
  ItemContainer, unit2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormCart, FormCart);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

