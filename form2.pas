unit Form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CartModel, ItemContainer;

type

  { TForm1 }

  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
  private
    procedure LogCartUpdated(Items: TArrayItem; Aggregates: TAggregateDictionary);

  public

  end;

var
  FormOther: TForm2;

implementation

{$R *.lfm}

procedure TForm2.LogCartUpdated(Items: TArrayItem; Aggregates: TAggregateDictionary);
var
  I: integer;
  Total: double;
begin
  WriteLn('Form 2 ===> Cart updated');
  Aggregates.TryGetValue('Total', Total);
  Label2.Caption := FloatToStr(Total);
end;

end.

