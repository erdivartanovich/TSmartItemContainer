unit unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CartModel,
  ItemContainer;

type

  { TForm2 }

  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    Cart: TCartModel;
    Store: TStoreModel;
    procedure LogCartUpdated(Items: TArrayItem; Aggregates: TAggregateDictionary);

  end;

var
  Form2: TForm2;


implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormShow(Sender: TObject);
begin

end;

procedure TForm2.FormCreate(Sender: TObject);
begin

end;

procedure TForm2.LogCartUpdated(Items: TArrayItem; Aggregates: TAggregateDictionary);
var
  Total, Qty: double;
begin
  WriteLn('======>', '[Form2] Cart updated');
  Aggregates.TryGetValue('Total', Total);
  Aggregates.TryGetValue('Qty', Qty);
  Label4.Caption := FloatToStr(Qty);
  Label2.Caption := FloatToStr(Total);
end;

end.

