unit cart;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, memds, DB, Forms, Controls, Dialogs, StdCtrls, DBGrids,
  RTTICtrls, Classes, CartModel, ItemContainer, unit2;

type

  { TFormCart }
  TFormCart = class(TForm)

    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MemDataset1: TMemDataset;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: word);
    procedure FormShow(Sender: TObject);

  private
    FCart: TCartModel;
    procedure LogCartUpdated(Items: TArrayItem; Aggregates: TAggregateDictionary);
    procedure ClearInputs();

  public


  end;

var
  FormCart: TFormCart;


implementation

{$R *.lfm}

{ TFormCart }


procedure TFormCart.Button1Click(Sender: TObject);
begin
  ClearInputs;
end;

procedure TFormCart.Button2Click(Sender: TObject);
begin
  Form2.Show;
end;


procedure TFormCart.Edit1KeyUp(Sender: TObject; var Key: word);
var
  product: string;
  ProductItem: TProductItemModel;
begin
  if IntToStr(Key) = '13' then
  begin
    product := Edit1.Text;
    Edit1.Text := '';
    try
      ProductItem := TProductItemModel.Create;
      ProductItem.Entity.sku := '001';
      ProductItem.Entity.Name := product;
      ProductItem.AddAggregate('Total', 1000);
      FCart.Add(ProductItem);
    finally

    end;

  end;
end;

procedure TFormCart.FormShow(Sender: TObject);
begin
  FCart :=  TCartModel.Create;
  FCart := Form2.Cart;
  FCart.OnUpdate := @LogCartUpdated;
  ClearInputs;
  with MemDataset1 do
  begin
    CreateTable;
    Open;
  end;

end;

procedure TFormCart.ClearInputs();
var
  i: integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TEdit then
      (Components[i] as TEdit).Clear();
end;

procedure TFormCart.LogCartUpdated(Items: TArrayItem; Aggregates: TAggregateDictionary);
var
  I: integer;
  Total: double;
begin
  WriteLn('Cart updated');
  //if MemDataset1.RecordSize > 0 then
  MemDataset1.Clear(False);
  for I := 0 to Length(Items) - 1 do
  begin
    MemDataset1.Append;
    MemDataset1.FieldByName('sku').Value := Items[I].Entity.sku;
    MemDataset1.FieldByName('name').Value := Items[I].Entity.Name;
    MemDataset1.Post;
  end;
  //WriteLn('Total');
  Aggregates.TryGetValue('Total', Total);
  Label2.Caption := FloatToStr(Total);
end;

end.

