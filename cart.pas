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
    Button3: TButton;
    Button4: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MemDataset1: TMemDataset;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: word);
    procedure FormShow(Sender: TObject);

  private
    FCart: TCartModel;
    FState: TObjectDictionary;
    FEventContainer: TEventContainer;
    procedure LogCartUpdated(Items: TArrayItem; Aggregates: TAggregateDictionary);
    procedure ClearInputs();
    procedure UpdateView(AData: TObject);
  public


  end;

var
  FormCart: TFormCart;


implementation

{$R *.lfm}

{ TFormCart }

procedure TFormCart.Button2Click(Sender: TObject);
begin
  Form2.Show;
end;

procedure TFormCart.Button1Click(Sender: TObject);
begin
  MemDataset1.Clear(false);
  FCart.Clear;
end;

procedure TFormCart.Button3Click(Sender: TObject);
Var DeletedId: String;
begin
   WriteLn('[Form1] ===> ' , 'DeletedId: ', MemDataset1.FieldValues['Id']);
   DeletedId := MemDataset1.FieldValues['Id'];
   FCart.Delete(DeletedId);
end;

procedure TFormCart.Button4Click(Sender: TObject);
begin
  FCart.Observe;
end;


procedure TFormCart.Edit1KeyUp(Sender: TObject; var Key: word);
var
  ProductItem: TProductItemModel;
  Product: string;
const
  Price: double = 1000;
begin
  if IntToStr(Key) = '13' then
  begin
    product := Edit1.Text;
    Edit1.Text := '';
    try
      {
       If Item identifier is not provided like
       ==============================================
          ProductItem := TProductItemModel.Create();
       ==============================================
       Then GUID will be used as identifier therefore the item will be never duplicated.
       If identifier is provided, item will be accumulated
      }
      ProductItem := TProductItemModel.Create(Product);
      ProductItem := TProductItemModel.Create(Product);
      ProductItem.Entity.Sku := Product;
      ProductItem.Entity.Name := Product;
      ProductItem.Entity.Price := Price;
      ProductItem.AddAggregate('Total', Price, True);
      FCart.Add(ProductItem);
      FState := TObjectDictionary.Create;
      FState.Add('Cart', FCart);
      Form2.Store.State := FState;
    finally

    end;

  end;
end;

procedure TFormCart.FormShow(Sender: TObject);
begin
  {INIT CART CONTAINER}
  Form2.Store := TStoreModel.Create;
  Form2.Cart := TCartModel.Create;
  Form2.Cart.Subscribe(@LogCartUpdated);
  Form2.Cart.Subscribe(@Form2.LogCartUpdated);

  FCart := TCartModel.Create;
  FCart := Form2.Cart;

  {PREPARE COMPONENTS}
  ClearInputs;
  with MemDataset1 do
  begin
    CreateTable;
    Open;
  end;

  {PREPARE EVENT CONTAINER}
  FEventContainer :=  TEventContainer.Create;
  // Add event listener
  FEventContainer.On('CartUpdated', @UpdateView);

  {FIRE CART CONTAINER OBSERVE EVENT}
  FCart.Observe;
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
  Total, SubTotal, Qty: double;
begin
  MemDataset1.Clear(False);
  for I := 0 to Length(Items) - 1 do
  begin
    MemDataset1.Append;
    MemDataset1.FieldByName('Id').Value := Items[I].Id;
    MemDataset1.FieldByName('sku').Value := Items[I].Entity.Sku;
    MemDataset1.FieldByName('name').Value := Items[I].Entity.Name;
    MemDataset1.FieldByName('price').Value := Items[I].Entity.price;
    Items[I].Aggregate.TryGetValue('Total', SubTotal);
    Items[I].Aggregate.TryGetValue('Qty', Qty);
    MemDataset1.FieldByName('subtotal').Value := SubTotal;
    MemDataset1.FieldByName('qty').Value := Qty;
    MemDataset1.Post;
  end;
  Aggregates.TryGetValue('Total', Total);
  Aggregates.TryGetValue('Qty', Qty);
  Label2.Caption := FloatToStr(Total);
  Label4.Caption := FloatToStr(Qty);
  FEventContainer.Emit('CartUpdated', nil);

  WriteLn('[Form1] Cart updated');
end;

procedure TFormCart.UpdateView(AData: TObject);
Var bEnabled: Boolean;
begin
  WriteLn('[Form1] Handle Update View');
  bEnabled := MemDataset1.DataSize > 0;
  Button1.Enabled := bEnabled;
  Button3.Enabled :=  bEnabled;
end;

end.

