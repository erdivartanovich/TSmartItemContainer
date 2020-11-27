unit CartModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ItemContainer;

type
  { TCartModel }
  TProduct = record
    sku: string;
    Name: string;
  end;

  TProductItemModel = specialize TItem<TProduct>;
  TCartModel = specialize TItemContainer<TProductItemModel>;
  TArrayItem = array of TProductItemModel;

  {CartListener}
  TCartListener = class(TObject)
  private
    procedure ListenForUpdate(Items: TArrayItem; Aggregates: TAggregateDictionary);
  end;

implementation

procedure TCartListener.ListenForUpdate(Items: TArrayItem; Aggregates: TAggregateDictionary);
begin

end;

end.
