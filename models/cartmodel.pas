unit CartModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ItemContainer;

type
  { TCartModel }
  TProduct = record
    Sku: string;
    Name: string;
  end;

  TProductItemModel = specialize TItem<TProduct>;
  TCartModel = specialize TAggregatedItemContainer<TProductItemModel>;
  TStoreModel = specialize TItemContainer<TCartModel>;
  TArrayItem = array of TProductItemModel;


implementation


end.
