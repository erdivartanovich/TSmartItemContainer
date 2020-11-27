unit ItemContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Generics.Collections;

type
  TAggregateMethod = (Sum, Avg, Max, Min);
  TAggregateDictionary = specialize TObjectDictionary<string, double>;
  TAggregateKeys = array of string;

  { TItem }

  generic TItem<T> = class
  private
    FAggregateKeys: TAggregateKeys;
    function GetAggregateKeys: TAggregateKeys;
  public
    Entity: T;
    Aggregate: TAggregateDictionary;
    property AggregateKeys: TAggregateKeys read GetAggregateKeys;
    constructor Create;
    function AddAggregate(Key: string; Value: double): TAggregateDictionary;
  end;

  { TItemContainer }

  generic TItemContainer<TItem> = class
    type
    TArrayItem = array of TItem;
    TUpdateEvent = procedure(Items: TArrayItem; Aggregates: TAggregateDictionary) of object;
  private
    FItems: TArrayItem;
    FUpdate: TUpdateEvent;

    function GetItems: TArrayItem;
    function GetAggregates: TAggregateDictionary;
    procedure SetUpdate(Ev: TUpdateEvent);
  public
    property Items: TArrayItem read GetItems;
    property Agregates: TAggregateDictionary read GetAggregates;
    property OnUpdate: TUpdateEvent read FUpdate write SetUpdate;
    procedure Add(Value: TItem);

    { Triggers the event if anything is pushed to container }
    procedure TriggerEvent();

  end;




implementation

constructor TItem.Create();
begin
  Self.Aggregate := TAggregateDictionary.Create;
  Self.FAggregateKeys := TAggregateKeys.Create;
end;

function TItem.AddAggregate(Key: string; Value: double): TAggregateDictionary;
begin
  Self.Aggregate.Add(Key, Value);
  SetLength(Self.FAggregateKeys, Length(Self.FAggregateKeys) + 1);
  Self.FAggregateKeys[Length(Self.FAggregateKeys) - 1] := Key;
  Result := Self.Aggregate;
end;

function TItem.GetAggregateKeys(): TAggregateKeys;
begin
  Result := Self.FAggregateKeys;
end;

function TItemContainer.GetItems(): TArrayItem;
begin
  Result := FItems;
end;

function TItemContainer.GetAggregates(): TAggregateDictionary;
var
  I, J: integer;
  AggregateKeys: TAggregateKeys;
  Item: TItem;
  Val: double;
  SumVals: double;
begin
  Result := TAggregateDictionary.Create;
  if Length(Self.Items) > 0 then
    AggregateKeys := Copy(FItems[0].AggregateKeys, 0, MaxInt);


  for I := 0 to Length(AggregateKeys) - 1 do
  begin
    SumVals := 0;
    Val := 0;
    for J := 0 to Length(FItems) - 1 do
    begin
      Item := FItems[J];
      Item.Aggregate.TryGetValue('Total', Val);
      SumVals += Val;
    end;
    Result.Add('Total', SumVals);
  end;
end;


procedure TItemContainer.Add(Value: TItem);
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[Length(FItems) - 1] := Value;
  TriggerEvent;
end;

procedure TItemContainer.SetUpdate(Ev: TUpdateEvent);
begin
  FUpdate := Ev;
end;

procedure TItemContainer.TriggerEvent;
begin
  { Call the update event only if there is a listener }
  if Assigned(FUpdate) then
    FUpdate(Self.Items, Self.GetAggregates);
end;

end.
