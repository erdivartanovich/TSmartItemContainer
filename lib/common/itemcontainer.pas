unit ItemContainer;

{$mode objfpc}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, TypInfo, Generics.Collections;

type
  TObjectDictionary = specialize TObjectDictionary<string, TObject>;
  TAggregateDictionary = specialize TObjectDictionary<string, double>;
  TAggregateMethod = (Sum, Avg, Max, Min);
  TAggregateKeys = array of string;

  { TItem }
  generic TItem<T> = class
  private
    FAggregateKeys: TAggregateKeys;
    function GetAggregateKeys: TAggregateKeys;
  public
    Entity: T;
    Id: string;
    Aggregate: TAggregateDictionary;
    property AggregateKeys: TAggregateKeys read GetAggregateKeys;
    constructor Create(Identifier: string);
    function AddAggregate(Key: string; Value: double): TAggregateDictionary;
  end;

  { TAggregatedItemContainer }
  generic TAggregatedItemContainer<TItem> = class
    type
    TArrayItem = array of TItem;
    TUpdateEvent =
    procedure(Items: TArrayItem; Aggregates: TAggregateDictionary) of object;
    TUpdateEventListeners = array of TUpdateEvent;
  private
    FItems: TArrayItem;
    FEvents: TUpdateEventListeners;
    function GetItems: TArrayItem;
    function GetAggregates: TAggregateDictionary;
    procedure SetOnUpdate(Ev: TUpdateEvent);

  public
    property Items: TArrayItem read GetItems;
    property Agregates: TAggregateDictionary read GetAggregates;
    property OnUpdate: TUpdateEvent write SetOnUpdate;
    procedure Add(Value: TItem);
    procedure Delete(ItemId: string);
    { Triggers the event if anything is pushed to container }
    procedure TriggerEvent();
  end;

  { TItemContainer }
  generic TItemContainer<TAggregatedItemContainer> = class
    type
    TUpdateEvent =
    procedure(State: TObjectDictionary) of object;
    TUpdateEventListeners = array of TUpdateEvent;
    TStates = array of TObjectDictionary;
  private
    FEvents: TUpdateEventListeners;
    FStates: TStates;
    procedure SetOnUpdate(Index: integer; Ev: TUpdateEvent);
    function GetOnUpdate(Index: integer): TUpdateEvent;
    procedure SetState(Value: TObjectDictionary);
    function GetState: TObjectDictionary;
  public
    property OnUpdate[Index: integer]: TUpdateEvent read GetOnUpdate write SetOnUpdate;
    property State: TObjectDictionary read GetState write SetState;
  end;



implementation

constructor TItem.Create(Identifier: string);
begin
  Self.Id := Identifier;
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

function TAggregatedItemContainer.GetItems(): TArrayItem;
begin
  Result := FItems;
end;

function TAggregatedItemContainer.GetAggregates(): TAggregateDictionary;
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


procedure TAggregatedItemContainer.Add(Value: TItem);
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[Length(FItems) - 1] := Value;
  TriggerEvent;
end;

procedure TAggregatedItemContainer.Delete(ItemId: string);
begin

end;

procedure TAggregatedItemContainer.SetOnUpdate(Ev: TUpdateEvent);
begin
  SetLength(FEvents, Length(FEvents) + 1);
  FEvents[Length(FEvents) - 1] := Ev;
end;



procedure TAggregatedItemContainer.TriggerEvent;
var
  I: integer;
begin
  { Call the update event only if there is a listener }
  for I := 0 to Length(FEvents) - 1 do
  begin
    if Assigned(FEvents[I]) then
      FEvents[I](Self.Items, Self.GetAggregates);
  end;
end;

procedure TItemContainer.SetOnUpdate(Index: integer; Ev: TUpdateEvent);
begin
  SetLength(FEvents, Index + 1);
  FEvents[Index] := Ev;
end;

function TItemContainer.GetOnUpdate(Index: integer): TUpdateEvent;
begin
  Result := FEvents[Index];
end;

procedure TItemContainer.SetState(Value: TObjectDictionary);
begin
  SetLength(FStates, Length(FStates) + 1);
  FStates[Length(FStates) - 1] := Value;
end;

function TItemContainer.GetState: TObjectDictionary;
begin
  if (Length(FStates) > 0) then
    Result := FStates[Length(FStates) - 1]
  else
    Result := nil;
end;

end.
