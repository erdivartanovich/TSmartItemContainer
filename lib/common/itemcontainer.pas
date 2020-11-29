{
 OBSERVABLE AGGREGATED ITEM CONTAINER & EVENT CONTAINER
 Copyright (c) 2020 by Erdivartanovich ( https://erdivartanovich.github.io/aboutme )
 License: MIT
}


unit ItemContainer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Generics.Collections;

type
  TObjectDictionary = specialize TObjectDictionary<string, TObject>;
  TAggregateDictionary = specialize TObjectDictionary<string, double>;
  {
   === WILL APPLIED ===
   TAggregateMethod = (Sum, Avg, Max, Min);
  }

  TAggregateField = record
    Key: string;
    Multiplied: boolean;
  end;

  TAggregateFields = array of TAggregateField;

  { TItem }
  generic TItem<T> = class
  private
    FAggregateFields: TAggregateFields;
    function GetAggregateFields: TAggregateFields;
  public
    Entity: T;
    Id: string;
    Aggregate: TAggregateDictionary;
    property AggregateFields: TAggregateFields read GetAggregateFields;
    constructor Create(Identifier: string = '');
    function AddAggregate(AKey: string; AValue: double;
      IsMultiplied: boolean = False): TAggregateDictionary;
  end;

  { TAggregatedItemContainer }
  generic TAggregatedItemContainer<TItem> = class
    type
    TArrayItem = array of TItem;
    TItemMap = specialize TObjectHashMap<string, TItem>;
    TUpdateEvent = procedure(Items: TArrayItem; Aggregates: TAggregateDictionary) of object;
    TUpdateEventListeners = array of TUpdateEvent;
  private
    FItems: TArrayItem;
    FItemMap: TItemMap;
    FEvents: TUpdateEventListeners;
    function GetItems: TArrayItem;
    function GetAggregates: TAggregateDictionary;
    procedure SetOnUpdate(Ev: TUpdateEvent);

  public
    property Items: TArrayItem read GetItems;
    property Agregates: TAggregateDictionary read GetAggregates;
    property OnUpdate: TUpdateEvent write SetOnUpdate;
    constructor Create;
    procedure Add(Value: TItem);
    procedure Delete(ItemId: string);
    procedure Clear;
    procedure Subscribe(ASubscriber: TUpdateEvent);
    { Triggers the event if anything is pushed to container }
    procedure Observe;
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

  { TEventContainer }
  TEventContainer = class
    type
    TEventHandler = procedure(AData: TObject) of object;
    TEventMap = specialize TObjectHashMap<string, TEventHandler>;
  private
    FEventMap: TEventMap;
  public
    constructor Create;
    procedure On(EventName: String; EventHandler: TEventHandler);
    procedure Emit(EventName: String; AData: TObject);
  end;


implementation

constructor TItem.Create(Identifier: string = '');
var
  NewId: string;
  Guid: TGuid;
begin
  if Identifier = '' then
  begin
    CreateGUID(Guid);
    NewId := GUIDToString(Guid);
  end
  else
    NewId := Identifier;
  Self.Id := NewId;
  Self.Aggregate := TAggregateDictionary.Create;
  Self.FAggregateFields := TAggregateFields.Create;
  Self.AddAggregate('Qty', 1);
end;

function TItem.AddAggregate(AKey: string; AValue: double;
  IsMultiplied: boolean = False): TAggregateDictionary;
var
  AggregateField: TAggregateField;
begin
  Self.Aggregate.AddOrSetValue(AKey, AValue);
  with AggregateField do
  begin
    Key := AKey;
    Multiplied := IsMultiplied;
  end;
  SetLength(Self.FAggregateFields, Length(Self.FAggregateFields) + 1);
  Self.FAggregateFields[Length(Self.FAggregateFields) - 1] := AggregateField;
  Result := Self.Aggregate;
end;

function TItem.GetAggregateFields(): TAggregateFields;
begin
  Result := Self.FAggregateFields;
end;

constructor TAggregatedItemContainer.Create;
begin
  if not Assigned(FItemMap) then
    FItemMap := TItemMap.Create;
end;

function TAggregatedItemContainer.GetItems(): TArrayItem;
var
  MapItems: TArrayItem;
begin
  MapItems := FItemMap.Values.ToArray();
  SetLength(FItems, Length(MapItems));
  FItems := Copy(MapItems, 0, MaxInt);
  Result := FItems;
end;

function TAggregatedItemContainer.GetAggregates(): TAggregateDictionary;
var
  I, J: integer;
  AggregateFields: TAggregateFields;
  Item: TItem;
  SumVals: double;
const
  Qty: double = 0;
  Val: double = 0;
begin
  Result := TAggregateDictionary.Create;
  if FItemMap.Count > 0 then
    AggregateFields := Copy(Self.Items[0].AggregateFields, 0, MaxInt);
  for I := 0 to Length(AggregateFields) - 1 do
  begin
    SumVals := 0;
    for J := 0 to Length(Self.Items) - 1 do
    begin
      Item := Self.Items[J];
      Item.Aggregate.TryGetValue(AggregateFields[I].Key, Val);
      Item.Aggregate.TryGetValue('Qty', Qty);
      SumVals += Val;
    end;
    Result.Add(AggregateFields[I].Key, SumVals);
  end;
end;


procedure TAggregatedItemContainer.Add(Value: TItem);
var
  I: integer;
  ExistingItem: TItem;
  AggregateField: TAggregateField;
const
  Qty: double = 0;
  Val: double = 0;
  NewVal: double = 0;
begin
  FItemMap.TryGetValue(Value.id, ExistingItem);
  if Assigned(ExistingItem) then
  begin
    ExistingItem.Aggregate.TryGetValue('Qty', Qty);
    Qty += 1;
    Value.Aggregate.AddOrSetValue('Qty', Qty);
    for I := 0 to Length(ExistingItem.AggregateFields) - 1 do
    begin
      AggregateField := ExistingItem.AggregateFields[I];
      if AggregateField.Multiplied then
      begin
        ExistingItem.Aggregate.TryGetValue(AggregateField.Key, Val);
        Value.Aggregate.TryGetValue(AggregateField.Key, NewVal);
        Value.Aggregate.AddOrSetValue(AggregateField.Key, Val + NewVal);
      end;
    end;
  end;
  FItemMap.AddOrSetValue(Value.Id, Value);
  Observe;
end;

procedure TAggregatedItemContainer.Delete(ItemId: string);
begin
  FItemMap.Remove(ItemId);
  Observe;
end;

procedure TAggregatedItemContainer.Clear();
begin
  FItemMap.Clear;
  Observe;
end;

procedure TAggregatedItemContainer.SetOnUpdate(Ev: TUpdateEvent);
begin
  SetLength(FEvents, Length(FEvents) + 1);
  FEvents[Length(FEvents) - 1] := Ev;
end;

procedure TAggregatedItemContainer.Subscribe(ASubscriber: TUpdateEvent);
begin
  Self.SetOnUpdate(ASubscriber);
end;

procedure TAggregatedItemContainer.Observe;
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

constructor TEventContainer.Create;
begin
  if not Assigned(FEventMap) then
    FEventMap := TEventMap.Create;
end;

procedure TEventContainer.On(EventName: String; EventHandler: TEventHandler);
begin
  FEventMap.AddOrSetValue(EventName, EventHandler);
end;

procedure TEventContainer.Emit(EventName: String; AData: TObject);
Var
  EventHandler: TEventHandler;
begin
  FEventMap.TryGetValue(EventName, EventHandler);
  If Assigned(EventHandler) then
     EventHandler(AData);
end;

end.
