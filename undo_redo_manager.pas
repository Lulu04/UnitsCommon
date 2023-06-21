unit undo_redo_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, generics.collections, Generics.Defaults;

type


{ TCustomUndoRedoManager }

generic TCustomUndoRedoManager<T> = class
private type
  TTypedStack = class(specialize TStack<T>);
  TUndoRedoManagerActionType = (urmatNone,
                                urmatUndo, urmatRedo,
                                urmatPushToUndo, urmatPushToRedo,
                                urmatPopFromUndo, urmatPopFromRedo);
private
  FMaxSize: integer;
  FOnChange: TNotifyEvent;
  FUndo: TTypedStack;
  FRedo: TTypedStack;
  FLastAction: TUndoRedoManagerActionType;
  FDoNotChangeLastAction: boolean;
  procedure DoChangeEvent;
  function GetRedoCount: integer;
  function GetUndoCount: integer;
  procedure SetLastAction(aActionType:TUndoRedoManagerActionType);
protected
  // override this method !!
  procedure DoUndoRedo(ItsUndo: boolean); virtual; abstract;
  // override this method to destroy an item removed because the stack is full
  procedure DestroyItem( constref aItem: T ); virtual; abstract;
public
  constructor Create;
  destructor destroy; override;

  procedure Clear; virtual;
  procedure PushToUndo( constref aItem: T );
  procedure PushToRedo( constref aItem: T );
  function UndoAvailable: boolean;
  function RedoAvailable: boolean;
  // pop item from Undo stack
  function PopFromUndo: T;
  // pop item from Redo stack
  function PopFromRedo: T;

  // return the current item from Undo stack, without pop it
  function UndoPeekCurrent: T;
  // return the current item from Redo stack, without pop it
  function RedoPeekCurrent: T;

  procedure Undo;
  procedure Redo;

  property OnChange: TNotifyEvent read FOnChange write FOnChange;
  // The maximum number of action to keep. default value is 50.
  property MaxSize: integer read FMaxSize write FMaxSize;

  property UndoCount: integer read GetUndoCount;
  property RedoCount: integer read GetRedoCount;
end;


implementation

{ TCustomUndoRedoManager }

procedure TCustomUndoRedoManager.DoChangeEvent;
begin
  if FOnChange <> NIL then
    FOnChange( self );
end;

function TCustomUndoRedoManager.GetRedoCount: integer;
begin
  Result := FRedo.Count;
end;

function TCustomUndoRedoManager.GetUndoCount: integer;
begin
  Result := FUndo.Count;
end;

procedure TCustomUndoRedoManager.SetLastAction(aActionType: TUndoRedoManagerActionType);
begin
  if not FDoNotChangeLastAction then
    FLastAction := aActionType;
end;

constructor TCustomUndoRedoManager.Create;
begin
  FUndo := TTypedStack.Create;
  FRedo := TTypedStack.Create;
  FMaxSize := 100;
  FLastAction := urmatNone;
end;

destructor TCustomUndoRedoManager.destroy;
begin
  FUndo.Free;
  FRedo.Free;
  inherited destroy;
end;

procedure TCustomUndoRedoManager.Clear;
begin
  FLastAction := urmatNone;
  FDoNotChangeLastAction := False;
  FUndo.Clear;
  FRedo.Clear;
  DoChangeEvent;
end;

procedure TCustomUndoRedoManager.PushToUndo(constref aItem: T);
begin
  if FLastAction = urmatUndo then begin
    PopFromRedo;
  end;

  SetLastAction(urmatPushToUndo);

  FUndo.Push(aItem);
  if FUndo.Count >= FMaxSize then
    DestroyItem(FUndo.DoRemove(0, cnRemoved));
  DoChangeEvent;
end;

procedure TCustomUndoRedoManager.PushToRedo(constref aItem: T);
begin
  SetLastAction(urmatPushToRedo);

  FRedo.Push(aItem);
  if FRedo.Count >= FMaxSize then
    DestroyItem(FRedo.DoRemove(0, cnRemoved));
  DoChangeEvent;
end;

function TCustomUndoRedoManager.PopFromUndo: T;
begin
  Result := FUndo.Pop;
  DoChangeEvent;
  SetLastAction(urmatPopFromUndo);
end;

function TCustomUndoRedoManager.PopFromRedo: T;
begin
  Result := FRedo.Pop;
  DoChangeEvent;
  SetLastAction(urmatPopFromRedo);
end;

function TCustomUndoRedoManager.UndoPeekCurrent: T;
begin
  Result := FUndo.Peek;
end;

function TCustomUndoRedoManager.RedoPeekCurrent: T;
begin
  Result := FRedo.Peek;
end;

procedure TCustomUndoRedoManager.Undo;
begin
  if UndoAvailable then begin
    FDoNotChangeLastAction := True;
    DoUndoRedo(True);
    FDoNotChangeLastAction := False;

    SetLastAction(urmatUndo);
  end;
end;

procedure TCustomUndoRedoManager.Redo;
begin
  if RedoAvailable then begin
    SetLastAction(urmatRedo);

    FDoNotChangeLastAction := True;
    DoUndoRedo(False);
    FDoNotChangeLastAction := False;
  end;
end;

function TCustomUndoRedoManager.UndoAvailable: boolean;
begin
  Result := FUndo.Count > 0;
end;

function TCustomUndoRedoManager.RedoAvailable: boolean;
begin
  Result := (FRedo.Count > 0);
  Result := Result and (FLastAction <> urmatPushToUndo);
end;

end.

