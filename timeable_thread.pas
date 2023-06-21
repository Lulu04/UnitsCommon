unit timeable_thread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

TUpdateEvent=procedure( const aElapsedTime: single ) of object;

{ TTimeableThread }

TTimeableThread=class(TThread)
private
  FCS: TRTLCriticalSection;
private
  FPeriodUpdate: integer;
  FDoUpdateEvent: TUpdateEvent;
  procedure SetPeriodUpdate(AValue: integer);
protected
  procedure Execute; override;
public
  // create a thread that call a callback every aUpdatePeriod milliseconds
  Constructor Create(aCallBack: TUpdateEvent; aUpdatePeriod: integer; aStart: boolean);

  procedure Lock;
  procedure Unlock;

  // to read or set the period (in milliseconds)
  property UpdatePeriod: integer read FPeriodUpdate write SetPeriodUpdate;
end;

implementation

{ TTimeableThread }

procedure TTimeableThread.SetPeriodUpdate(AValue: integer);
begin
  if FPeriodUpdate=AValue then Exit;
  FPeriodUpdate:=AValue;
  if FPeriodUpdate<1 then FPeriodUpdate:=1;
end;

procedure TTimeableThread.Execute;
var T1, T2, DeltaT: QWord;
    v: single;
begin
 T1 := GetTickCount64;
 while not Terminated do
  begin
   T2 := GetTickCount64;
   DeltaT := T2-T1;
   if DeltaT >= FPeriodUpdate then begin
           v := single( DeltaT )/1000.0;
           if FDoUpdateEvent<>NIL then begin
            Lock;
            FDoUpdateEvent( v );
            Unlock;
           end;
           T1:=T2;
   end else sleep( 1 );
  end;
 DoneCriticalSection( FCS );
end;

constructor TTimeableThread.Create( aCallBack: TUpdateEvent; aUpdatePeriod: integer; aStart: boolean );
begin
 inherited Create(true );
 FPeriodUpdate := aUpdatePeriod;
 FDoUpdateEvent := aCallBack;

 InitCriticalSection( FCS );

 if aStart then Start;
end;

procedure TTimeableThread.Lock;
begin
 EnterCriticalSection( FCS );
end;

procedure TTimeableThread.Unlock;
begin
 LeaveCriticalSection( FCS );
end;


end.

