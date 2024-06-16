unit frame_bglvirtualscreen_sequencer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics,
  LCLType, ExtCtrls, Types, LCLTranslator,
  BGRABitmap, BGRABitmapTypes, BGLVirtualScreen, BGRAOpenGL, BGRAOpenGLType,
  generics.collections, Generics.Defaults ;

resourcestring
  NotifyMoveMessage='Move';
  NotifyChangeDurationMessage='Change duration';
  NotifyCutMessage='Cut';
  NotifyInsertMessage='Insert';
  NotifyDeleteMessage='Delete';
  NotifyPasteMessage='Paste';
  NotifyDuplicateMessage='Duplicate';
  NotifyGroupMessage='Group';
  NotifyUnGroupMessage='Ungroup';
  NotifyVerticalShiftMessage='Vertical shift';
  NotifyRearrangeMessage='Rearrange';
  NotifyShiftMessage='Shift';



const
  SEQUENCER_VIEW_MAX_SECONDS = 3600*24*4; // the sequencer view can cover a maximum duration of 4 days
  SEQUENCER_VIEW_ZOOM_MIN = 0.5;       // smaller zoom is 0.5 second
  SEQUENCER_VIEW_ZOOM_MAX = 3600*24*2; // larger zoom is 2 days



type
  TFrameBGLSequencer=class;

  { TCustomSequencerStep }

  TCustomSequencerStep = class
  private
   FCaption: string;
   FParentSeq: TFrameBGLSequencer;
   FID: integer;
   FTimePos: single;
   FDuration: single;
   FSelected: boolean;
   FGroup: integer;
   FTop: integer;
   FWidth: integer;
   procedure SetCaption(AValue: string);
   procedure SetParentSeq(AValue: TFrameBGLSequencer);
   procedure SetSelected(AValue: boolean);
   procedure SetGroup(AValue: integer);
   procedure SetDuration(AValue: single);
  public
   constructor Create;

   procedure Redraw(aParentFrame: TFrameBGLSequencer; aBGLContext: TBGLContext); virtual;

   function Serialize: string; virtual;
   procedure Deserialize(const {%H-}s: string); virtual;

   // Return TRUE if TimePos + aDeltaTime >= 0
   function CanApplyTimeOffset(aDeltaTime: single): boolean;
   procedure UpdateWidth; virtual;

   property ParentSeq: TFrameBGLSequencer read FParentSeq write SetParentSeq;
   property Caption: string read FCaption write SetCaption;
   property Top: integer read FTop write FTop;
   property Width: integer read FWidth write FWidth;
   // Time position in seconds, from 0 to what you need
   property TimePos: single read FTimePos write FTimePos;
   // Length of the step in seconds
   property Duration: single read FDuration write SetDuration;
   // TRUE if the step is in selected mode
   property Selected: boolean read FSelected write SetSelected;
   // The group index to which the step belongs. 0=not grouped (default)
   property Group: integer read FGroup write SetGroup;
   // Unique step ID
   property ID: integer read FID write FID;
 end;

  ArrayOfCustomSequencerStep = array of TCustomSequencerStep;


  { TSequencerStepComparer }

  TSequencerStepComparer = class(specialize TComparer<TCustomSequencerStep>)
   function Compare(constref ALeft, ARight: TCustomSequencerStep): Integer; override;
  end;

  { TStepList }

  TStepList = class(specialize TList<TCustomSequencerStep>)
  private
   FID: integer;
   function GetNextID: integer;
  public
   constructor Create;
   function Add(constref aStep: TCustomSequencerStep): SizeInt; override;
   procedure FreeSteps;
   procedure ResetID;
   function IndexOfID(aId: integer): integer;
   function GetItemByID(aID: integer): TCustomSequencerStep;
   property ID: integer read FID write FID;
  end;

   // action notification types for undo/redo
   TSequencerNotification = (snAdded, snDeleted, snChanged);

   // callback
   TSequencerNotifyEvent = procedure(Senders: ArrayOfCustomSequencerStep;
                                     aAction: TSequencerNotification;
                                     const aDescription: string) of object;

   TSequencerClickEvent = procedure(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; TimePos: single) of Object;

   TSequencerDuplicateStepEvent = function(aOriginal: TCustomSequencerStep): TCustomSequencerStep of object;
   TSequencerMergeStepEvent = function: TCustomSequencerStep of object;
   TNeedErrorSymbolCallback = function(aRenderer: TFrameBGLSequencer): TBGRABitmap of object;

   TFrameBGLSequenceOption=(
       // Draws vertical lines to mark the time position of each step.
       // Enabled by default.
       bglsStepVerticalLineVisible,

       // The step's vertical position are dicted by their height
       // Disabled by default
       bglsForceVStepPosition,

       // Use two color to fill the different step's vertical position
       // Disabled by default
       bglsAlternateColorForVStepPosition,

       // Draws the time legend area on the bottom of the view.
       // Enabled by default.
       bglsTimeAreaVisible,

       // Don't allow view scrolling to the right and keep the time 0 always visible
       // Disabled by default
       bglsKeepTimeOriginVisible
       );

   TFrameSequencerOptions = set of TFrameBGLSequenceOption;

  { TFrameBGLSequencer }

  TFrameBGLSequencer = class(TFrame)
    BGLVirtualScreen1: TBGLVirtualScreen;
    ScrollBar1: TScrollBar;
    procedure BGLVirtualScreen1MouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BGLVirtualScreen1MouseEnter(Sender: TObject);
    procedure BGLVirtualScreen1MouseLeave(Sender: TObject);
    procedure BGLVirtualScreen1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BGLVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGLVirtualScreen1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
    procedure BGLVirtualScreen1UnloadTextures(Sender: TObject; {%H-}BGLContext: TBGLContext);
    procedure FrameExit(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; {%H-}ScrollCode: TScrollCode;
      var ScrollPos: Integer);

    procedure StepMouseDown(Sender: TCustomSequencerStep;Button: TMouseButton; Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
   protected
    FOnNeedErrorSymbol: TNeedErrorSymbolCallback;
    FTextureErrorSymbol: IBGLTexture;
    FStepFont,
    FTimeFont: IBGLRenderedFont;
    FStepFontHeight: integer;
    FOpenGLObjectsNeedToBeReconstruct: boolean;
    procedure SetStepFontHeight(AValue: integer);
    procedure CreateOpenGLObjects;
    procedure DeleteOpenGLObjects;
    function VerticalLineCount: integer;
    function AdjustYStepIntoLine(aY: integer): integer;
    function DoNeedErrorSymbolCallback: TBGRABitmap; virtual;
   private
    FBeginTimeGraduation, FDeltaTimeGraduation: single;
    FCounterSmallGraduation: integer;
    procedure ComputeGraduationsParameters;
   protected
    FOnSelectionChange: TNotifyEvent;
    FOnTimeAreaClick: TSequencerClickEvent;
    FOnEmptyAreaClick: TSequencerClickEvent;
    FOnStepClick: TSequencerClickEvent;
    FOnDuplicateStep: TSequencerDuplicateStepEvent;
    FOnMergeStep: TSequencerMergeStepEvent;
    FOnMoveStep: TNotifyEvent;
    FOnNotify: TSequencerNotifyEvent;
    FOnUserChangeStepDuration: TNotifyEvent;
    procedure DoSelectionChangeEvent; virtual;
    procedure DoViewChangeEvent; virtual;
    procedure DoTimeAreaClickEvent( Button: TMouseButton; Shift: TShiftState; TimePos: single ); virtual;
    procedure DoEmptyAreaClickEvent( Button: TMouseButton; Shift: TShiftState; TimePos: single ); virtual;
    procedure DoStepClickEvent( aStep: TCustomSequencerStep; Button: TMouseButton; Shift: TShiftState ); virtual;
    function DoDuplicateStepEvent( aStep: TCustomSequencerStep ): TCustomSequencerStep; virtual;
    function DoMergeStepEvent: TCustomSequencerStep; virtual;
    procedure DoMoveStepEvent; virtual;
    procedure DoUserChangeDurationEvent; virtual;
   private
    FStepList: TStepList;
    FDuplicateValue: integer;
    FGroupValue: integer;
    FMousePosOrigin: TPoint;
    FTimePosOrigin: single;
    FUserIsDragingStep: boolean;
    FAlreadyInDragLoop: boolean;
    FUserScrollTheView: boolean;
    FUserDoSelection: boolean;
    FUserChangeStepDuration: boolean;
    FSelectedCount: integer;
    FCTRLPressed: boolean;   // to duplicate with key CTRL
    FALTPressed: boolean;    // to change the duration of a step with leftClick+ALT
    FTimeSelectionLow,
    FTimeSelectionHigh: single;
    function GetID: integer;
    function StepArea: TRect;
    function TimeArea: TRect;
    function TimeBase: integer;
    function IsInTimeArea(aY: integer): boolean;
    function IsInStepArea(aY: integer): boolean;
    procedure SetID(AValue: integer);
    procedure LoopUserDragStep;
    procedure LoopUserSetStepDuration;
    procedure LoopUserDoSelection;
    procedure LoopUserScrollTheViewWithMiddleMouseButton;
   private
    procedure UpdateStepsWidth;
    procedure UpdateStepsTop;
    function StepUnderMouse: TCustomSequencerStep; // return nil if none
   protected
    function ScreenToTimePos( aP: TPoint ): single;
    function XMouseToTime: single;
    function MouseIsOverSequencer: boolean;
    function YLineUnderMouse: integer;
   private
    FClipBoard: TStepList;
    FOnClipBoardChange: TNotifyEvent;
    procedure DoOnClipBoardChange;
    procedure DoCopySelectionToClipBoard( aDeleteTheCopied: boolean );
   private
    FMouseCursorX: integer;
    FMouseCursorVisible: boolean;
    procedure UpdateMouseCursorPosition;
   private
    FPlayCursorTimePos: single;
    FPlayCursorVisible: boolean;
   private
    FNeedStepsWidthUpdate: boolean;
    FNeedStepsTopUpdate: boolean;
    FOnViewChange: TNotifyEvent;
    FPixelPerSecond: single;
    FView_BeginTime: single;
    function GetView_EndTime: single;
    procedure SetPixelPerSecond(AValue: single);
    procedure SetView_BeginTime(AValue: single);
    procedure Sel_DragSelection( deltaTime: single );
    procedure UpdateScrollBar;
   private
    FOptions: TFrameSequencerOptions;
    procedure InternalSel_Group;
    procedure InternalSel_SetSelected( aStep: TCustomSequencerStep; SelectedState: boolean );
    procedure InternalSel_ToggleSelected ( aStep: TCustomSequencerStep );
   private
    FInvalidateAlreadySent: boolean;
    FColorTimeArea,
    FColorTimeLegend,
    FColorUserAreaSelection,
    FColorXAxis,
    FColorBackground1: TBGRAPixel;
    function GetColorBackground: TColor;
    function GetStepHeight: integer;
    procedure SetColorBackground(AValue: TColor);
   public
    Selected: ArrayOfCustomSequencerStep;
    // at the next redraw, forces the recalculation of steps width (will be done in the next redraw event)
    procedure NeedStepsWidthUpdate;
    procedure NeedStepsTopUpdate;
    procedure UpdateSelectedArray;
    constructor Create(aOwner: TComponent);override;
    destructor Destroy; override;
   // procedure EraseBackground({%H-}DC: HDC); override;

    procedure NotifyRange( aStartIndex, aEndIndex: integer;
                           aAction: TSequencerNotification;
                           const aDescription: string );
    procedure Notify( const aSteps: ArrayOfCustomSequencerStep;
                      aAction: TSequencerNotification;
                      const aDescription: string); virtual;

    // Force redraw
    procedure Redraw;

    procedure ForceReconstructOpenGLObjects;

    // Clear the sequencer
    procedure Clear; virtual;
    // Add a step to the sequencer.
    // aSetSelected=TRUE force the step to be selected -> Callback OnSelectionChange is then called
    procedure Add( aStep: TCustomSequencerStep; aSetSelected: boolean );
    procedure RawAdd( aStep: TCustomSequencerStep; aSetSelected: boolean; aSetID: boolean=TRUE );
    procedure RawDelete( aStep: TCustomSequencerStep );
    procedure RawDelete( aIndex: integer );
    procedure RawDeleteStepByID( aID: integer );
    procedure RawReplaceStepByID( aReplacement: TCustomSequencerStep );
    procedure RawApplyTimeOffsetOnStep( aStep: TCustomSequencerStep; TimeOffset: single );
   //    procedure SetSelected( aStep: TCustomSequencerStep; SelectedState: boolean );
    procedure Delete( aStep: TCustomSequencerStep; aDoOnSelectionChange: boolean=TRUE );

    procedure ClipBoard_CutSelection;
    procedure ClipBoard_CopySelection;
    procedure ClipBoard_PasteTo( aTimePos: single );
    procedure ClipBoard_Clear;
    function Clipboard_HasData: boolean;

    // duplicate the selected steps and group them.
    // Callback OnDuplicateStep event is fired to get the clone.
    procedure Sel_DuplicateAndGroup;

    // Merge the selected steps in single one.
    // Callback OnMergeStep event is fired to have the replacing one.
    procedure Sel_Merge;

    // delete the selected steps.
    procedure Sel_Delete;
    // group the selected steps.
    procedure Sel_Group;
    // ungroup the selected steps.
    procedure Sel_Ungroup;
    // return TRUE if selected steps remains visible if shifted vertically by delta pixel.
    function Sel_CanVerticalShift( delta: integer ): boolean;
    // shifts vertically the selected steps. 'delta' is in pixels.
    procedure Sel_VerticalShift( delta: integer );
    // the selected steps are re-positionned vertically.
    procedure Sel_RecomputeVerticalStepsPosition;
    procedure RecomputeVerticalStepsPosition;

    // return the first selected step or NIL if none.
    function Sel_FirstStepSelected: TCustomSequencerStep;
    // return the last selected step or NIL if none.
    function Sel_LastStepSelected: TCustomSequencerStep;
    procedure Sel_SelectNone;
    procedure Sel_SelectAll;

    function AnAreaIsSelected: boolean;
    procedure ForceNoAreaSelected;
    procedure ForceAreaSelected(aTimeBegin, aTimeEnd: single);
    function StepCountInSelectedArea: integer;
    function SelectedAreaBeginTime: single;
    function SelectedAreaEndTime: single;
    function SelectedAreaDuration: single;

    // maximize the view to fit all steps
    procedure View_All;
    // zoom on the selected area
    procedure View_ZoomOnSelectedArea;

    // set play cursor visible or not
    procedure PlayCursorVisible( AValue: boolean );
    // set play cursor time position
    procedure UpdatePlayCursorPosition( aTimePos: single );

    // converts abscissa to time (without adding View_BeginTime)
    function AbscissaToTimePos( aValue: integer ): single;
    // converts time to abscissa (substract View_BeginTime from aValue)
    function TimePosToAbscissa( aValue: single ): integer;

    function TimePosIsBeforeFirstStep( aTimePos: single ): boolean;
    function TimePosIsAfterLastStep( aTimePos: single ): boolean;
    function GetStepBefore( aTimePos: single ): TCustomSequencerStep; // NIL if none
    function GetStepAfter( aTimePos: single ): TCustomSequencerStep; // NIL if none
    function GetFirstStep: TCustomSequencerStep;
    function GetLastStep: TCustomSequencerStep;

    // chronology tools
    // Apply a time offset on one step
    procedure ApplyTimeOffsetOnStep( aStep: TCustomSequencerStep; TimeOffset: single );
    // Apply a time offset on each steps from aFirstStep to the last step
    procedure ShiftStepTimePositionFrom( aFirstStep: TCustomSequencerStep; TimeOffset: single );
    // Delete the time interval pointed by the specified time position.
    // If aTimePos points before the first step, all the chronology is shifted to the beginning
    // if aTimepos points between two steps, the time slice between this two steps is removed
    // if aTimePos points after the last step, do nothing
    procedure DeleteTimeAt( aTimePos: single );

    // enable/disable option
    procedure SetOptions( aSet:TFrameSequencerOptions; AValue: boolean );

    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    // Fired when the view has changed (shifted or zoomed)
    property OnViewChange: TNotifyEvent read FOnViewChange write FOnViewChange;
    // Fired when user click in the Time Area on the bottom of the view
    property OnTimeAreaClick: TSequencerClickEvent read FOnTimeAreaClick write FOnTimeAreaClick;
    // Fired when user click on empty position on the view
    property OnEmptyAreaClick: TSequencerClickEvent read FOnEmptyAreaClick write FOnEmptyAreaClick;
    // Fired when user clicks on a step
    property OnStepClick: TSequencerClickEvent read FOnStepClick write FOnStepClick;
    // Fired when it is needed to duplicate a step. If you use this possibility, you have to
    // define this callback that will return the duplicated step.
    property OnDuplicateStep: TSequencerDuplicateStepEvent read FOnDuplicateStep write FOnDuplicateStep;
    // Fired on any change in the steps list or on a step(s). Used to implement Undo/Redo system
    property OnNotify: TSequencerNotifyEvent read FOnNotify write FOnNotify;
    // Fired when user change the duration of a step
    property OnUserChangeStepDuration: TNotifyEvent read FOnUserChangeStepDuration write FOnUserChangeStepDuration;

    // callback fired when Sel_Merge is called
    // This callback must return the replacement step from the selected ones.
    // Take care to settup Caption, Top and TimePos property.
    property OnMergeStep: TSequencerMergeStepEvent read FOnMergeStep write FOnMergeStep;
    property OnMoveStep: TNotifyEvent read FOnMoveStep write FOnMoveStep;

    // called when the clipboard contents change. Usefull to enable/disable some widgets.
    property OnClipBoardChange: TNotifyEvent read FOnClipBoardChange write FOnClipBoardChange;
    // clipboard access
    property ClipBoard: TStepList read FClipBoard;

    // the list that contains all steps
    property StepList: TStepList read FStepList;
    property SelectedCount: integer read FSelectedCount;
    // Use to control the zoom in the view
    property PixelPerSecond: single read FPixelPerSecond write SetPixelPerSecond;
    // Use it to change the font height of the text in the steps
    property StepFontHeight: integer read FStepFontHeight write SetStepFontHeight default 13;
    // the total height of a step: timepos_height+caption_height+duration_height
    property StepHeight: integer read GetStepHeight;

    // contains the time in second of the beginning (left) of the view
    property View_BeginTime: single read FView_BeginTime write SetView_BeginTime;
    // The time in seconds of the end (right) of the view
    property View_EndTime: single read GetView_EndTime;

    // This property reflects the current options enabled.
    // By default, [bglsStepVerticalLineVisible, bglsTimeAreaVisible] are enabled.
    // Use SetOptions to change them.
    property Options: TFrameSequencerOptions read FOptions;

    // Steps have an unique ID. For each new step, this ID value is incremented.
    // So it is necessary to update this value when a project is loaded.
    // When the project is saved, this ID value must be saved also.
    property ID: integer read GetID write SetID;
    // Several steps can be grouped. It is done to assign them the same group value.
    // For each group created, this value is incremented.
    // So it is necessary to update this value when a project is loaded.
    // When the project is saved, this value must be saved also.
    property GroupValue: integer read FGroupValue write FGroupValue;

    property StepFont: IBGLRenderedFont read FStepFont;
    property TimeFont: IBGLRenderedFont read FTimeFont;
    property TextureErrorSymbol: IBGLTexture read FTextureErrorSymbol;
    property ColorBackground: TColor read GetColorBackground write SetColorBackground;
    property ColorBackground1: TBGRAPixel read FColorBackground1 write FColorBackground1;
    property ColorTimeArea: TBGRAPixel read FColorTimeArea write FColorTimeArea;
    property ColorTimeLegend: TBGRAPixel read FColorTimeLegend write FColorTimeLegend;
    property ColorUserAreaSelection: TBGRAPixel read FColorUserAreaSelection write FColorUserAreaSelection;
    property ColorXAxis: TBGRAPixel read FColorXAxis write FColorXAxis;

    property OnNeedErrorSymbol: TNeedErrorSymbolCallback read FOnNeedErrorSymbol write FOnNeedErrorSymbol;
  end;


  // returns a formatted string DD:HH:MM:SS.xxx of the given time in seconds.
  function TimeToString( aSeconds: double ): string;

implementation

uses Math;

var
  GroupColor: array[0..9] of TBGRAPixel;

function TimeToString(aSeconds: double): string;
var v, d, h, m, s, ms: integer;
begin
  v := Trunc(aSeconds);
  d := v div 86400;
  v := v mod 86400;

  h := v div 3600;
  v := v mod 3600;

  m := v div 60;
  s := v mod 60;

  ms := Round(Frac(aSeconds)*1000);
  if ms = 1000 then
  begin
    ms := 0;
    inc(s);
  end;

  Result := '';
  if d > 0 then
  begin
    Result += d.ToString+'j';
    if h < 10 then Result+='0';
    if h = 0 then Result+='0:';
  end;
  if ( h > 0 ) then begin
   Result += inttostr( h ) + ':';
   if m < 10 then Result += '0';
   if m = 0 then Result+='0:';
  end;
  if m > 0 then
  begin
    Result += inttostr( m ) + ':';
    if s < 10 then Result+='0';
  end;
  Result += s.ToString+'.';
  if ms < 10 then
    Result+='00'
  else if ms < 100 then
    Result+='0';
  Result+=ms.ToString;
end;

{ TStepList }

function TStepList.GetNextID: integer;
begin
  Result := FID;
  inc(FID);
end;

constructor TStepList.Create;
var FComp: TSequencerStepComparer;
begin
  FComp := TSequencerStepComparer.Create;
  inherited Create(FComp);
end;

function TStepList.Add(constref aStep: TCustomSequencerStep): SizeInt;
begin
  Result:=inherited Add(aStep);
  aStep.ID := GetNextID;
end;

procedure TStepList.FreeSteps;
begin
  while Count>0 do begin
   Items[0].Free;
   Delete(0);
  end;
end;

procedure TStepList.ResetID;
begin
  FID:=0;
end;

function TStepList.IndexOfID(aId: integer): integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
   if Items[i].ID=aID then begin
    Result := i;
    exit;
   end;
end;

function TStepList.GetItemByID(aID: integer): TCustomSequencerStep;
var i: Integer;
begin
  Result := NIL;
  for i:=0 to Count-1 do
   if Items[i].ID = aID then begin
     Result := Items[i];
     exit;
   end;
end;

{ TSequencerStepComparer }

function TSequencerStepComparer.Compare(constref ALeft, ARight: TCustomSequencerStep): Integer;
begin
 if ALeft.TimePos = ARight.TimePos then
   Result := 0
 else
 if ALeft.TimePos > ARight.TimePos then
   Result := 1
 else
   Result := -1;
end;

{ TFrameBGLSequencer }

procedure TFrameBGLSequencer.BGLVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  clickedStep: TCustomSequencerStep;
begin
  // Click on step
  if not FUserIsDragingStep and not FUserScrollTheView and not FUserChangeStepDuration then begin
    clickedStep := StepUnderMouse;
    if clickedStep <> NIL then begin
      StepMouseDown(clickedStep, Button, Shift, X, Y);
      exit;
    end;
  end;

  // Scroll the view with mouse middle button
  if (Button = mbMiddle)
     and not FUserIsDragingStep
     and not (bglsKeepTimeOriginVisible in FOptions) then
    LoopUserScrollTheViewWithMiddleMouseButton;

  if (Button = mbRight) and not FUserScrollTheView and not FUserIsDragingStep then
  begin
  end;

  if (Button = mbLeft) and not FUserScrollTheView then
  begin              // unselect all and enter in the step selection loop
    ForceNoAreaSelected;
    Sel_SelectNone;
    DoSelectionChangeEvent;
    FUserIsDragingStep := FALSE;
    LoopUserDoSelection;
  end;

  FCTRLPressed := FALSE;
end;

procedure TFrameBGLSequencer.BGLVirtualScreen1UnloadTextures(Sender: TObject;  BGLContext: TBGLContext);
begin
  FStepFont := NIL;
  FTimeFont := NIL;
end;

procedure TFrameBGLSequencer.FrameExit(Sender: TObject);
begin
  Sel_SelectNone;
  Redraw;
end;

procedure TFrameBGLSequencer.ScrollBar1Scroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  View_BeginTime := ScrollPos/100;
end;

procedure TFrameBGLSequencer.BGLVirtualScreen1MouseEnter(Sender: TObject);
begin
  FMouseCursorVisible:=TRUE;
  Redraw;
end;

procedure TFrameBGLSequencer.BGLVirtualScreen1MouseLeave(Sender: TObject);
begin
  FMouseCursorVisible:=FALSE;
  Redraw;
end;

procedure TFrameBGLSequencer.BGLVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FUserIsDragingStep then begin // drag step
    LoopUserDragStep;
  end
  else if not FUserIsDragingStep and not FUserChangeStepDuration and not FUserScrollTheView then begin
    if (StepUnderMouse <> NIL) or IsInTimeArea(Y) then BGLVirtualScreen1.Cursor := crHandPoint
      else BGLVirtualScreen1.Cursor := crDefault;
   UpdateMouseCursorPosition;
   Redraw;
  end;
end;

procedure TFrameBGLSequencer.BGLVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbMiddle) then begin // end of scroll of the view
    FUserScrollTheView := FALSE;
    BGLVirtualScreen1.Cursor := crDefault;
    exit;
  end;

  if Button = mbLeft then begin
    FUserIsDragingStep := FALSE;
    FUserDoSelection := FALSE;
    FUserChangeStepDuration := FALSE;
  end;

//  if Button = mbRight then begin
    if IsInTimeArea(Y) then
      DoTimeAreaClickEvent(Button, Shift, AbscissaToTimePos(X)+View_BeginTime)
    else
      if StepUnderMouse = NIL then DoEmptyAreaClickEvent(Button, Shift, AbscissaToTimePos(X)+View_BeginTime);
//  end;
end;

procedure TFrameBGLSequencer.BGLVirtualScreen1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var pps, TimePosTargetedByMouse: single;
  xmouse: LongInt;
begin
  //call first TFrameBGLSequencer.DoMouseWheel
  Handled := DoMouseWheel(Shift, WheelDelta, MousePos);
  // exit if done
  if Handled = TRUE then exit;

  xmouse := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).x;
  TimePosTargetedByMouse := AbscissaToTimePos(xmouse) + View_BeginTime;

  pps := PixelPerSecond;
  if WheelDelta < 0
    then pps := pps - pps * 0.2
    else pps := pps + pps * 0.2;
  PixelPerSecond := pps;

  // shift the view according to TimePosTargetedByMouse and xmouse
  // at 'xmouse' one want the time 'TimePosTargetedByMouse'
  View_BeginTime := View_BeginTime - (AbscissaToTimePos(xmouse) + View_BeginTime-TimePosTargetedByMouse);

  Redraw;
  handled := TRUE;
end;

procedure TFrameBGLSequencer.BGLVirtualScreen1Redraw(Sender: TObject; BGLContext: TBGLContext);
var i: integer;
    xx, yy, beginTime, endtime: single;
    txt: string;
    step: TCustomSequencerStep;
begin
 if not BGLVirtualScreen1.MakeCurrent(false) then
   exit;

  FInvalidateAlreadySent := FALSE;

  if FOpenGLObjectsNeedToBeReconstruct then begin
    FOpenGLObjectsNeedToBeReconstruct := False;
    DeleteOpenGLObjects;
    CreateOpenGLObjects;
  end;

  // update steps Y coordinates
  if FNeedStepsTopUpdate then begin
    FNeedStepsTopUpdate := False;
    UpdateStepsTop;
  end;
  // update steps width
  if FNeedStepsWidthUpdate then
  begin
    FNeedStepsWidthUpdate := FALSE;
    UpdateStepsWidth;
  end;

  with BGLContext.Canvas do
  begin
    if bglsAlternateColorForVStepPosition in FOptions then
    begin
      // render the horizontal line in two color
      xx := GetStepHeight;
      yy:=0;
      i:=0;
      while yy<Height do
      begin
        if i mod 2 = 0 then
          FillRect(0, yy, Width, yy+xx, ColorBackground)
        else
          FillRect(0, yy, Width, yy+xx, FColorBackground1);
        yy:=yy+xx;
        inc(i);
      end;
    end;

    // time scale background
    if bglsTimeAreaVisible in FOptions then
      FillRect(TimeArea, ColorTimeArea);

    // selection area
    if AnAreaIsSelected then
      FillRect( TimePosToAbscissa( FTimeSelectionLow ), 0,
                TimePosToAbscissa( FTimeSelectionHigh ), BGLContext.Canvas.Height,
                FColorUserAreaSelection);

    yy := TimeBase;

    if bglsTimeAreaVisible in FOptions then begin
      // abscissa axis
      Line(0, yy, BGLContext.Canvas.Width, yy, FColorXAxis);
      Line(0, yy+1, BGLContext.Canvas.Width, yy+1, FColorXAxis);
      // graduations and time scale
      beginTime := FBeginTimeGraduation;
      endtime := View_EndTime;
      repeat
       xx := TimePosToAbscissa(beginTime);
       Line(xx, yy-10, xx, yy, FColorXAxis);
       FTimeFont.TextOut( xx, yy-10-FTimeFont.FullHeight, TimeToString( beginTime ), FColorTimeLegend);

       // small graduations
       for i:=1 to FCounterSmallGraduation do
       begin
         xx := TimePosToAbscissa(beginTime+i*FDeltaTimeGraduation/FCounterSmallGraduation);
         Line( xx, yy-3, xx, yy, FColorXAxis);
       end;

       beginTime := beginTime + FDeltaTimeGraduation;
      until  beginTime > endtime;
    end;

    if bglsStepVerticalLineVisible in FOptions then begin
      // render vertical line for steps time position
      for step in FStepList do begin
        xx := TimePosToAbscissa(step.TimePos);
        if (xx >= 0) and (xx < Width) then
          Line( xx, step.Top+FStepFont.FullHeight, xx, yy, BGRA(100,100,100));
      end;
    end;

    // render steps
    for step in FStepList do step.ReDraw(Self, BGLContext);

    // Mouse cursor
    yy := TimeBase;
    if FMouseCursorVisible then
    begin
      Line( FMouseCursorX, yy-10-FTimeFont.FullHeight, FMouseCursorX, BGLVirtualScreen1.ClientHeight, BGRA(255,255,255,180));
      txt := TimeToString(AbscissaToTimePos(FMouseCursorX)+View_BeginTime);
      xx := FMouseCursorX-trunc(FTimeFont.TextWidth(txt)) div 2;
      xx := EnsureRange( xx, 0, BGLVirtualScreen1.ClientWidth-trunc(FTimeFont.TextWidth(txt)));
      FTimeFont.TextOut(xx, yy-10-FTimeFont.FullHeight*2, txt, BGRA(255,255,255,180));
    end;

    // Play cursor
    if FPlayCursorVisible then
    begin
      xx := TimePosToAbscissa(FPlayCursorTimePos);
      Line( xx, 0, xx, BGLVirtualScreen1.ClientHeight, BGRA(0,255,255,190));
      txt := TimeToString(FPlayCursorTimePos);
      xx := xx-trunc(FTimeFont.TextWidth(txt)) div 2;
      xx := EnsureRange( xx, 0, BGLVirtualScreen1.ClientWidth-trunc(FTimeFont.TextWidth(txt)));
      FTimeFont.TextOut(xx, yy-10-FTimeFont.FullHeight*2, txt, BGRA(0,255,255,190));
    end;
  end;
end;

procedure TFrameBGLSequencer.StepMouseDown(Sender: TCustomSequencerStep;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FCTRLPressed := ssCtrl in Shift;
  FALTPressed := ssAlt in Shift;

  if (Button = mbRight) and not FCTRLPressed and not FALTPressed then begin
    if not Sender.Selected then begin
      Sel_SelectNone;
      InternalSel_SetSelected(Sender, TRUE);
      DoSelectionChangeEvent;
      Redraw;
    end;
  end;

  if (Button = mbLeft) and not FALTPressed then begin // sélectionne le label ou le groupe, et mémorise la position de la souris pour un éventuel DRAG
    // prepare the drag operation
    FMousePosOrigin := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
    FTimePosOrigin := Sender.TimePos;
    FUserIsDragingStep := TRUE;
    if not FCTRLPressed then begin
      // update selected state
      if not (ssShift in Shift) then begin
        if not Sender.Selected then
          Sel_SelectNone;
        InternalSel_SetSelected(Sender, TRUE);
      end else InternalSel_ToggleSelected(Sender);
      UpdateSelectedArray;
      Redraw;
      DoSelectionChangeEvent;
    end;
  end;

  // change the duration of the step
  if (Button = mbLeft) and FALTPressed and (Sender.Duration > 0) and
     not FUserChangeStepDuration then begin
    if not Sender.Selected then begin
      Sel_SelectNone;
      InternalSel_SetSelected(Sender, TRUE);
    end;
    LoopUserSetStepDuration;
    FALTPressed := FALSE;
  end;

  DoStepClickEvent(Sender, Button, Shift);
end;

function TFrameBGLSequencer.GetStepHeight: integer;
begin
  if (FTimeFont <> NIL) and (FStepFont <> NIL)
   then Result := FTimeFont.FullHeight * 2 + FStepFont.FullHeight
   else Result := FStepFontHeight + 11 * 2;
end;

function TFrameBGLSequencer.AdjustYStepIntoLine(aY: integer): integer;
var hStep, ymax: integer;
begin
  hStep := StepHeight;
  ymax := StepArea.Bottom - hStep;
  Result := EnsureRange(aY, 0, ymax);

  if bglsForceVStepPosition in FOptions then begin
    Result := Result div hStep;
    if Result >= VerticalLineCount then Result := VerticalLineCount - 1;
    Result := Result * hStep;
  end;
end;

function TFrameBGLSequencer.DoNeedErrorSymbolCallback: TBGRABitmap;
begin
  if FOnNeedErrorSymbol <> NIL then Result := FOnNeedErrorSymbol(Self)
    else Result := NIL;
end;

procedure TFrameBGLSequencer.ComputeGraduationsParameters;
begin
 if PixelPerSecond>800 then begin
   FDeltaTimeGraduation := 0.100;
   FBeginTimeGraduation := trunc(View_BeginTime);
   FCounterSmallGraduation := 10;
 end else
 if PixelPerSecond>300 then begin
   FDeltaTimeGraduation := 0.500;
   FBeginTimeGraduation := trunc(View_BeginTime);
   FCounterSmallGraduation := 5;
 end else
 if PixelPerSecond>100 then begin
   FDeltaTimeGraduation := 1.000;
   FBeginTimeGraduation := trunc(View_BeginTime);
   FCounterSmallGraduation := 5;
 end else
 if PixelPerSecond>50 then begin
   FDeltaTimeGraduation := 5.000;
   FBeginTimeGraduation := trunc(View_BeginTime/10)*10;
   FCounterSmallGraduation := 5;
 end else
 if PixelPerSecond>8 then begin
   FDeltaTimeGraduation := 10.000;
   FBeginTimeGraduation := trunc(View_BeginTime/100)*100;
   FCounterSmallGraduation := 5;
 end else
 if PixelPerSecond>4 then begin
   FDeltaTimeGraduation := 30.000;
   FBeginTimeGraduation := trunc(View_BeginTime/100)*100;
   FCounterSmallGraduation := 5;
 end else
 if PixelPerSecond>1.5 then begin
   FDeltaTimeGraduation := 60.000;
   FBeginTimeGraduation := trunc(View_BeginTime/100)*100;
   FCounterSmallGraduation := 5;
 end else
 if PixelPerSecond>0.3 then begin
   FDeltaTimeGraduation := 60.000*5;
   FBeginTimeGraduation := trunc(View_BeginTime/1000)*1000;
   FCounterSmallGraduation := 5;
 end else
  if PixelPerSecond>0.1 then begin
    FDeltaTimeGraduation := 60.000*30;
    FBeginTimeGraduation := trunc(View_BeginTime/10000)*10000;
    FCounterSmallGraduation := 5;
  end else
   if PixelPerSecond>0.015 then begin
     FDeltaTimeGraduation := 3600.000;
     FBeginTimeGraduation := trunc(View_BeginTime/10000)*10000;
     FCounterSmallGraduation := 5;
  end else begin
    FDeltaTimeGraduation := 86400.000;
    FBeginTimeGraduation := trunc(View_BeginTime/100000)*100000;
    FCounterSmallGraduation := 5;
  end;
end;

procedure TFrameBGLSequencer.DoSelectionChangeEvent;
begin
  if FOnSelectionChange <> NIL
    then FOnSelectionChange(Sel_LastStepSelected);
end;

procedure TFrameBGLSequencer.DoViewChangeEvent;
begin
  if FOnViewChange <> NIL
    then FOnViewChange(Self);
end;

procedure TFrameBGLSequencer.DoTimeAreaClickEvent(Button: TMouseButton;
  Shift: TShiftState; TimePos: single);
begin
 if FOnTimeAreaClick <> NIL
   then FOnTimeAreaClick(Self, Button, Shift, TimePos);
end;

procedure TFrameBGLSequencer.DoEmptyAreaClickEvent(Button: TMouseButton;
  Shift: TShiftState; TimePos: single);
begin
  if FOnEmptyAreaClick <> NIL
    then FOnEmptyAreaClick(Self, Button, Shift, TimePos);
end;

procedure TFrameBGLSequencer.DoStepClickEvent(aStep: TCustomSequencerStep;
  Button: TMouseButton; Shift: TShiftState);
begin
  if FOnStepClick <> NIL
    then FOnStepClick(aStep, Button, Shift, aStep.TimePos);
end;

function TFrameBGLSequencer.DoDuplicateStepEvent(aStep: TCustomSequencerStep ): TCustomSequencerStep;
begin
  if FOnDuplicateStep <> NIL then
    Result := FOnDuplicateStep(aStep);
end;

function TFrameBGLSequencer.DoMergeStepEvent: TCustomSequencerStep;
begin
  Result := NIL;
  if FOnMergeStep<>NIL
    then Result := FOnMergeStep()
    else Raise Exception.Create('TFrame_Sequencer: OnMergeStep is not assigned and you attempts to merge.');
end;

procedure TFrameBGLSequencer.DoMoveStepEvent;
begin
  if FOnMoveStep<>NIL then FOnMoveStep(Self);
end;

procedure TFrameBGLSequencer.DoUserChangeDurationEvent;
begin
  if FOnUserChangeStepDuration<>NIL then FOnUserChangeStepDuration(Self);
end;

function TFrameBGLSequencer.GetID: integer;
begin
  Result := FStepList.ID;
end;

function TFrameBGLSequencer.VerticalLineCount: integer;
begin
  Result := StepArea.Height div StepHeight;
end;

function TFrameBGLSequencer.StepArea: TRect;
begin
 Result.Create(Point(0, 0), BGLVirtualScreen1.Width, BGLVirtualScreen1.Height-ScaleDesignToForm(60) );
end;

function TFrameBGLSequencer.TimeArea: TRect;
begin
  Result.Create(Point(0, BGLVirtualScreen1.Height-ScaleDesignToForm(60)), BGLVirtualScreen1.Width, ScaleDesignToForm(60));
end;

function TFrameBGLSequencer.TimeBase: integer;
begin
  Result := TimeArea.Bottom - ScaleDesignToForm(23);
end;

function TFrameBGLSequencer.IsInTimeArea(aY: integer): boolean;
begin
  Result := (aY >= TimeArea.Top) and (bglsTimeAreaVisible in FOptions);
end;

function TFrameBGLSequencer.IsInStepArea(aY: integer): boolean;
begin
  Result :=(aY >= TimeArea.Top) and (ay < TimeArea.Bottom);
end;

procedure TFrameBGLSequencer.SetID(AValue: integer);
begin
  FStepList.ID := AValue;
end;

procedure TFrameBGLSequencer.LoopUserDragStep;
var
 xMouse, XscrollTriggerLeft, XscrollTriggerRight: integer;
 deltaTime, delta: single;
 stepMoved, _notified, canDuplicate: boolean;
begin
  if FAlreadyInDragLoop then exit;
  FAlreadyInDragLoop := TRUE;
  BGLVirtualScreen1.Cursor := crDrag;

  XscrollTriggerLeft := 1;
  XscrollTriggerRight := BGLVirtualScreen1.ClientWidth - 2;

  _notified := False;
  stepMoved := False;
  canDuplicate := False;
  repeat
   xMouse := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).X;
   deltaTime := AbscissaToTimePos(xMouse-FMousePosOrigin.X);
   if FCTRLPressed then begin
     // before duplication, user must move the mouse at least 20 pixels horizontally
     canDuplicate := Abs(xMouse-FMousePosOrigin.X) >= ScaleDesignToForm(20);
   end else begin
     FMousePosOrigin.X := xMouse;
   end;

   if deltaTime <> 0 then begin
     if FCTRLPressed and canDuplicate then begin // CTRL pressed = duplicate the selected steps
       Sel_DuplicateAndGroup;
       _notified := TRUE;
       FCTRLPressed := False;
     end else if not _notified then begin
       _notified := TRUE;
       Notify(Selected, snChanged, NotifyMoveMessage);
     end;

     if not FCTRLPressed then begin
       Sel_DragSelection(deltaTime);
       Redraw;
       stepMoved := TRUE;
     end;
   end;

   // shift the view if needed
   if not (bglsKeepTimeOriginVisible in FOptions) then
   begin
    delta := 0;
    if xMouse < XscrollTriggerLeft
      then delta := -1/PixelPerSecond*5
      else if xMouse > XscrollTriggerRight
      then delta := 1/PixelPerSecond*5;
    if delta<>0 then begin
     View_BeginTime := View_BeginTime + delta;
     Sel_DragSelection( delta );
    end;
   end;

   Application.ProcessMessages;
  until not FUserIsDragingStep;

  FStepList.Sort;

  Redraw;
  BGLVirtualScreen1.Cursor := crDefault;
  FCTRLPressed := FALSE;
  FAlreadyInDragLoop := FALSE;
  if stepMoved then DoMoveStepEvent;
end;

procedure TFrameBGLSequencer.LoopUserSetStepDuration;
var
 xorigin, xMouse, i: integer;
 deltaTime: single;
 _notified: boolean;
begin
  FUserChangeStepDuration := TRUE;
  _notified := FALSE;

  xorigin := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).X;
  repeat
   xMouse := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).x;
   deltaTime := AbscissaToTimePos(xMouse-xorigin);
   if deltaTime <> 0 then begin
     if not _notified then begin
       _notified := TRUE;
       Notify(Selected, snChanged, NotifyChangeDurationMessage);
     end;
     xorigin := xMouse;
     for i:=0 to High(Selected) do
       if (Selected[i].Duration > 0) and (Selected[i].Duration+deltaTime > 0)
         then Selected[i].Duration := Selected[i].Duration+deltaTime;
     Redraw;
   end;
   Sleep(1);
   Application.ProcessMessages;
  until not FUserChangeStepDuration;

  if _notified then DoUserChangeDurationEvent;
end;

procedure TFrameBGLSequencer.LoopUserDoSelection;
var
 i, xMouse, XscrollTriggerLeft, XscrollTriggerRight: integer;
 oldLow, oldHigh, delta, timeorigin, timemouse: single;
 redraw_flag: boolean ;
begin
  FUserDoSelection := TRUE;
  BGLVirtualScreen1.Cursor := crHSplit;

  timeorigin := AbscissaToTimePos(BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).X)+View_BeginTime;
  FTimeSelectionLow := timeorigin;
  FTimeSelectionHigh := FTimeSelectionLow;

  XscrollTriggerLeft := 0;
  XscrollTriggerRight := BGLVirtualScreen1.ClientWidth-1;

  repeat
   redraw_flag := FALSE;
   oldLow := FTimeSelectionLow;
   oldHigh := FTimeSelectionHigh;

   // update the selection boundaries
   xMouse := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).x;
   timemouse := AbscissaToTimePos(xMouse)+View_BeginTime;
   if timemouse>timeorigin then begin
     FTimeSelectionHigh := timemouse;
     FTimeSelectionLow := timeorigin;
   end else begin
     FTimeSelectionHigh := timeorigin;
     FTimeSelectionLow := timemouse;
   end;
   redraw_flag := (FTimeSelectionLow<>OldLow) or (FTimeSelectionHigh<>oldHigh);

   // scroll the view if the mouse is near the boundaries of the screen
   if not (bglsKeepTimeOriginVisible in FOptions) then
   begin
    delta := 0;
    if xMouse <= XscrollTriggerLeft
      then delta := -1/PixelPerSecond*5
      else if xMouse >= XscrollTriggerRight
         then delta := 1/PixelPerSecond*5;
    if delta<>0 then begin
      redraw_flag := TRUE;
      View_BeginTime := View_BeginTime + delta;
    end;
   end;

   if redraw_flag then Redraw;
   Application.ProcessMessages;
   sleep(1);
  until not FUserDoSelection;

  // on recherche les étapes inclusent dans la sélection et on les sélectionne
  for i:=0 to FStepList.Count-1 do
   if (FStepList[i].TimePos>=FTimeSelectionLow) and (FStepList[i].TimePos<=FTimeSelectionHigh) then begin
            InternalSel_SetSelected(FStepList[i], TRUE);
   end else begin  // on désélectionne l'étape
            InternalSel_SetSelected(FStepList[i], FALSE);
   end;
  UpdateSelectedArray;

  DoSelectionChangeEvent;
  BGLVirtualScreen1.Cursor := crDefault;
  Redraw;
  FCTRLPressed := FALSE;
end;

procedure TFrameBGLSequencer.LoopUserScrollTheViewWithMiddleMouseButton;
var deltaTime: single;
    p: TPoint;
    xorigine: LongInt;
begin
  FUserScrollTheView := TRUE;
  BGLVirtualScreen1.Cursor := crSizeWE;
  xorigine := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).X;
  repeat
   p := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
   // scroll the view : deltaTime is relative to mouse position
   deltaTime := (p.x - xorigine) / PixelPerSecond * 0.05;
   View_BeginTime := View_BeginTime+deltaTime;
   Application.ProcessMessages;
  until not FUserScrollTheView;
  BGLVirtualScreen1.Cursor := crDefault;
end;

procedure TFrameBGLSequencer.SetStepFontHeight(AValue: integer);
begin
  if FStepFontHeight = AValue then Exit;

  FStepFontHeight := AValue;
  FOpenGLObjectsNeedToBeReconstruct := True;
  Redraw;
end;

procedure TFrameBGLSequencer.CreateOpenGLObjects;
var ima: TBGRABitmap;

begin
  FStepFont := BGLFont('Arial', FStepFontHeight, []);
  FStepFont.Quality := fqSystemClearType;

  FTimeFont := BGLFont('Arial', 11, []);
  FTimeFont.Quality := fqSystemClearType;

  if FTextureErrorSymbol = NIL then begin
    ima := DoNeedErrorSymbolCallback;
    if ima <> NIL then begin
      FTextureErrorSymbol := BGLTexture(ima);
      ima.Free;
    end;
  end;
end;

procedure TFrameBGLSequencer.DeleteOpenGLObjects;
begin
  FStepFont := NIL;
  FTimeFont := NIL;
  FTextureErrorSymbol := NIL;
end;

procedure TFrameBGLSequencer.ForceReconstructOpenGLObjects;
begin
  FOpenGLObjectsNeedToBeReconstruct := True;
  Redraw;
end;

procedure TFrameBGLSequencer.UpdateStepsWidth;
var step: TCustomSequencerStep;
begin
  for step in FStepList do
    step.UpdateWidth;
end;

procedure TFrameBGLSequencer.UpdateStepsTop;
var step: TCustomSequencerStep;
begin
  for step in FStepList do
    step.Top := AdjustYStepIntoLine(step.Top); // ForceStepTopToBeInStepArea(step.Top);
end;

function TFrameBGLSequencer.StepUnderMouse: TCustomSequencerStep;
var i: integer;
  step: TCustomSequencerStep;
  p: TPoint;
  xx, yy: integer;
begin
  Result:=NIL;

  if not Assigned(FStepFont) then exit;

  p := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
  for i:= FStepList.Count-1 downto 0 do begin
    step := FStepList[i];
    xx := TimePosToAbscissa(step.TimePos);
    yy := step.Top+FStepFont.FullHeight;    //FStepFontHeight
    if InRange( p.X, xx, xx+step.Width ) and
       InRange( p.Y, yy, yy+FStepFont.FullHeight) then begin
         Result := step;
         exit;
       end;
  end;
end;

function TFrameBGLSequencer.ScreenToTimePos(aP: TPoint): single;
begin
  aP := BGLVirtualScreen1.ScreenToClient(aP);
  Result := AbscissaToTimePos(aP.X) + View_BeginTime;
end;

function TFrameBGLSequencer.XMouseToTime: single;
var p: TPoint;
begin
  p := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
  Result := AbscissaToTimePos(p.X) + View_BeginTime;
end;

function TFrameBGLSequencer.MouseIsOverSequencer: boolean;
var p: TPoint;
begin
  p := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos);
  Result := BGLVirtualScreen1.BoundsRect.Contains(p);
end;

function TFrameBGLSequencer.YLineUnderMouse: integer;
begin
  Result := (BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).Y div StepFontHeight) * StepFontHeight;
end;

procedure TFrameBGLSequencer.UpdateMouseCursorPosition;
begin
  FMouseCursorX := BGLVirtualScreen1.ScreenToClient(Mouse.CursorPos).X;
end;

procedure TFrameBGLSequencer.DoOnClipBoardChange;
begin
  if FOnClipBoardChange <> NIL
    then FOnClipBoardChange(self);
end;

procedure TFrameBGLSequencer.DoCopySelectionToClipBoard( aDeleteTheCopied: boolean);
var timeZero: single;
  s, sd: TCustomSequencerStep;
  i: LongInt;
begin
  if SelectedCount = 0 then exit;

  if aDeleteTheCopied then Notify(Selected, snDeleted, NotifyCutMessage);

  FClipBoard.Clear;
  timeZero := Sel_FirstStepSelected.TimePos;
  for i:=StepList.Count-1 downto 0 do begin
    s := StepList.Items[i];
    if s.Selected then begin
      if aDeleteTheCopied then begin
        // CUT -> remove the step from sequencer and push it to the clipboard
          FClipBoard.Insert(0, s);
          StepList.Remove(s);
          s.TimePos := s.TimePos-timeZero;
      end else begin
        // COPY -> duplicate the step and push it to the clipboard
          sd := DoDuplicateStepEvent( s );
          sd.TimePos := sd.TimePos-timeZero;
          FClipBoard.Insert(0, sd);
      end;
    end;
  end;
end;

function TFrameBGLSequencer.GetView_EndTime: single;
begin
  Result := View_BeginTime + AbscissaToTimePos(BGLVirtualScreen1.ClientWidth);
end;

procedure TFrameBGLSequencer.SetPixelPerSecond(AValue: single);
var modifiedClientWidth: single;
begin
// if AValue=0 then exit;
  if FPixelPerSecond = AValue then Exit;

  modifiedClientWidth := BGLVirtualScreen1.ClientWidth*0.95;
  FPixelPerSecond := EnsureRange(AValue,
                                 modifiedClientWidth/SEQUENCER_VIEW_ZOOM_MAX,
                                 modifiedClientWidth/SEQUENCER_VIEW_ZOOM_MIN);
// FPixelPerSecond:=AValue;
  ComputeGraduationsParameters;
  FNeedStepsWidthUpdate := TRUE;
  UpdateScrollBar;
 //UpdateScrollBarRange;

  DoViewChangeEvent;
end;

procedure TFrameBGLSequencer.SetView_BeginTime(AValue: single);
begin
  if bglsKeepTimeOriginVisible in FOptions then
    AValue := 0;

  if AValue = FView_BeginTime then exit;
  if FPixelPerSecond = 0 then exit;

  if AValue+(BGLVirtualScreen1.ClientWidth*0.95)/FPixelPerSecond > SEQUENCER_VIEW_MAX_SECONDS
    then AValue := SEQUENCER_VIEW_MAX_SECONDS-(BGLVirtualScreen1.ClientWidth*0.95)/FPixelPerSecond;
  if AValue < 0 then AValue := 0;
  FView_BeginTime := AValue;
  UpdateScrollBar;
  ComputeGraduationsParameters;
  Redraw;
  DoViewChangeEvent;
end;

procedure TFrameBGLSequencer.Sel_DragSelection(deltaTime: single);
var s: TCustomSequencerStep;
begin
  s := Sel_FirstStepSelected;
  if s = NIL then exit;

  // Timepos can not be negative...
  if s.TimePos+deltaTime < 0
    then deltaTime := -s.TimePos;
  if deltaTime = 0 then exit;

  for s in FStepList do
   if s.Selected
     then if s.CanApplyTimeOffset(deltaTime)
            then RawApplyTimeOffsetOnStep(s, deltaTime);

// FStepList.Sort;
end;

procedure TFrameBGLSequencer.UpdateScrollBar;
var last: TCustomSequencerStep;
begin
  ScrollBar1.Position := Round( View_BeginTime*100 );
  //update scrollBar range
  last:=GetLastStep;
  if last<>NIL then begin
    ScrollBar1.Max := Round(Abs(last.TimePos+last.Duration)*1.02*100);
    ScrollBar1.PageSize := Round(BGLVirtualScreen1.Width/FPixelPerSecond)*100;
  end else begin
    ScrollBar1.Max := 100;
    ScrollBar1.PageSize := 100;
  end;
end;

procedure TFrameBGLSequencer.NeedStepsWidthUpdate;
begin
  FNeedStepsWidthUpdate := TRUE;
end;

procedure TFrameBGLSequencer.NeedStepsTopUpdate;
begin
  FNeedStepsTopUpdate := TRUE;
end;

procedure TFrameBGLSequencer.UpdateSelectedArray;
var c: integer;
  s: TCustomSequencerStep;
begin
  SetLength(Selected, FSelectedCount);
  if FSelectedCount=0 then exit;
  c := 0;
  for s in StepList do
   if s.Selected then begin
     if c>=FSelectedCount
        then Raise Exception.Create('TFrameBGLSequencer error: FSelectedCount is not valid...');
    Selected[c] := s;
    inc(c);
   end;
end;

constructor TFrameBGLSequencer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FStepList := TStepList.Create;
  FClipBoard := TStepList.Create;
  FGroupValue := 0;
  FPixelPerSecond := 100;
  ComputeGraduationsParameters;
  FDuplicateValue := 0;
  FStepFontHeight := 13;
  FOptions := [bglsStepVerticalLineVisible, bglsTimeAreaVisible];
  FColorBackground1 := BGRA(40,40,40);
  FColorTimeArea := BGRA(5,5,5);
  FColorTimeLegend := BGRA(180,180,180);
  FColorUserAreaSelection := BGRA(50,100,255,70);//BGRA(32,52,52);
  FColorXAxis := BGRA(100,100,0);
  {$ifdef MSWINDOWS}
  BGLVirtualScreen1.MultiSampling := 2;
  {$endif}

  FOpenGLObjectsNeedToBeReconstruct := True;
end;

destructor TFrameBGLSequencer.Destroy;
begin
  Clear;
  FStepFont := NIL;
  FTimeFont := NIL;
  FStepList.Free;
  ClipBoard_Clear;
  FClipBoard.Free;
  inherited Destroy;
end;

procedure TFrameBGLSequencer.NotifyRange(aStartIndex, aEndIndex: integer;
  aAction: TSequencerNotification; const aDescription: string);
var A: ArrayOfCustomSequencerStep;
    i, k: integer;
begin
  if aStartIndex > aEndIndex then begin
    i := aEndIndex;
    aEndIndex := aStartIndex;
    aStartIndex := i;
  end;

  A := NIL;
  SetLength(A, aEndIndex-aStartIndex+1);
  k := 0;
  for i:=aStartIndex to aEndIndex do begin
    A[k] := StepList[i];
    inc(k);
  end;

  Notify(A, aAction, aDescription);
end;

procedure TFrameBGLSequencer.Notify(const aSteps: ArrayOfCustomSequencerStep;
  aAction: TSequencerNotification; const aDescription: string);
begin
  if FOnNotify <> NIL
    then FOnNotify(aSteps, aAction, aDescription);
end;

procedure TFrameBGLSequencer.Redraw;
begin
  if not FInvalidateAlreadySent then begin
    BGLVirtualScreen1.Invalidate;
    FInvalidateAlreadySent := TRUE;
  end;
end;

procedure TFrameBGLSequencer.Clear;
begin
  while FStepList.Count > 0 do
    FStepList.ExtractIndex(0).Free;
  FStepList.ResetID;

  FSelectedCount := 0;
  ForceNoAreaSelected;
  if not(csDestroying in ComponentState) then begin
    Redraw;
    DoSelectionChangeEvent;
  end;
end;

procedure TFrameBGLSequencer.Add(aStep: TCustomSequencerStep; aSetSelected: boolean);
begin
  RawAdd(aStep, aSetSelected);
  FStepList.Sort;
  if aSetSelected then DoSelectionChangeEvent;
  Notify([aStep], snAdded, NotifyInsertMessage);
end;

procedure TFrameBGLSequencer.RawAdd(aStep: TCustomSequencerStep; aSetSelected: boolean; aSetID: boolean);
var curID: integer;
begin
  curID := aStep.ID;

  aStep.Top:=AdjustYStepIntoLine(aStep.Top);
  FStepList.Add(aStep);
  aStep.ParentSeq := self;

  if not aSetID then begin
    aStep.ID := curId;
    FStepList.ID := FStepList.ID-1;
  end;


  if aSetSelected then begin
    aStep.Selected := TRUE;
    inc(FSelectedCount);
    SetLength(Selected, FSelectedCount);
    Selected[FSelectedCount-1] := aStep;
  end;
end;

procedure TFrameBGLSequencer.RawDelete(aStep: TCustomSequencerStep);
var i: LongInt;
begin
  i := StepList.IndexOf(aStep);
  RawDelete(i);
end;

procedure TFrameBGLSequencer.Delete(aStep: TCustomSequencerStep; aDoOnSelectionChange: boolean);
var i: integer;
  flag: boolean;
begin
  if aStep.Selected then begin
   dec(FSelectedCount);
   flag := TRUE;
  end else flag := FALSE;

  aStep.Selected := FALSE;
  Notify([aStep], snDeleted, NotifyDeleteMessage);

  i := FStepList.IndexOf(aStep);
  FStepList.ExtractIndex(i).Free;
  if flag then UpdateSelectedArray;

  if aDoOnSelectionChange then begin
    DoSelectionChangeEvent;
    Redraw;
  end;
end;

procedure TFrameBGLSequencer.RawDelete(aIndex: integer);
begin
  with StepList[aIndex] do begin
   if Selected then begin
     dec(FSelectedCount);
   end;
   StepList[aIndex].Free;
   StepList.Delete(aIndex);
  end;
end;

procedure TFrameBGLSequencer.RawDeleteStepByID(aID: integer);
var i: LongInt;
begin
  i := StepList.IndexOfID(aID);
  if i <> -1
    then RawDelete(i);
end;

procedure TFrameBGLSequencer.RawReplaceStepByID( aReplacement: TCustomSequencerStep);
var i: integer;
  old: TCustomSequencerStep;
  flagSelectionChange: boolean;
begin
  i := StepList.IndexOfID(aReplacement.ID);
  if i <> -1 then begin
    flagSelectionChange := FALSE;
    old := StepList[i];
    if old.Selected then begin
      dec(FSelectedCount);
      flagSelectionChange := TRUE;
    end;

    aReplacement.Top := old.Top;
    aReplacement.ParentSeq := old.ParentSeq;
    aReplacement.UpdateWidth;

    StepList[i] := aReplacement;
    old.Free;
    if aReplacement.Selected then begin
     inc(FSelectedCount);
     flagSelectionChange:=TRUE;
    end;
    if flagSelectionChange
      then UpdateSelectedArray;
  end;
end;

procedure TFrameBGLSequencer.RawApplyTimeOffsetOnStep( aStep: TCustomSequencerStep; TimeOffset: single);
begin
  if aStep.CanApplyTimeOffset(TimeOffset) then
    aStep.TimePos := aStep.TimePos + TimeOffset;
end;

procedure TFrameBGLSequencer.ClipBoard_CutSelection;
begin
  DoCopySelectionToClipBoard(True);
  Sel_SelectNone;
  Redraw;
end;

procedure TFrameBGLSequencer.ClipBoard_CopySelection;
begin
  DoCopySelectionToClipBoard(False);
end;

procedure TFrameBGLSequencer.ClipBoard_PasteTo(aTimePos: single);
var i: integer;
  s: TCustomSequencerStep;
  A: ArrayOfCustomSequencerStep;
begin
  A := NIL;
  SetLength(A, FClipBoard.Count);

  Sel_SelectNone;
  for i:=0 to FClipBoard.Count-1 do begin
   s := DoDuplicateStepEvent(FClipBoard.Items[i]);
   s.TimePos:=s.TimePos+aTimePos;
   RawAdd(s, TRUE);
   A[i] := s;
  end;
  Notify(A, snAdded, NotifyPasteMessage);
  NeedStepsWidthUpdate;
  Redraw;
end;

procedure TFrameBGLSequencer.ClipBoard_Clear;
begin
  while FClipBoard.Count > 0 do
    FClipBoard.ExtractIndex(0).Free;
end;

function TFrameBGLSequencer.Clipboard_HasData: boolean;
begin
  Result := FClipBoard.Count <> 0;
end;

procedure TFrameBGLSequencer.Sel_DuplicateAndGroup;
var i, c: integer;
  s: TCustomSequencerStep;
begin
  c := 0;
  for i:=FStepList.Count-1 downto 0 do
   if FStepList[i].Selected then begin
     FStepList[i].Selected := FALSE ; // unselect
     s := DoDuplicateStepEvent(FStepList[i]); // duplicate
     if s <> NIL then begin
       s.TimePos := FStepList[i].TimePos;
       s.ParentSeq := self;
       FStepList.Add(s);
       Selected[c] := s;
       inc(c);
       s.Selected := TRUE;  // re-select
     end;
  end;
  if c > 1 then begin
    FStepList.Sort;
    InternalSel_Group; // group selected
  end;
  NeedStepsWidthUpdate;
  DoSelectionChangeEvent;
  Notify(Selected, snAdded, NotifyDuplicateMessage);
end;

procedure TFrameBGLSequencer.Sel_Merge;
var replacementStep: TCustomSequencerStep;
begin
  if SelectedCount < 2 then exit;

  replacementStep := DoMergeStepEvent();
  if replacementStep = NIL then exit;

  replacementStep.TimePos := Sel_FirstStepSelected.TimePos;

  Sel_Delete;

  Add(replacementStep, FALSE);
end;

procedure TFrameBGLSequencer.Sel_Delete;
var i: integer;
begin
  if SelectedCount = 0 then exit;
  Notify(Selected, snDeleted, NotifyDeleteMessage);

  for i:=FStepList.Count-1 downto 0 do
   if FStepList[i].Selected
     then RawDelete(i);

  FSelectedCount := 0;
  Selected := NIL;

  DoSelectionChangeEvent;
  Redraw;
end;

procedure TFrameBGLSequencer.Sel_Group;
begin
  Notify(Selected, snChanged, NotifyGroupMessage);
  InternalSel_Group;
end;

procedure TFrameBGLSequencer.InternalSel_Group;
var i: integer;
begin
  inc(FGroupValue);
  for i:=0 to FSelectedCount-1 do
   Selected[i].Group := FGroupValue;
end;

procedure TFrameBGLSequencer.Sel_Ungroup;
var i: integer;
begin
  Notify(Selected, snChanged, NotifyUnGroupMessage);
  for i:=0 to FSelectedCount-1 do
    Selected[i].Group := 0;
end;

function TFrameBGLSequencer.Sel_CanVerticalShift(delta: integer): boolean;
var ymax: Integer;
  step: TCustomSequencerStep;
begin
  ymax := TimeArea.Top-StepFontHeight;
  Result := TRUE;
  for step in Selected do
    Result := Result and InRange(step.Top+delta,0, ymax);
end;

procedure TFrameBGLSequencer.Sel_VerticalShift(delta: integer);
var step: TCustomSequencerStep;
begin
  Notify(Selected, snChanged, NotifyVerticalShiftMessage);

  for step in Selected do
    step.Top := AdjustYStepIntoLine(step.Top + delta);
end;

procedure TFrameBGLSequencer.Sel_RecomputeVerticalStepsPosition;
var k, vLineCount: integer;
  step: TCustomSequencerStep;
begin
  Notify(Selected, snChanged, NotifyRearrangeMessage);
  vLineCount := VerticalLineCount;
  k := 0;
  for step in Selected do begin
     step.Top := (k mod vLineCount) * StepHeight;
     inc(k);
  end;
  Redraw;
end;

procedure TFrameBGLSequencer.RecomputeVerticalStepsPosition;
var k, vLineCount: integer;
  step: TCustomSequencerStep;
begin
  Sel_SelectAll;
  Notify( Selected, snChanged, NotifyRearrangeMessage);
  Sel_SelectNone;
  vLineCount := VerticalLineCount;
  k := 0;
  for step in FStepList do begin
     step.Top := ( k mod vLineCount ) * StepHeight;
     inc(k);
  end;
  Redraw;
end;

function TFrameBGLSequencer.Sel_FirstStepSelected: TCustomSequencerStep;
begin
  if SelectedCount = 0
    then Result := NIL
    else Result := Selected[0];
end;

function TFrameBGLSequencer.Sel_LastStepSelected: TCustomSequencerStep;
begin
  if SelectedCount = 0
    then Result := NIL
    else Result := Selected[SelectedCount-1];
end;

procedure TFrameBGLSequencer.Sel_SelectNone;
var s: TCustomSequencerStep;
begin
  for s in FStepList do
   s.Selected := FALSE;
  FSelectedCount := 0;
  Selected := NIL;
  ForceNoAreaSelected;
end;

procedure TFrameBGLSequencer.Sel_SelectAll;
var i: integer;
begin
  SetLength(Selected, FStepList.Count);
  for i:=0 to FStepList.Count-1 do begin
   FStepList[i].Selected := TRUE;
   Selected[i] := FStepList[i];
  end;
  FSelectedCount := FStepList.Count;
end;

procedure TFrameBGLSequencer.InternalSel_SetSelected(aStep: TCustomSequencerStep; SelectedState: boolean);
var d: Integer;
  s: TCustomSequencerStep;
begin
  if aStep.Selected = SelectedState then exit;

  if SelectedState then d := 1
    else d := -1;

  if aStep.Group=0 then begin
    // single
    aStep.Selected:=SelectedState;
    FSelectedCount+=d;
    UpdateSelectedArray;
  end else begin
    //group
    for s in FStepList do
     if s.Group = aStep.Group then begin
       s.Selected:=SelectedState;
       FSelectedCount+=d;
     end;
    UpdateSelectedArray;
  end;
end;

procedure TFrameBGLSequencer.InternalSel_ToggleSelected(aStep: TCustomSequencerStep);
var s: TCustomSequencerStep;
begin
  if aStep.Group = 0 then begin
    // single
    aStep.Selected := not aStep.Selected;
    if aStep.Selected
      then inc(FSelectedCount)
      else dec(FSelectedCount);
    UpdateSelectedArray;
  end else begin
    // group
     for s in FStepList do
      if s.Group = aStep.Group then begin
        s.Selected := not s.Selected;
        if s.Selected
          then inc(FSelectedCount)
          else dec(FSelectedCount);
      end;
      UpdateSelectedArray;
  end;
end;

function TFrameBGLSequencer.GetColorBackground: TColor;
begin
  Result := BGLVirtualScreen1.Color;
end;

procedure TFrameBGLSequencer.SetColorBackground(AValue: TColor);
begin
  BGLVirtualScreen1.Color:=AValue;
end;

function TFrameBGLSequencer.AnAreaIsSelected: boolean;
begin
  Result := (FTimeSelectionLow<FTimeSelectionHigh) and (FTimeSelectionLow>=0);
end;

procedure TFrameBGLSequencer.ForceNoAreaSelected;
begin
  FTimeSelectionLow := FTimeSelectionHigh
end;

procedure TFrameBGLSequencer.ForceAreaSelected(aTimeBegin, aTimeEnd: single);
begin
  if aTimeBegin < 0 then exit;
  if aTimeEnd < aTimeBegin then begin
   FTimeSelectionLow := aTimeEnd;
   FTimeSelectionHigh := aTimeBegin;
  end else begin
    FTimeSelectionLow := aTimeBegin;
    FTimeSelectionHigh := aTimeEnd;
  end;
end;

function TFrameBGLSequencer.StepCountInSelectedArea: integer;
var i: Integer;
begin
  Result := 0;
  for i:=0 to FStepList.Count-1 do
   if InRange( FStepList[i].TimePos, FTimeSelectionLow, FTimeSelectionHigh )
      then inc( Result );
end;

function TFrameBGLSequencer.SelectedAreaBeginTime: single;
begin
  if AnAreaIsSelected
    then Result := FTimeSelectionLow
    else Result := 0;
end;

function TFrameBGLSequencer.SelectedAreaEndTime: single;
begin
  if AnAreaIsSelected
    then Result := FTimeSelectionHigh
    else Result := 0;
end;

function TFrameBGLSequencer.SelectedAreaDuration: single;
begin
  if AnAreaIsSelected
    then Result := FTimeSelectionHigh - FTimeSelectionLow
    else Result := 0;
end;

procedure TFrameBGLSequencer.View_All;
var last: TCustomSequencerStep;
begin
  View_BeginTime := 0.0;

  last := GetLastStep;
  if last <> NIl then begin
    // x1.2 -> the view will be large enough to show the text of the last step.
    PixelPerSecond:= BGLVirtualScreen1.ClientWidth/((last.TimePos+last.Duration ) *1.2);
    Redraw;
  end;
end;

procedure TFrameBGLSequencer.View_ZoomOnSelectedArea;
var vbt: single;
begin
  if AnAreaIsSelected then begin
    PixelPerSecond := BGLVirtualScreen1.ClientWidth/((FTimeSelectionHigh-FTimeSelectionLow)*1.05);

    vbt := (BGLVirtualScreen1.ClientWidth/PixelPerSecond)-(FTimeSelectionHigh-FTimeSelectionLow);
    vbt := FTimeSelectionLow - vbt/2;
    View_BeginTime := vbt;
    Redraw;
  end;
end;

procedure TFrameBGLSequencer.PlayCursorVisible(AValue: boolean);
begin
  if FPlayCursorVisible = AValue then exit;
  FPlayCursorVisible := AValue;
  Redraw;
end;

procedure TFrameBGLSequencer.UpdatePlayCursorPosition(aTimePos: single);
begin
  if FPlayCursorTimePos = aTimePos then exit;
  FPlayCursorTimePos := aTimePos;
  Redraw;
end;

function TFrameBGLSequencer.AbscissaToTimePos(aValue: integer): single;
begin
  Result := aValue/PixelPerSecond;
end;

function TFrameBGLSequencer.TimePosToAbscissa(aValue: single): integer;
begin
  Result := Round((aValue-View_BeginTime)*PixelPerSecond);
end;

function TFrameBGLSequencer.TimePosIsBeforeFirstStep(aTimePos: single): boolean;
begin
  if FStepList.Count > 0 then Result := aTimePos < FStepList.Items[0].TimePos
    else Result := TRUE;
end;

function TFrameBGLSequencer.TimePosIsAfterLastStep(aTimePos: single): boolean;
begin
  if FStepList.Count > 0 then Result := aTimePos > FStepList.Items[FStepList.Count-1].TimePos
    else Result := TRUE;
end;

function TFrameBGLSequencer.GetStepBefore(aTimePos: single): TCustomSequencerStep;
var s: TCustomSequencerStep;
begin
  Result := NIL;
  if FStepList.Count > 0 then begin
    for s in FStepList do
     if s.TimePos <= aTimePos
       then Result := s
       else exit;
  end;
end;

function TFrameBGLSequencer.GetStepAfter(aTimePos: single ): TCustomSequencerStep;
var i: Integer;
begin
  Result := NIL;
  if FStepList.Count>0 then begin
    for i:=FStepList.Count-1 downto 0 do
     if FStepList.Items[i].TimePos > aTimePos
        then Result := FStepList.Items[i]
        else exit;
  end;
end;

function TFrameBGLSequencer.GetFirstStep: TCustomSequencerStep;
begin
  if FStepList.Count > 0
    then Result := FStepList.Items[0]
    else Result := NIL;
end;

function TFrameBGLSequencer.GetLastStep: TCustomSequencerStep;
begin
  if FStepList.Count > 0
   then Result := FStepList.Items[FStepList.Count-1]
   else Result := NIL;
end;

procedure TFrameBGLSequencer.ApplyTimeOffsetOnStep(aStep: TCustomSequencerStep; TimeOffset: single);
begin
  Notify([aStep], snChanged, NotifyShiftMessage);
  RawApplyTimeOffsetOnStep(aStep, TimeOffset);
end;

procedure TFrameBGLSequencer.ShiftStepTimePositionFrom( aFirstStep: TCustomSequencerStep; TimeOffset: single);
var k, i: LongInt;
begin
  k := FStepList.IndexOf(aFirstStep);

  NotifyRange( k, StepList.Count-1, snChanged, 'Décaler');

  for i:=k to FStepList.Count-1 do
   RawApplyTimeOffsetOnStep(FStepList.Items[i], TimeOffset);
  FStepList.Sort;
  Redraw;
end;

procedure TFrameBGLSequencer.DeleteTimeAt(aTimePos: single);
var before, after: TCustomSequencerStep;
  delta: single;
begin
  before := GetStepBefore(aTimePos);
  after := GetstepAfter(aTimePos);
  if (before = NIL) and (after = NIL) then exit; // sequencer is empty

  if (before <> NIL) and (after = NIL) then exit; // time position is after the last step

  if (before = NIL) and (after <> NIL) then begin
    // shift all chronologie to the left
    delta := -after.TimePos;
    ShiftStepTimePositionFrom(after, delta);
  end
  else
  if (before <> NIL) and (after <> NIL) then begin
    // between 2 steps
    delta := -(after.TimePos-before.TimePos);
    ShiftStepTimePositionFrom(after, delta);
  end;
end;

procedure TFrameBGLSequencer.SetOptions(aSet: TFrameSequencerOptions; AValue: boolean);
var o: TFrameBGLSequenceOption;
begin
  if AValue then for o in aSet do Include(FOptions, o)
    else for o in aSet do Exclude(FOptions, o);

  if (bglsKeepTimeOriginVisible in aSet) and Avalue then View_BeginTime := 0;

  Redraw;
end;

{ TCustomSequencerStep }

procedure TCustomSequencerStep.SetCaption(AValue: string);
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
  if ParentSeq <> NIL
    then ParentSeq.NeedStepsWidthUpdate;
end;

procedure TCustomSequencerStep.SetParentSeq(AValue: TFrameBGLSequencer);
begin
  FParentSeq := AValue;
  ParentSeq.NeedStepsWidthUpdate;
end;

procedure TCustomSequencerStep.SetSelected(AValue: boolean);
begin
  if FSelected = AValue then exit;
  FSelected := AValue;
end;

procedure TCustomSequencerStep.SetGroup(AValue: integer);
begin
  if FGroup = AValue then exit;
  FGroup := AValue;
end;

procedure TCustomSequencerStep.SetDuration(AValue: single);
begin
  if FDuration = AValue then exit;
  FDuration := AValue;
  if ParentSeq <> NIL
    then ParentSeq.NeedStepsWidthUpdate;
end;

procedure TCustomSequencerStep.UpdateWidth;
var captionWidth, durationWidth: Integer;
begin
  if FParentSeq = NIL then exit;
  captionWidth := Round(FParentSeq.FStepFont.TextWidth(FCaption));
  durationWidth := Round(FParentSeq.PixelPerSecond*FDuration);
  FWidth := Max(captionWidth, durationWidth);
end;

constructor TCustomSequencerStep.Create;
begin
  Top := 0;
  FWidth := 20;
end;

procedure TCustomSequencerStep.Redraw(aParentFrame: TFrameBGLSequencer; aBGLContext: TBGLContext);
var fc, bg: TBGRAPixel;
  xx, yy, stepFontHeight, timeFontHeight: Integer;
  txt: String;
begin
  xx := aParentFrame.TimePosToAbscissa(TimePos);
  if (xx >= aBGLContext.Canvas.Width) or (xx + Width <= 0) then exit;

  stepFontHeight := aParentFrame.StepFont.FullHeight;
  timeFontHeight:= aParentFrame.TimeFont.FullHeight;

  if Group = 0 then begin
    // no group
    fc := BGRA(234,234,234);
    bg := BGRA(25,25,25,100);
  end else begin
    // with group
    fc := BGRA(255,255,255);
    bg := GroupColor[Group mod high(GroupColor)+1];
  end;
  // because a step can not be on vertical position that is not visible
  // one forces it to be in Step area, just in case...
//     if step.Top+FTimeFont.FullHeight*2+stepFontHeight>StepArea.Height then
//       step.Top := ForceStepTopToBeInStepArea(step.Top);
  with aBGLContext.Canvas do begin
   yy := Top + timeFontHeight;
   // render step background
   FillRect(xx, yy, xx+Self.Width, yy+stepFontHeight, bg);
   // render selected state
   if Selected then
     Rectangle(xx, yy, xx+Self.Width, yy+stepFontHeight, BGRA(25,255,255), 1.5);
   // render time position
   aParentFrame.FTimeFont.TextOut(xx, yy-timeFontHeight, TimeToString(TimePos),BGRA(255,255,0));
   // render caption
   aParentFrame.FStepFont.TextOut( xx, yy, Caption, fc);
   // render duration lines
   if Duration > 0 then begin
     Line(xx, yy, xx, yy+stepFontHeight, BGRA(255,255,0));
     Line(xx-1, yy, xx-1, yy+stepFontHeight, BGRA(255,255,0));

     yy := yy + stepFontHeight;
     Line(xx-1, yy,
          xx-1+Duration*aParentFrame.PixelPerSecond, yy,
          BGRA(255,255,0));
     Line(xx-1, yy+1,
          xx-1+Duration*aParentFrame.PixelPerSecond, yy+1,
          BGRA(255,255,0));
     txt := TimeToString(Duration);
     aParentFrame.FTimeFont.TextOut(xx-1+Duration*aParentFrame.PixelPerSecond-aParentFrame.FTimeFont.TextWidth(txt)*0.5,
                       yy+1, txt, BGRA(255,255,0));
   end;
  end;
end;

function TCustomSequencerStep.Serialize: string;
begin
  // override to do the job at your convenience
  Result := '';
end;

procedure TCustomSequencerStep.Deserialize(const s: string);
begin
  // override to do the job at your convenience
end;

function TCustomSequencerStep.CanApplyTimeOffset(aDeltaTime: single): boolean;
begin
 Result := (TimePos+aDeltaTime) >= 0;
end;

Initialization
  {$I frame_bglvirtualscreen_sequencer.lrs}
GroupColor[0] := BGRA(158,52,52);
GroupColor[1] := BGRA(10,40,120);
GroupColor[2] := BGRA(10,110,110);
GroupColor[3] := BGRA(10,150,255);
GroupColor[4] := BGRA(149,66,138);
GroupColor[5] := BGRA(104,2,166);
GroupColor[6] := BGRA(62,86,255);
GroupColor[7] := BGRA(133,123,3);
GroupColor[8] := BGRA(43,115,48);
GroupColor[9] := BGRA(186,97,7);

end.

