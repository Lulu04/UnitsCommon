unit lightcolorgradient;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, Forms, Graphics, Spin, Dialogs,
  BGRABitmap, BGRABitmapTypes;


// UTILISATION :
//
// poser 2 PaintBox sur la fiche, un pour le colori et un autre pour le nuancier.
// poser 3 SpinEdit pour les valeurs de rouge vert et bleu et un TShape pour afficher la couleur à l'utilisateur.
// créer un TLightColorGradient dans OnCreate de la fiche,
// puis utiliser TLightColorGradient.AssignPaintBox et TLightColorGradient.AssignSpinEditAndShape
// TLightColorGradient.OnColorChange = callback lorsque la couleur est modifié par l'utilisateur
// TLightColorGradient.SelectedColor = la couleur choisi par l'utilisateur
// TLightColorGradient.ChartColor =  record avec la couleur de base et le poucentage appliqué dessus pour
//                                   arriver à la couleur choisi par l'utilisateur.
//                                   Utile pour sauvegarder une couleur du TColorChart

type

// define a base color and a percentage to apply on
// percentage is [0..1] -> 0=black   0.5=BaseColor   1.0=white

{ TColorChartColor }

TColorChartColor= record
  BaseColor: TColor;
  Percentage: single;
  function ToColor: TColor;
  function ToString: string;
  procedure InitFromString(const s: string);
  procedure InitFromStringArray(const A: TStringArray);
end;


{ TColorChart }

TColorChart = class
 Constructor Create;
 Destructor Destroy; override;
 private
  FPB: TPaintBox;  // conteneur de la palette
  FTemp: TBGRABitmap; // pour pré-tracer l'image des nuances
  FBaseColor: TColor;
  FX: integer;  // coordonnées de la croix ( de la couleur sélectionnée )
  FMouseDrag: boolean;  // = TRUE si la souris a été cliquée sur les coloris
  FOnChange: TNotifyEvent;
  procedure DoOnChange;
  function GetChartColor: TColorChartColor;
  procedure SetBaseColor( acouleur: TColor );
  function GetSelectedColor: TColor;
  procedure SetChartColor(AValue: TColorChartColor);
  procedure SetSelectedColor( aNuance: TColor );
  function GetPercent: single;
  procedure SetPercent( AValue: single );
  procedure DoPaint( {%H-}Sender: TObject );
  procedure DoMouseDown({%H-}Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, {%H-}Y: Integer);
  procedure DoMouseMove({%H-}Sender: TObject; {%H-}Shift: TShiftState; X, {%H-}Y: Integer) ;
  procedure DoMouseUp({%H-}Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer) ;
  procedure DoOnResize({%H-}Sender: TObject);
  procedure Redraw;
 public
  procedure AssignPaintBox( aPB: TPaintBox );
  // set the cursor position to the middle of the gradient, so to the BaseColor
  procedure ResetCursor;
  procedure SetPercentageTo(AValue: single); // [0..1]

  property BaseColor: TColor read FBaseColor write SetBaseColor; // couleur de base d'où seront tirées les nuances
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
  property ChartColor: TColorChartColor read GetChartColor write SetChartColor;
  // this callback is called when SelectedColor change by the user or by code
  property OnChange: TNotifyEvent read FOnChange write FOnChange; // CallBack lorsque la nuance est changé par l'utilisateur
end;


{ TPalette }
// Trace une palette de couleurs sur un PaintBox, et gère la souris dessus
// gère un nuancier associé ( s'il y en a un )
TPalette = class
 Constructor Create;
 Destructor Destroy; override;
 private
  FPB: TPaintBox;  // conteneur de la palette
  FImage: TBGRABitmap;// pour pré-tracer l'image des coloris
  FSelectedColor: TColor;  // couleur sélectionnée
  FX, FY: integer;  // coordonnées de la croix ( de la couleur sélectionnée )
  FMouseDrag: boolean;  // = TRUE si la souris a été cliquée sur les coloris
  FOnChange: TNotifyEvent;
  procedure DrawImageBuffer;
  procedure SetSelectedColor(AValue: TColor);
  procedure DoChangeColor(aColor: TColor);
  procedure ProcessPaintBoxPaint({%H-}Sender: TObject);
  procedure ProcessPaintBoxMouseDown({%H-}Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
  procedure ProcessPaintBoxMouseMove({%H-}Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer) ;
  procedure ProcessPaintBoxMouseUp({%H-}Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer) ;
  procedure ProcessPaintBoxResize({%H-}Sender: TObject);
 public
  procedure AssignPaintBox(aPB: TPaintBox);

  property SelectedColor: TColor read FSelectedColor write SetSelectedColor; // lire/fixer la couleur sélectionnée
  // this callback is called when SelectedColor change by user or by code
  property OnChange: TNotifyEvent read FOnChange write FOnChange;
end;


{ TLightColorGradient }
// manage a TPalette, a TColorChart, a TShape and 3 TSpinEdit
TLightColorGradient = class
private
  FPalette: TPalette;
  FColorChart: TColorChart;
  FSERed, FSEGreen, FSEBlue: TSpinEdit;
  FShape: TShape;
  function GetChartColor: TColorChartColor;
  function GetSelectedColor: TColor;
  procedure SetSelectedColor(AValue: TColor);
  function ShapeAvailable: boolean;
  function SpinEditRVBAvailable: boolean;
  procedure UpdateShapeColor;
  procedure ProcessSpinEditChange({%H-}Sender: TObject);
  procedure ProcessColorChartChange( {%H-}Sender: TObject );
  procedure ProcessPaletteChange( {%H-}Sender: TObject );
  procedure DoOnChange;
private
  FOnChange: TNotifyEvent;
  FLockOnChange: boolean;
  procedure LockDoChange;
  procedure SetChartColor(AValue: TColorChartColor);
  procedure UnlockDoChange;
public
  constructor Create;
  destructor Destroy; override;

  procedure AssignPaintBox( aPBColoris, aPBNuancier: TPaintBox );
  procedure AssignSpinEditAndShape( aSERed, aSEGreen, aSEBlue: TSpinEdit; aShape: TShape=NIL );

  procedure ResetCursorToMiddle;
  // percentage is [0..1]
  //   0=black   0.5=BaseColor   1.0=white
  procedure SetPercentageTo( AValue:single);

  // gives the color selected by the user:
  // 1: if the 3 SpinEdit are available : return the color defined by them
  // 2: else return the current color in the color chart
  property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
  // gives the base color and the percentage applyed on by the cursor of color chart
  property ChartColor: TColorChartColor read GetChartColor write SetChartColor;
  // this callback is called when SelectedColor change by the user or by code
  property OnColorChange: TNotifyEvent read FOnChange write FOnChange;
end;

// aPercentage[0..1]
function ChartColor( aBaseColor: TColor; aPercentage: single ): TColorChartColor;

implementation

uses Math, PropertyUtils;

function StringToSingle(const aStr: string): single;
var i, k, integerPart, decimalPart: integer;
  divisor: double;
begin
  k := Pos('.', aStr);
  if k = 0 then
    k := Pos(',', aStr);

  if k = 0 then
    Result := Single(aStr.ToInteger)
  else
  begin
    integerPart := Copy(aStr, 1, k-1).ToInteger;
    decimalPart := Copy(aStr, k+1, Length(aStr)-k).ToInteger;

    divisor := 1;
    for i:=1 to Length(aStr)-k do
      divisor := divisor*10;

    Result := Single(integerPart+decimalPart/divisor);
  end;
end;

function ChartColor(aBaseColor: TColor; aPercentage: single): TColorChartColor;
begin
  Result.BaseColor:=aBaseColor;
  Result.Percentage:=aPercentage;
end;

{ TColorChartColor }

function TColorChartColor.ToColor: TColor;
var r,g,b: integer;
  pc: single;
begin
  // percentage is [0..1] -> 0=black   0.5=BaseColor   1.0=white
  if Percentage=0.5 then begin
    Result:=BaseColor;
    exit;
  end;
  r:=Red(BaseColor);
  g:=Green(BaseColor);
  b:=Blue(BaseColor);
  if Percentage<0.5 then begin
    pc:=Percentage*2;
    r:=Round(r-r*pc);
    g:=Round(g-g*pc);
    b:=Round(b-b*pc);
  end else begin
    pc:=(Percentage-0.5)*2;
    r:=Round(r+(255-r)*pc);
    g:=Round(g+(255-g)*pc);
    b:=Round(b+(255-b)*pc);
  end;
  r:=EnsureRange(r, 0, 255);
  g:=EnsureRange(g, 0, 255);
  b:=EnsureRange(b, 0, 255);
  Result:=RGBToColor(r,g,b);
end;

function TColorChartColor.ToString: string;
begin
  Result:=integer(BaseColor).ToString+' '+FormatFloat('0.00', Percentage);
  Result := Red(BaseColor).ToString+' '+
            Green(BaseColor).ToString+' '+
            Blue(BaseColor).ToString+' '+
            FormatFloatWithDot('0.00', Percentage);
end;

procedure TColorChartColor.InitFromString(const s: string);
var A: TStringArray;
begin
  A := s.Split([' ']);
  InitFromStringArray(A);
end;

procedure TColorChartColor.InitFromStringArray(const A: TStringArray);
begin
  BaseColor := RGBToColor(A[0].ToInteger, A[1].ToInteger, A[2].ToInteger);
  Percentage := StringToSingle(A[3]);
end;

{ TLightColorGradient }

function TLightColorGradient.SpinEditRVBAvailable: boolean;
begin
 Result := Assigned(FSERed) and Assigned(FSEGreen) and Assigned(FSEBlue);
end;

function TLightColorGradient.GetSelectedColor: TColor;
begin
 if SpinEditRVBAvailable then
   Result := RGBToColor(FSERed.Value, FSEGreen.Value, FSEBlue.Value)
 else
   Result := FColorChart.SelectedColor;
end;

function TLightColorGradient.GetChartColor: TColorChartColor;
begin
 Result := FColorChart.ChartColor;
end;

procedure TLightColorGradient.SetSelectedColor(AValue: TColor);
var c: TColorChartColor;
begin
 //if AValue=0 then;
 //ResetCursorToMiddle;
 c.BaseColor:=AValue;
 c.Percentage:=0.5;
 ChartColor := c;
end;

function TLightColorGradient.ShapeAvailable: boolean;
begin
 Result := Assigned(FShape);
end;

procedure TLightColorGradient.UpdateShapeColor;
var c: TColor;
begin
 if FShape<>NIL then begin
   if SpinEditRVBAvailable
     then c := RGBToColor(FSERed.Value, FSEGreen.Value, FSEBlue.Value)
     else c := FColorChart.SelectedColor;
   FShape.Brush.Color := c;
   FShape.Pen.Color:=RGBToColor(0,0,0);
 end;
end;

procedure TLightColorGradient.ProcessSpinEditChange(Sender: TObject);
begin
 UpdateShapeColor;
 DoOnChange;
end;

procedure TLightColorGradient.ProcessColorChartChange(Sender: TObject);
var c: TColor;
begin
 c := FColorChart.SelectedColor;
 LockDoChange;
 UpdateShapeColor;
 if SpinEditRVBAvailable then begin
   FSERed.Value := Red(c);
   FSEGreen.Value := Green(c);
   FSEBlue.Value := Blue(c);
 end;
 UnlockDoChange;
 DoOnChange;
end;

procedure TLightColorGradient.ProcessPaletteChange(Sender: TObject);
begin
 FColorChart.BaseColor := FPalette.SelectedColor;
end;

procedure TLightColorGradient.DoOnChange;
begin
 if FLockOnChange then exit;
 if FOnChange<>NIL then FOnChange(Self);
end;

procedure TLightColorGradient.LockDoChange;
begin
 FLockOnChange := TRUE;
end;

procedure TLightColorGradient.SetChartColor(AValue: TColorChartColor);
begin
 FColorChart.ChartColor := AValue;
end;

procedure TLightColorGradient.UnlockDoChange;
begin
 FLockOnChange := FALSE;
end;

constructor TLightColorGradient.Create;
begin
 FPalette := TPalette.Create;
 FPalette.OnChange:=@ProcessPaletteChange;
 FColorChart := TColorChart.Create;
 FColorChart.OnChange:=@ProcessColorChartChange;
end;

destructor TLightColorGradient.Destroy;
begin
 FPalette.Free;
 FColorChart.Free;
 inherited Destroy;
end;

procedure TLightColorGradient.AssignPaintBox(aPBColoris, aPBNuancier: TPaintBox);
begin
 FPalette.AssignPaintBox( aPBColoris );
 FColorChart.AssignPaintBox( aPBNuancier );
end;

procedure TLightColorGradient.AssignSpinEditAndShape(aSERed, aSEGreen,
  aSEBlue: TSpinEdit; aShape: TShape);
begin
 if FSERed<>NIL then FSERed.OnChange:=NIL;
 if FSEGreen<>NIL then FSEGreen.OnChange:=NIL;
 if FSEBlue<>NIL then FSEBlue.OnChange:=NIL;

 FSERed:= aSERed;
 FSEGreen:= aSEGreen;
 FSEBlue:= aSEBlue;

 if SpinEditRVBAvailable then begin
   LockDoChange;
   FSERed.OnChange:=@ProcessSpinEditChange;
   FSEGreen.OnChange:=@ProcessSpinEditChange;
   FSEBlue.OnChange:=@ProcessSpinEditChange;

   FSERed.Value:=Red(FColorChart.SelectedColor);
   FSEGreen.Value:=Green(FColorChart.SelectedColor);
   FSEBlue.Value:=Blue(FColorChart.SelectedColor);
   UnlockDoChange;
 end;

 FShape := aShape;
 UpdateShapeColor;
end;

procedure TLightColorGradient.ResetCursorToMiddle;
begin
 FColorChart.ResetCursor;
 ProcessColorChartChange(NIL);
end;

procedure TLightColorGradient.SetPercentageTo(AValue: single);
begin
 FColorChart.SetPercentageTo(AValue);
end;

{ TColorChart }

constructor TColorChart.Create;
begin
 inherited create;
 FBaseColor := clRed;
 FTemp:=TBGRABitmap.Create(1,1);
end;

destructor TColorChart.Destroy;
begin
 FTemp.Free;
 inherited Destroy;
end;

procedure TColorChart.DoOnChange;
begin
 if FOnChange<>NIL then FOnChange(FPB);
end;

function TColorChart.GetChartColor: TColorChartColor;
begin
 Result.BaseColor := BaseColor;
 Result.Percentage := GetPercent;
end;

procedure TColorChart.SetBaseColor(acouleur: TColor);
begin
 FBaseColor := acouleur;
 Redraw;
 if assigned( FPB ) then FPB.Invalidate;
 DoOnChange;
end;

function TColorChart.GetSelectedColor: TColor;
begin
 Result:=BGRAToColor(FTemp.GetPixel(FX,0));
end;

procedure TColorChart.SetChartColor(AValue: TColorChartColor);
begin
 BaseColor := AValue.BaseColor;
 SetPercent( AValue.Percentage );
end;

procedure TColorChart.SetSelectedColor(aNuance: TColor);
var i: integer;
  c: TBGRAPixel;
begin
 c:= ColorToBGRA(aNuance);
 for i:=0 to FTemp.Width-1 do
  if FTemp.GetPixel(i,0) = c then begin
    FX := i;
    break;
  end;
 FPB.Invalidate;
end;

function TColorChart.GetPercent: single;
begin
 Result := FX/(FPB.Width-1);
end;

procedure TColorChart.SetPercent(AValue: single);
begin
 if AValue<0 then AValue:=0;
 if AValue>1 then AValue:=1;
 FX := round( (FPB.Width-1)*AValue );
 DoOnChange;
end;

procedure TColorChart.Redraw;
var xx, milieu: integer;
  r, v, b, pr, pv, pb: single;
begin

 r:=Red(FBaseColor);
 v:=Green(FBaseColor);
 b:=Blue(FBaseColor);

 milieu:=FTemp.Width shr 1;
 pr:=r/milieu;
 pv:=v/milieu;
 pb:=b/milieu;

  for xx:=milieu downto 0 do
   begin
    FTemp.VertLine(xx, 0, FTemp.Height, BGRA(Trunc(r), Trunc(v), Trunc(b)));
    r := r - pr ; v := v - pv ; b := b - pb;
    if r < 0 then r := 0;
    if v < 0 then v := 0;
    if b < 0 then b := 0;
   end;

  r:=Red(FBaseColor);
  v:=Green(FBaseColor);
  b:=Blue(FBaseColor);
  pr:=(255-r)/milieu;
  pv:=(255-v)/milieu;
  pb:=(255-b)/milieu;
  for xx:=milieu to FTemp.Width do
   begin
    FTemp.VertLine(xx, 0, FTemp.Height, BGRA(Trunc(r), Trunc(v), Trunc(b)));
    r := r + pr;
    v := v + pv;
    b := b + pb;
    if r > 255 then r := 255;
    if v > 255 then v := 255;
    if b > 255 then b := 255;
   end;

{ r := FBaseColor and $FF;
 v := ( FBaseColor shr 8 ) and $FF;
 b := ( FBaseColor shr 16 ) and $FF;

 milieu := (FNuances.Width-1) shr 1;
 pr := r / milieu;
 pv := v / milieu;
 pb := b / milieu;

 with FNuances.Canvas do
 begin
  for xx:=milieu downto 0 do
   begin
    Pen.Color := ( (trunc(b) shl 16 ) or (trunc(v) shl 8 ) or trunc(r) );
    Line ( xx, 0, xx, FNuances.Height );
    r := r - pr ; v := v - pv ; b := b - pb;
    if r < 0 then r := 0;
    if v < 0 then v := 0;
    if b < 0 then b := 0;
   end;
  r := FBaseColor and $FF;
  v := ( FBaseColor shr 8 ) and $FF;
  b := ( FBaseColor shr 16 ) and $FF;
  pr := (255-r) / milieu;
  pv := (255-v) / milieu;
  pb := (255-b) / milieu;
  for xx:=milieu to FNuances.Width-1 do
   begin
    Pen.Color := ( (trunc(b) shl 16 ) or (trunc(v) shl 8 ) or trunc(r) );
    Line( xx, 0, xx, FNuances.Height );
    r := r + pr;
    v := v + pv;
    b := b + pb;
    if r > 255 then r := 255;
    if v > 255 then v := 255;
    if b > 255 then b := 255;
   end;
 end;  }
end;

procedure TColorChart.DoPaint(Sender: TObject);
var i: integer;
  c: TBGRAPixel;
begin
 with FPB.Canvas do begin
  // render color gradient
  FTemp.Draw(FPB.Canvas, 0, 0);
  // render cursor
  c:=FTemp.GetPixel(FX,0);
  if c.red+c.green+c.blue>600
    then Pen.Color := clBlack
    else Pen.Color := clWhite;
   for i:=FX-1 to FX+1 do begin    // vertical
     Line( i, 0, i, FPB.Height shr 2 );
     Line( i, FPB.Height, i, FPB.Height - FPB.Height shr 2 );
   end;
 end;
end;

procedure TColorChart.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then begin
   FMouseDrag := TRUE;
   FX := X;
   if FX < 0 then FX := 0;
   if FX > FPB.Width-1 then FX := FPB.Width-1;
   if assigned( FOnChange ) then FOnChange( FPB );  // callback
   FPB.Invalidate;
 end;
end;

procedure TColorChart.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if FMouseDrag then begin
   FX := X;
   if FX < 0 then
     FX := 0;
   if FX > FPB.Width-1 then
     FX := FPB.Width-1;
   if assigned( FOnChange ) then
     FOnChange( FPB );  // callback

   FPB.Invalidate;
 end;
end;

procedure TColorChart.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then begin
   FMouseDrag := FALSE;
   FPB.Invalidate;
 end;
end;

procedure TColorChart.DoOnResize(Sender: TObject);
begin
  FTemp.SetSize(FPB.ClientWidth, FPB.ClientHeight);
  Redraw;
  ResetCursor;
end;

procedure TColorChart.AssignPaintBox(aPB: TPaintBox);
begin
 FPB := aPB;
 FPB.OnPaint := @DoPaint;
 FPB.OnMouseDown := @DoMouseDown;
 FPB.OnMouseMove := @DoMouseMove;
 FPB.OnMouseUp := @DoMouseUp;
 FPB.OnResize:=@DoOnResize;;
 FTemp.SetSize(FPB.ClientWidth, FPB.ClientHeight);
 FX:=FPB.ClientWidth shr 1;
 Redraw;
end;

procedure TColorChart.ResetCursor;
begin
 FX:=FTemp.Width shr 1;
 FPB.Invalidate;
end;

procedure TColorChart.SetPercentageTo(AValue: single);
begin
  SetPercent(AValue);
 FPB.Invalidate;
end;


{ TPalette }

constructor TPalette.Create;
begin
 FImage := TBGRABitmap.Create(1, 1);
 FSelectedColor := clRed;
end;

destructor TPalette.Destroy;
begin
 FImage.Free;
 inherited Destroy;
end;

procedure TPalette.DrawImageBuffer;
var
  r,v,b : integer ;
  pas : double ;
  xp : double ;
     procedure TraceLigneVerticale(xx: integer; c: TColor);
      var i, br, bv, bb, but: integer;
         ar, av, ab: double;
     begin
      but := 127;
      br := c and $FF; bv := (c shr 8) and $FF; bb := (c shr 16) and $FF;
      ar := ( but - br ) / FImage.Height ; av := ( but - bv ) / FImage.Height ; ab := ( but - bb ) / FImage.Height;
      for i:=0 to FImage.Height-1 do
       FImage.SetPixel(xx, i,
          (trunc(ab * i + bb) shl 16) or
          ( trunc(av * i + bv) shl 8) or
          trunc(ar * i + br));
     end;
begin
  // resize image
  FImage.SetSize(FPB.ClientWidth, FPB.ClientHeight);

  // render gradient
  pas := FImage.Width / (6*256);
  xp := 0;
  r := 255; v :=0; b := 0;
  for v:=0 to 255 do  // I
   begin
    TraceLigneVerticale(trunc(xp), ( b shl 16 ) or ( v shl 8 ) or r );
    xp := xp + pas;
   end;
  for r:=255 downto 0 do  // II
   begin
    TraceLigneVerticale(trunc(xp), ( b shl 16 ) or ( v shl 8 ) or r );
    xp := xp + pas;
   end;
  for b:=0 to 255 do  // III
   begin
    TraceLigneVerticale(trunc(xp), ( b shl 16 ) or ( v shl 8 ) or r);
    xp := xp + pas ;
   end;
  for v:=255 downto 0 do  // IV
   begin
    TraceLigneVerticale(trunc( xp ), ( b shl 16 ) or ( v shl 8 ) or r);
    xp := xp + pas;
   end;
  for r:=0 to 255 do   // V
   begin
    TraceLigneVerticale(trunc( xp ), ( b shl 16 ) or ( v shl 8 ) or r);
    xp := xp + pas;
   end;
  for b:=255 downto 0 do  // VI
   begin
    TraceLigneVerticale(trunc( xp ), ( b shl 16 ) or ( v shl 8 ) or r);
    xp := xp + pas;
   end;
end;

procedure TPalette.SetSelectedColor(AValue: TColor);
var
   xp, yp, r, v, b: integer;
   pasx, pasy: double;
begin
 // il faut positionner le curseur sur la palette de coloris à l'endroit correspondant à la couleur
 r := AValue and $FF;              // on récupère les composantes r v b
 v := ( AValue shr 8 ) and $FF;
 b := ( AValue shr 16 ) and $FF;
 // on ramène au moins l'une des composantes à 0
 while ( r <> 0 ) and ( v <> 0 ) and ( b <> 0 ) do
 begin
  if r > 0 then dec ( r );
  if v > 0 then dec ( v );
  if b > 0 then dec ( b );
 end;
 // on ramène maintenant au moins l'une des composantes à 255, sauf la ou les composantes nulles
 yp := 0;
 if ( r <> 0 ) or ( v <> 0 ) or ( b <> 0 )
   then while ( r <> 255 ) and ( v <> 255 ) and ( b <> 255 ) do
         begin
          if r <> 0 then inc ( r );
          if v <> 0 then inc ( v );
          if b <> 0 then inc ( b );
          inc ( yp );
         end;
 // maintenant, on teste les composantes nulles
 pasx := FImage.Width / (6*256);
 pasy := FImage.Height / ( 256 - 128 );
 if ( ( r = 0 ) and ( v = 0 ) and ( b = 0 ) ) or
    ( ( r = 255 ) and ( v = 255 ) and ( b = 255 ) )
 then begin
       xp := 0;
       yp := FImage.Height - 1;
      end
 else if ( r = 255 ) {and ( v <> 0 )} and ( b = 0 )
        then xp := trunc ( ( (0*256) + v ) * pasx )  // I
 else if {( r <> 0 ) and} ( v = 255 ) and ( b = 0 )
        then xp := trunc ( ( (1*256) + ( 255 - r ) ) * pasx )  // II
 else if ( r = 0 ) and ( v = 255 ) {and ( b <> 0 ) }
        then xp := trunc ( ( (2*256) + b ) * pasx )  // III
 else if ( r = 0 ) {and ( v <> 0 ) }and ( b = 255 )
        then xp := trunc ( ( (3*256) + ( 255 - v ) ) * pasx )  // IV
 else if {( r <> 0 ) and }( v = 0 ) and ( b = 255 )
        then xp := trunc ( ( (4*256) + r ) * pasx )  // V
 else if ( r = 255 ) and ( v = 0 ) {and ( b = 255 )}
        then xp := trunc ( ( (5*256) + ( 255 - b ) ) * pasx ) ; // VI
 yp := trunc ( FImage.Height - ( (255 - yp ) * pasy ) );
 if yp < 0 then yp := 0;
 FX := xp;
 FY := yp;
 FSelectedColor := FPB.Canvas.Pixels[FX,FY];
 FPB.Invalidate;
end;

procedure TPalette.ProcessPaintBoxPaint(Sender: TObject);
var
  i: integer;
begin
 with FPB.Canvas do
 begin
  // tracé des coloris
  FImage.Draw(FPB.Canvas, 0, 0);
  // tracé du curseur
  with FPB.Canvas do
  begin
   Pen.Color := clBlack;
   for i:=FY-1 to FY+1 do     // horizontal
   begin
     Line(FX-7, i, FX-3, i);
     Line(FX+7, i, FX+3, i);
   end;
   for i:=FX-1 to FX+1 do     // vertical
   begin
     Line(i, FY-7, i, FY-3);
     Line(i, FY+7, i, FY+3);
   end;
  end;
 end;
end;

procedure TPalette.ProcessPaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then
 begin
   FMouseDrag := TRUE;
   FX := EnsureRange(X, 0, FPB.Width-1);
   FY := EnsureRange(Y, 0, FPB.Height-1);
   DoChangeColor( FImage.Canvas.Pixels[FX,FY] );
 end;
end;

procedure TPalette.AssignPaintBox(aPB: TPaintBox);
begin
 FPB := aPB;
 FPB.OnPaint := @ProcessPaintBoxPaint;
 FPB.OnMouseDown := @ProcessPaintBoxMouseDown;
 FPB.OnMouseMove := @ProcessPaintBoxMouseMove;
 FPB.OnMouseUp := @ProcessPaintBoxMouseUp;
 FPB.OnResize := @ProcessPaintBoxResize;

 DrawImageBuffer;
end;

procedure TPalette.DoChangeColor( aColor: TColor );
begin
 FSelectedColor := aColor;
 if FOnChange <> NIL then
   FOnChange(self);
end;

procedure TPalette.ProcessPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  c: TColor;
begin
 if FMouseDrag then
 begin
   FX := EnsureRange(X, 0, FPB.ClientWidth-1);
   FY := EnsureRange(Y, 0, FPB.ClientHeight-1);

   c := FImage.GetPixel(FX, FY);
   DoChangeColor( c );
   FPB.Invalidate;
 end;
end;

procedure TPalette.ProcessPaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then
 begin
   FMouseDrag := FALSE;
   FPB.Invalidate;
 end;
end;

procedure TPalette.ProcessPaintBoxResize(Sender: TObject);
begin
  DrawImageBuffer;
  FPB.Invalidate;
end;




end.

