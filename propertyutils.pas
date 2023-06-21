unit PropertyUtils;

{$mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}

interface

uses
  Classes, SysUtils;

type
  // Here, properties and their values are concatened in a string and separated
  // with a character separator

  { TProperties }

  TProperties = record
   private
    FProperties: TStringArray;
    FSeparator: Char;
    function ValueIndexOf(const aName: string; out index: integer): boolean;
    procedure AddProp(const n, v: string);
    function GetPackedProperty: string;
   public
    procedure SetEmpty;
    // Split and return the number of 'blocks' found.
    function Split(const s: string; aSeparator: Char): integer;
    // Return True if the property is defined
    function PropertyIsPresent(const aPropertyName: string): boolean;
    // Return true if all names property in the passed array are presents.
    function CheckProperties(aPropertyNames: TStringArray): boolean;
    // Return True if aPropertyName is found and set its value to Value
    function StringValueOf(const aPropertyName: string; var Value: string; const aDefault: string): boolean;
    function BooleanValueOf(const aPropertyName: string; var Value: boolean; aDefault: boolean): boolean;
    function IntegerValueOf(const aPropertyName: string; var Value: integer; aDefault: integer): boolean;
    function Int64ValueOf(const aPropertyName: string; var Value: int64; aDefault: int64): boolean;
    function SingleValueOf(const aPropertyName: string; var Value: single; aDefault: single): boolean;
    //function DoubleValueOf(const aPropertyName: string; var Value: double; aDefault: single): boolean;
    function ByteValueOf(const aPropertyName: string; var Value: byte; aDefault: byte): boolean;
    function WordValueOf(const aPropertyName: string; var Value: word; aDefault: word): boolean;

   public //
    procedure Init(aSeparator: char);
    procedure Add(const aPropertyName, aValue: string);
    procedure Add(const aPropertyName: string; aValue: boolean);
    procedure Add(const aPropertyName: string; aValue: integer);
    procedure Add(const aPropertyName: string; aValue: int64);
    procedure Add(const aPropertyName: string; aValue: single);
    property PackedProperty: string read GetPackedProperty;

   public
    function ReplaceValue(const aPropertyName, aValue: string): boolean;
    function ReplaceValue(const aPropertyName: string; aValue: boolean): boolean;
    function ReplaceValue(const aPropertyName: string; aValue: integer): boolean;
    function ReplaceValue(const aPropertyName: string; aValue: int64): boolean;
    function ReplaceValue(const aPropertyName: string; aValue: single): boolean;
  end;


  function FormatFloatWithDot(const aFmt: string; aValue: single): string;
  // convert the string to single. The string can have '.' or ','
  function StringToSingle(const aStr: string): single;

implementation

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

function FormatFloatWithDot(const aFmt: string; aValue: single): string;
var i: integer;
begin
  Result := FormatFloat(aFmt, aValue);
  i := Pos(',', Result);
  if i > 0 then
    Result[i] := '.';
end;

{ TProperties }

function TProperties.ValueIndexOf(const aName: string; out index: integer): boolean;
var i: integer;
begin
  i := 0;
  while i < High(FProperties) do
  begin
     if FProperties[i] = aName then
     begin
       index := i+1;
       Result := True;
       exit;
     end;
     inc(i, 2);
  end;
  Result := False;
end;

procedure TProperties.AddProp(const n, v: string);
var i: integer;
begin
  i := Length(FProperties);
  SetLength(FProperties, i+2);
  FProperties[i] := n;
  FProperties[i+1] := v;
end;

function TProperties.GetPackedProperty: string;
var i: integer;
begin
  Result := '';
  i := 0;
  while i+1 < Length(FProperties) do begin
     if Length(Result) <> 0 then
       Result := Result + FSeparator;
     Result := Result + FProperties[i] + FSeparator + FProperties[i+1];
    inc(i, 2);
  end;
end;

procedure TProperties.SetEmpty;
begin
  FProperties := NIL;
end;

function TProperties.Split(const s: string; aSeparator: Char): integer;
begin
  FSeparator := aSeparator;
  FProperties := s.Split([aSeparator]);
  Result := Length(FProperties);
end;

function TProperties.PropertyIsPresent(const aPropertyName: string): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
end;

function TProperties.CheckProperties(aPropertyNames: TStringArray): boolean;
var i, j: integer;
begin
  for i:=0 to High(aPropertyNames) do
    if not ValueIndexOf(aPropertyNames[i], j) then
    begin
      Result := False;
      exit;
    end;
  Result := True;
end;

function TProperties.StringValueOf(const aPropertyName: string; var Value: string; const aDefault: string): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    Value := FProperties[i]
  else
    Value := aDefault;
end;

function TProperties.BooleanValueOf(const aPropertyName: string; var Value: boolean; aDefault: boolean): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    Value := FProperties[i] = 'true'
  else
  Value := aDefault;
end;

function TProperties.IntegerValueOf(const aPropertyName: string;
  var Value: integer; aDefault: integer): boolean;
var i, v: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    if TryStrToInt(FProperties[i], v) then
      Value := v
    else
      Value := aDefault;
end;

function TProperties.Int64ValueOf(const aPropertyName: string;
  var Value: int64; aDefault: int64): boolean;
var i: integer;
  v: int64;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    if TryStrToInt64(FProperties[i], v) then
      Value := v
    else
      Value := aDefault;
end;

function TProperties.SingleValueOf(const aPropertyName: string;
  var Value: single; aDefault: single): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    Value := StringToSingle(FProperties[i])
  else
    Value := aDefault;
end;

function TProperties.ByteValueOf(const aPropertyName: string;
  var Value: byte; aDefault: byte): boolean;
var i, v: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    if TryStrToInt(FProperties[i], v) then
      Value := byte(v)
    else
      Value := aDefault;
end;

function TProperties.WordValueOf(const aPropertyName: string;
  var Value: word; aDefault: word): boolean;
var i, v: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    if TryStrToInt(FProperties[i], v) then
      Value := word(v)
    else
      Value := aDefault;
end;

procedure TProperties.Init(aSeparator: char);
begin
  FProperties := NIL;
  FSeparator := aSeparator;
end;

procedure TProperties.Add(const aPropertyName, aValue: string);
begin
  AddProp(aPropertyName, aValue);
end;

procedure TProperties.Add(const aPropertyName: string; aValue: boolean);
begin
  AddProp(aPropertyName, BoolToStr(aValue, 'true','false'));
end;

procedure TProperties.Add(const aPropertyName: string; aValue: integer);
begin
  AddProp(aPropertyName, aValue.ToString);
end;

procedure TProperties.Add(const aPropertyName: string; aValue: int64);
begin
  AddProp(aPropertyName, aValue.ToString);
end;

procedure TProperties.Add(const aPropertyName: string; aValue: single);
begin
  AddProp(aPropertyName, FormatFloatWithDot('0.000', aValue));
end;

function TProperties.ReplaceValue(const aPropertyName, aValue: string): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    FProperties[i] := aValue;
end;

function TProperties.ReplaceValue(const aPropertyName: string; aValue: boolean): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    FProperties[i] := BoolToStr(aValue, 'true','false');
end;

function TProperties.ReplaceValue(const aPropertyName: string; aValue: integer): boolean;
begin
  Result := ReplaceValue(aPropertyName, int64(aValue));
end;

function TProperties.ReplaceValue(const aPropertyName: string; aValue: int64): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    FProperties[i] := aValue.ToString;
end;

function TProperties.ReplaceValue(const aPropertyName: string; aValue: single): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    FProperties[i] := FormatFloatWithDot('0.000', aValue)
end;


end.

