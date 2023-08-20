unit i18_utils;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, StdCtrls;

{ How to use:
    1) somewhere in your code, declare a constant array of string with your
       application supported languages, composed by the full language name and
       its identifier (iso 639):

             const  MyAppAvailableLanguages: array[0..xxx] of string=(
                            'English', 'en',
                            'Français', 'fr', ...);
    2) call AppLang.RegisterLanguagesFrom(MyAppAvailableLanguages);
}

type
  TLanguageIdentifier = string; // 'en, fr, ru...'

  { TApplicationAvailableLanguages }

  TApplicationAvailableLanguages = record
  private
    FLangs: TStringArray;
    FUsedLangID: TLanguageIdentifier;
  public
    procedure RegisterLanguagesSupportedByApp(const aLangs: TStringArray);

    // return the native OS language
    function GetOSLanguage: TLanguageIdentifier;
    // return True if the native OS language is supported by the application
    function OSLanguageIsSupportedByApp: boolean;

    procedure FillComboBoxWithSupportedLanguage(aCB: TComboBox);

    // return the selected language identifier in the combobox
    function ComboBoxGetSelectedLanguage(aCB: TComboBox): TLanguageIdentifier;

    // Select the language in the combobox
    procedure ComboBoxSetSelectedLanguage(aCB: TComboBox;
                                          const aLangID: TLanguageIdentifier);

    procedure UseLanguage(const aLangID: TLanguageIdentifier;
                          const aFolderToTranslationsFile: string);

    function GetBidiMode(const aLangID: TLanguageIdentifier): TBiDiMode;

    property UsedLangID: TLanguageIdentifier read FUsedLangID;
  end;

var
  // use this instance in your application to manage your supported languages
  AppLang: TApplicationAvailableLanguages;

implementation
uses gettext, LCLTranslator
{$IFDEF unix}
  ,Unix
   {$IFDEF LCLCarbon}
   ,MacOSAll
   {$ENDIF}
{$ENDIF} ;

{ TApplicationAvailableLanguages }

procedure TApplicationAvailableLanguages.RegisterLanguagesSupportedByApp(
  const aLangs: TStringArray);
{var
  i, c: Integer;    }
begin
  if Length(aLangs) mod 2 <> 0 then
   Raise Exception.Create('Bad array: its length must be multiple of 2 !');

  FLangs := aLangs;
end;

function TApplicationAvailableLanguages.GetOSLanguage: TLanguageIdentifier;
var
  l, fbl: string;
  {$IFDEF LCLCarbon}
  theLocaleRef: CFLocaleRef;
  locale: CFStringRef;
  buffer: StringPtr;
  bufferSize: CFIndex;
  encoding: CFStringEncoding;
  success: boolean;
  {$ENDIF}
begin
  {$IFDEF LCLCarbon}
  theLocaleRef := CFLocaleCopyCurrent;
  locale := CFLocaleGetIdentifier(theLocaleRef);
  encoding := 0;
  bufferSize := 256;
  buffer := new(StringPtr);
  success := CFStringGetPascalString(locale, buffer, bufferSize, encoding);
  if success then
    l := string(buffer^)
  else
    l := '';
  fbl := Copy(l, 1, 2);
  dispose(buffer);
  {$ELSE}
  l := '';
  fbl := '';
  GetLanguageIDs(l, fbl);
  {$ENDIF}
  Result := fbl;
end;

function TApplicationAvailableLanguages.OSLanguageIsSupportedByApp: boolean;
var osLang: TLanguageIdentifier;
  i: Integer;
begin
  Result := False;
  osLang := GetOSLanguage;
  if osLang='' then exit;

  i := 0;
  while i < High(FLangs) do begin
    Result := FLangs[i+1] = osLang;
    if Result then exit;
    inc(i, 2);
  end;
end;

procedure TApplicationAvailableLanguages.FillComboBoxWithSupportedLanguage(aCB: TComboBox);
var i: integer;
begin
  aCB.Clear;

  i := 0;
  while i < High(FLangs) do begin
    aCB.Items.Add(FLangs[i]+' ('+FLangs[i+1]+')');
    inc(i, 2);
  end;
end;

function TApplicationAvailableLanguages.ComboBoxGetSelectedLanguage(aCB: TComboBox): TLanguageIdentifier;
var iBegin, iEnd: integer;
  aItem: string;
begin
  Result := '';
  if aCB.ItemIndex = -1 then exit;

  aItem := aCB.Items.Strings[aCB.ItemIndex];

  iBegin := Pos('(', aItem);
  if iBegin = 0 then exit;
  inc(iBegin);

  iEnd := Pos(')', aItem);
  if iEnd = 0 then exit;
  dec(iEnd);
  if iEnd < iBegin then exit;

  Result := Copy(aItem, iBegin, iEnd-iBegin+1);
end;

procedure TApplicationAvailableLanguages.ComboBoxSetSelectedLanguage(
  aCB: TComboBox; const aLangID: TLanguageIdentifier);
var i: integer;
  toSearch: string;
begin
  if Length(aLangID) > 0 then begin
    toSearch := '('+aLangID+')';
    for i:=0 to aCB.Items.Count-1 do
      if Pos(toSearch, aCB.Items.Strings[i]) > 0 then begin
        aCB.ItemIndex := i;
        exit;
      end;
  end;

  aCB.ItemIndex := -1;
end;

procedure TApplicationAvailableLanguages.UseLanguage(
  const aLangID: TLanguageIdentifier; const aFolderToTranslationsFile: string);
begin
  FUsedLangID := aLangID;
  SetDefaultLang(aLangID, aFolderToTranslationsFile);
end;

function TApplicationAvailableLanguages.GetBidiMode(const aLangID: TLanguageIdentifier): TBiDiMode;
begin
  case aLangID of
    'ar': Result := bdRightToLeft;
    else
      Result := bdLeftToRight;
  end;
end;

end.

