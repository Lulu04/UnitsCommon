unit utilitaire_bgrabitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls,
  strutils, Math,
  BGRABitmap, BGRABitmapTypes, BGRATextFX, BGRAGradients;

// return a TBGRABitmap from an SVG file.
// aWidth and aHeight = -1 => return an image with original size as described in svg.
// aWidth and aHeight <> -1 => return a maximized image that fit in the given rectangle (keep aspect ratio).
// Only aImageWidth = '-1' => returned image width is computed from aHeight to keep aspect ratio.
// Only aImageHeight = '-1' => returned image height is computed from aWidth to keep aspect ratio.
// the drawing is centered in the returned bitmap
function SVGFileToBGRABitmap(const aSVGFileName: string; aWidth: integer=-1; aHeight: integer=-1):TBGRABitmap;

// load an svg file image to the specified TImage instance.
procedure SVGFileToTImage(aImage: TImage; const aSVGFileName: string);

// return string '$bbggrraa'
function BGRAPixelToHex( aColor: TBGRAPixel ): string;
function HexToBGRAPixel( const s: string ): TBGRAPixel;


// Renvoi la couleur + pc%
function variecoul ( c : Tcolor ; pc : integer ) : Tcolor ;
// renvoi la somme rouge + vert + bleu
function SommeRVB ( c : TColor ) : integer ;


type

// alignement pour le tracé d'un texte dans un conteneur
TAlignement = ( alGauche , alCentre , alDroite , alJustifier ) ;

TXYMot = record
          x          : integer ;
          y          : integer ;
          Largeur    : integer ;
          IndexLigne : integer ;
         end;

// classe pour calculer une hauteur de font maximale pour l'affichage d'un texte dans un rectangle défini

TFormateTexte = class
  Constructor Create ;
  Destructor Destroy ; override ;
  private
    FImaTampon        : TBGRABitmap ;
    FMotsDuTexte      : TStringArray; // contient les mots distincts
    TabloXYMot        : array of TXYMot ;
    FHauteurTrace     : integer     ;
    FLargeurTrace     : integer     ;
    FDecalageOmbre    : integer     ;
    //entrées
    FTexte            : string      ;
    FXCadre           : integer     ;
    FYCadre           : integer     ;
    FLargeurCadre     : integer     ;
    FHauteurCadre     : integer     ;
    FFontName         : string      ;
    FFontStyle        : TFontStyles ;
    FAlignement       : TAlignement ;
    // sorties
    FHauteurFont      : integer     ;
    FLignesAffichable : TStringList ;
    function GetHauteurLettre(Lettre: string): integer;
    function GetLargeurLettre(Lettre: string): integer;
    function GetLigne(Index: integer): string;
    function getMot(Index: integer): string;
    function GetNBLignes: integer ;
    function GetNBMots: integer;
    function getXYMot(Index: integer): TPoint;
    procedure SetHauteurFont(AValue: integer);
    procedure SimuleLeTrace ( aHauteurFont : integer ) ;
    procedure AfficheLesLignesSur(aImage: TBGRABitmap; aCouleurLettre : TBGRAPixel; aOmbre : boolean ; aCouleurOmbre : TBGRAPixel ) ;
    procedure RechercheLesIndexDesMotsDeLaLigne ( aNumLigne : integer ; var iDebut , iFin : integer ) ;
    procedure CentreLesMotsVerticalement ;
  public
    // calcul d'initialisation
    procedure Formate ( aTexte : string ;
                        aXCadre , aYCadre ,
                        aLargeurCadre , aHauteurCadre : integer ;
                        aFontName : string ;
                        aFontStyle : TFontStyles ;
                        aAlignement : TAlignement ) ;
    procedure Formate ( aTexte : string ;
                        aRect : TRect;
                        aFontName : string ;
                        aFontStyle : TFontStyles ;
                        aAlignement : TAlignement ) ;
    // Placement des mots dans le cadre
    procedure AligneLesMotsAGauche ;
    procedure AligneLesMotsAuCentre ;
    procedure AligneLesMotsADroite ;
    procedure JustifierLesMots ;
    // affichage sur le cadre
    procedure AfficheLesLignesSur(aImage: TBGRABitmap; aCouleurLettre : TBGRAPixel ) ;
    procedure AfficheLesLignesSur(aImage: TBGRABitmap; aCouleurLettre , aCouleurOmbre : TBGRAPixel ) ;
    procedure AfficheLesLignesSur(aImage: TBGRABitmap; aCouleurLettre , aCouleurContour, aCouleurOmbre : TBGRAPixel ) ;

    property HauteurFont : integer read FHauteurFont write SetHauteurFont ;
    property HauteurLettre[Lettre:string] : integer read GetHauteurLettre ;
    property LargeurLettre[Lettre:string] : integer read GetLargeurLettre ;
    property DecalageOmbre : integer read FDecalageOmbre ;
    // accès aux lignes
    property Lignes : TStringList read FLignesAffichable ;
    property NBLignes : integer read GetNBLignes ;
    property Ligne[Index:integer]:string read GetLigne ;
    // accès aux mots
    property Mots : TStringArray read FMotsDuTexte ;
    property NBMots : integer read GetNBMots ;
    property Mot[Index:integer]:string read getMot ; // 0 based
    property XYMot[Index:integer]:TPoint read getXYMot ; // coordonnées de chaque mot
    property LargeurDuTrace : integer read FLargeurTrace ;
    property HauteurDuTrace : integer read FHauteurTrace ;
end;



//-----------------------------------------------
//                  DIMENSIONS
//




// renvoi une image de taille maximale pouvant être contenu dans la largeur et hauteur spécifié.
// les proportions sont gardées
// ENTREE:   aImage       = l'image source
//           aLargeurScene = largeur de la scène où doit être affiché l'image
//           aHauteurScene = hauteur de la scène où doit être affiché l'image
// SORTIE:   une nouvelle image redimensionnée. aImage n'est pas touchée.
function TailleMaximaleDeLImage ( aImage : TBGRABitmap ; aLargeurScene , aHauteurScene : integer ) : TBGRABitmap ;

// étire au maxi et centre l'image source dans l'image dest
procedure MaximiseEtCentre( aSource, aDest: TBGRABitmap );


//-----------------------------------------------
//                  TEXTE
//



// calcule la hauteur de Font pour que 'aNBLettreMaxParLigne' caractères
// puissent être affiché sur une scène de aLargeurScene de large
// ENTREE:   aLargeurScene = largeur de la scène où doit être affiché la ligne
//           aFontName     = nom de la font utilisée
//           aFontStyle    = style de la font utilisée
//           aNBLettreMaxParLigne = nombre de caractère maxi que l'on veut par ligne
// SORTIE:   la hauteur de la font
function CalculeLaLargeurMaxiDeLaFont( aLargeurScene : integer ;
                                       aFontName : string ;
                                       aFontStyle : TFontStyles ;
                                       aNBLettreMaxParLigne: integer ) : integer;

function CalculeLaHauteurMaxiDeLaFont( aHauteurScene : integer ;
                                       aFontName : string ;
                                       aFontStyle : TFontStyles ;
                                       aNBLettreMaxParColonne: integer ) : integer;




//-----------------------------------------------
//                  Scrolling
//




// Scrolling d'une image sur elle-même
// ENTREE:   ImaSource  = l'image qu'on veut enrouler
//           NbPixelX   = nombre de pixel à enrouler horizontalement
//           NbPixelY   = nombre de pixel à enrouler verticalement
procedure EnrouleImage ( ImaSource : TBGRABitmap ; NbPixelX , NbPixelY : integer ) ;



//-----------------------------------------------
//                  OPACITE
//




// fixe l'opacité d'une image entière
// ENTREE:   aImage   = l'image dont on veut régler l'opacité
//           aOpacite  = l'opacité voulue
procedure FixeOpaciteDeLImage ( aImage : TBGRABitmap ; aOpacite : byte ) ;






//-----------------------------------------------
//                  COULEUR
//

// teinte une image
// ENTREE:   aImage       = l'image qu'on veut transformer
//           aTeinte      = couleur
procedure TeinteImage(aImage: TBGRABitmap; aTeinte: TBGRAPixel );

// aPercent [0..1]
function PercentColor(aColor: TBGRAPixel; aPercent: single): TBGRAPixel;


// transforme une image en son équivalent monochrome de couleur unique. La transparence est conservée
// ENTREE:   aImage   = l'image qu'on veut transformer
//           aCouleur = couleur de remplacement de tous les pixels
//           aOpacite = nouvelle opacité voulue. -1 garde l'opacité courante des pixels.
procedure ImageMonochrome(aImage: TBGRABitmap; aCouleur: TBGRAPixel; aOpacite : integer ) ;


// permet de coloriser une image qui n'a que des contours noirs antialiasés sur un fond blanc.
// Au plus les pixels d'origine sont noirs, au plus les pixels générés seront opaques.
// Au plus les pixels d'origine sont blancs, au plus le pixels générés seront transparents.
procedure ColoriseUnTracerNoir ( aImage : TBGRABitmap ; aCouleur : TBGRAPixel ) ;
// la version contours blancs sur fond noir
procedure ColoriseUnTracerBlanc ( aImage : TBGRABitmap ; aCouleur : TBGRAPixel ) ;





//-----------------------------------------------
//                  TEXTURE
//
//   finesse>0
function CreateTileableCustomTexture(c1, c2: TBGRAPixel; aWidth, aHeight, finesse: integer): TBGRABitmap;

function CreateTileableMarbleTexture(aWidth, aHeight, finesse: integer): TBGRABitmap; overload;
function CreateTileableMarbleTexture(c1, c2: TBGRAPixel; aWidth, aHeight, finesse: integer): TBGRABitmap; overload;

function CreateTileableWoodTexture(aWidth, aHeight, finesse: integer): TBGRABitmap; overload;
function CreateTileableWoodTexture(c1_1, c1_2, c2_1, c2_2: TBGRAPixel; aWidth, aHeight, finesse: integer): TBGRABitmap; overload;

function CreateTileableVerticalWoodTexture(aWidth, aHeight, finesse: integer): TBGRABitmap; overload;
function CreateTileableVerticalWoodTexture(c1_1, c1_2, c2_1, c2_2: TBGRAPixel; aWidth, aHeight, finesse: integer): TBGRABitmap; overload;


implementation
uses BGRASVG, BGRAUnits;


function SVGFileToBGRABitmap(const aSVGFileName: string; aWidth: integer; aHeight: integer): TBGRABitmap;
var svg: TBGRASVG;
  FWHFactor, cw, ch: single;
begin
 Result := NIL;
 svg:= TBGRASVG.Create( aSVGFileName );
 try
   cw := svg.WidthAsPixel;
   ch := svg.HeightAsPixel;
   FWHFactor := cw/ch;

   if (aWidth > -1) and (aHeight > -1) then begin
     Result := TBGRABitmap.Create(aWidth, aHeight, BGRAPixelTransparent);
     svg.StretchDraw(Result.Canvas2D, 0, 0, Result.Width, Result.Height, True);
     exit;
   end;

   if (aWidth = -1) and (aHeight = -1) then begin
     aWidth := Round(cw);
     aHeight := Round(ch);
   end
   else
   if aWidth = -1
     then aWidth := round(aHeight*FWHFactor)
   else
   if aHeight = -1
     then aHeight := round(aWidth/FWHFactor);

   Result := TBGRABitmap.Create(aWidth, aHeight, BGRAPixelTransparent);
   svg.StretchDraw(Result.Canvas2D, taCenter, tlCenter, 0, 0, Result.Width, Result.Height);
 finally
   svg.Free;
 end;
end;

procedure SVGFileToTImage(aImage: TImage; const aSVGFileName: string);
var ima: TBGRABitmap;
begin
  ima := SVGFileToBGRABitmap(aSVGFileName, aImage.ClientWidth, aImage.ClientHeight);
  ima.AssignToBitmap(aImage.Picture.Bitmap);
  ima.Free;
end;

function BGRAPixelToHex(aColor: TBGRAPixel): string;
begin
 Result := '$'+inttoHex(aColor.red,2)+inttoHex(aColor.green,2)+inttoHex(aColor.blue,2)+inttoHex(aColor.alpha,2);
end;

function HexToBGRAPixel(const s: string): TBGRAPixel;
var ss: string;
begin
 ss := copy( s, 2, 2);
 Result.red:=Hex2Dec(ss);
 ss := copy( s, 4, 2);
 Result.green:=Hex2Dec(ss);
 ss := copy( s, 6, 2);
 Result.blue:=Hex2Dec(ss);
 ss := copy( s, 8, 2);
 Result.alpha:=Hex2Dec(ss);
end;

function variecoul(c: Tcolor; pc: integer): Tcolor;
var r, v, b: integer;
begin
 r := c and $FF;
 v := ( c shr 8  ) and $FF;
 b := ( c shr 16 ) and $FF;
 r := r + ( r * pc ) div 100;
 v := v + ( v * pc ) div 100;
 b := b + ( b * pc ) div 100;
 if r < 0
   then r := 0
   else if r > $FF
     then r := $FF;
 if v < 0
   then v := 0
   else if v > $FF
     then v := $FF;
 if b < 0
   then b := 0
   else if b > $FF
     then b := $FF;
 Result := ( b shl 16 ) or ( v shl 8 ) or  r;
end;

function SommeRVB(c: TColor): integer;
begin
 Result := ( c AND $000000FF ) + ( ( c AND $0000FF00 ) shr 8 ) + ( ( c AND $00FF0000 ) shr 16 ) ;
end;

// renvoi une image de taille maximale pouvant être contenu dans la largeur et hauteur spécifié.
// les proportions sont gardées
// ENTREE:   aImage       = l'image source
//           aLargeurScene = largeur de la scène où doit être affiché l'image
//           aHauteurScene = hauteur de la scène où doit être affiché l'image
// SORTIE:   une nouvelle image redimensionnée. aImage n'est pas touchée.
function TailleMaximaleDeLImage(aImage: TBGRABitmap; aLargeurScene, aHauteurScene: integer): TBGRABitmap;
var rl, rh, r: single;
begin
 rl := aLargeurScene / aImage.Width;
 rh := aHauteurScene / aImage.Height;
 if rl < rh then r := rl else r := rh;
 Result := TBGRABitmap ( aImage.Resample ( round ( r * aImage.Width ) , round ( r * aImage.Height ) , rmFineResample ) ) ;
end;

procedure MaximiseEtCentre(aSource, aDest: TBGRABitmap);
var ima: TBGRABitmap;
  xx, yy: Integer;
begin
 ima := TailleMaximaleDeLImage( aSource, aDest.Width, aDest.Height );
 xx:= (aDest.Width-ima.Width) div 2;
 yy:= (aDest.Height-ima.Height) div 2;
 aDest.PutImage( xx, yy, ima, dmDrawWithTransparency);
 ima.Free;
end;





//-----------------------------------------------
//                  FONT
//




// calcule la hauteur de Font pour que 'aNBLettreMaxParLigne' caractères puissent être affiché sur une scène de aLargeurScene de large
// ENTREE:   aLargeurScene = largeur de la scène où doit être affiché la ligne
//           aFontName     = nom de la font utilisée
//           aFontStyle    = style de la font utilisée
//           aNBLettreMaxParLigne = nombre de caractère maxi que l'on veut par ligne
// SORTIE:   la hauteur de la font
function CalculeLaLargeurMaxiDeLaFont( aLargeurScene : integer ;
                                       aFontName : string ;
                                       aFontStyle : TFontStyles ;
                                       aNBLettreMaxParLigne: integer ) : integer;
var
    ima : TBGRABitmap ;
begin
 ima := TBGRABitmap.Create ( aLargeurScene , 1 );
 ima.FontName := aFontName;
 ima.FontStyle := aFontStyle;
 ima.FontHeight := 1;
 repeat
  ima.FontHeight := ima.FontHeight + 1;
 until ima.TextSize ( 'W' ).cx * aNBLettreMaxParLigne > aLargeurScene;
 Result := ima.FontHeight - 1;
 ima.Free;
end;

function CalculeLaHauteurMaxiDeLaFont(aHauteurScene: integer;
                                      aFontName: string;
                                      aFontStyle: TFontStyles;
                                      aNBLettreMaxParColonne: integer ): integer;
var
    ima : TBGRABitmap ;
begin
 ima := TBGRABitmap.Create ( 1 , aHauteurScene ) ;
 ima.FontName := aFontName ;
 ima.FontStyle := aFontStyle ;
 ima.FontHeight := 1 ;
 repeat
  ima.FontHeight := ima.FontHeight + 1 ;
 until ima.TextSize ( 'L' ).cy * aNBLettreMaxParColonne > aHauteurScene ;
 Result := ima.FontHeight - 1 ;
 ima.Free ;
end;




// Scrolling d'une image sur elle-même
// ENTREE:   ImaSource  = l'image qu'on veut enrouler
//           NbPixelX   = nombre de pixel à enrouler horizontalement
//           NbPixelY   = nombre de pixel à enrouler verticalement
procedure EnrouleImage(ImaSource: TBGRABitmap; NbPixelX, NbPixelY: integer);
var
 temp1 : TBGRABitmap ;
 temp2 : TBGRABitmap ;
begin
 if not assigned ( ImaSource ) then exit ;
 // enroulement horizontal
 if NbPixelX <> 0
   then begin
         temp1 := TBGRABitmap.Create ( ImaSource.Width - abs ( NbPixelX ) , ImaSource.Height ) ;
         temp2 := TBGRABitmap.Create ( abs ( NbPixelX ) , ImaSource.Height ) ;
         if NbPixelX < 0
           then begin
                 temp1.PutImage ( NbPixelX , 0 , ImaSource , dmSet ) ;
                 temp2.PutImage ( 0 , 0 , ImaSource , dmSet ) ;
                 ImaSource.PutImage ( 0 , 0 , temp1 , dmSet ) ;
                 ImaSource.PutImage ( ImaSource.Width + NbPixelX , 0 , temp2 , dmSet ) ;
                end
           else if NbPixelX > 0
                  then begin
                        temp1.PutImage ( 0 , 0 , ImaSource , dmSet ) ;
                        temp2.PutImage ( -(ImaSource.Width-NbPixelX) , 0 , ImaSource , dmSet ) ;
                        ImaSource.PutImage ( NbPixelX , 0 , temp1 , dmSet ) ;
                        ImaSource.PutImage ( 0 , 0 , temp2 , dmSet ) ;
                       end;
         temp1.Free ;
         temp2.Free ;
        end;
 // enroulement vertical
 if NbPixelY <> 0
   then begin
         temp1 := TBGRABitmap.Create ( ImaSource.Width , ImaSource.Height - abs ( NbPixelY ) ) ;
         temp2 := TBGRABitmap.Create ( ImaSource.Width , abs ( NbPixelY ) ) ;
         if NbPixelY < 0
           then begin
                 temp1.PutImage ( 0 , NbPixelY , ImaSource , dmSet ) ;
                 temp2.PutImage ( 0 , 0 , ImaSource , dmSet ) ;
                 ImaSource.PutImage ( 0 , 0 , temp1 , dmSet ) ;
                 ImaSource.PutImage ( 0 , ImaSource.Height + NbPixelY , temp2 , dmSet ) ;
                end
           else if NbPixelY > 0
                  then begin
                        temp1.PutImage ( 0 , 0 , ImaSource , dmSet ) ;
                        temp2.PutImage ( 0 , -(ImaSource.Height-NbPixelY) , ImaSource , dmSet ) ;
                        ImaSource.PutImage ( 0 , NbPixelY , temp1 , dmSet ) ;
                        ImaSource.PutImage ( 0 , 0 , temp2 , dmSet ) ;
                       end;
         temp1.Free ;
         temp2.Free ;
        end;
{ for j:=0 to imaActive.Height-1 do
  begin
   p := imaActive.ScanLine[j] ;
   premierpixel := p^ ;
   for i:=0 to imaActive.Width-1 do
    begin
     p^ := (p+1)^ ;
     inc (p) ;
    end;
   p^ := premierpixel ;
  end;}
end;




procedure FixeOpaciteDeLImage ( aImage : TBGRABitmap ; aOpacite : byte ) ;
var yy , xx : integer ;
    p : PBGRAPixel ;
begin
 if aOpacite = 0 then aOpacite := 1 ;
 for yy:=0 to aImage.Height-1 do
  begin
   p := aImage.ScanLine[yy] ;
   for xx:=1 to aImage.Width do
    begin
     if p^.alpha > 0 then p^.alpha := aOpacite ;
     inc ( p ) ;
    end;
  end;
 aImage.InvalidateBitmap ;
end;




// teinte une image
// ENTREE:   aImage       = l'image qu'on veut transformer
//           aTeinte      = couleur
procedure TeinteImage(aImage: TBGRABitmap; aTeinte: TBGRAPixel ) ;
var yy , xx : integer ;
    p : PBGRAPixel ;
    r,r1:double;
begin
 if aTeinte.alpha = 0 then exit ;
 r := aTeinte.alpha / 255 ;
 r1 := 1 - r ;
 for yy:=0 to aImage.Height-1 do
  begin
   p := aImage.ScanLine[yy] ;
   for xx:=1 to aImage.Width do
    begin
     if p^.alpha > 0
       then begin
             p^.blue := round(p^.blue*r1 + aTeinte.blue*r);
             p^.green := round(p^.green*r1 + aTeinte.green*r);
             p^.red := round(p^.red*r1 + aTeinte.red*r);
            end;
     inc ( p ) ;
    end;
  end;
end;

function PercentColor(aColor: TBGRAPixel; aPercent: single): TBGRAPixel;
begin
 Result.Blue := EnsureRange(round(aColor.blue*aPercent), 0, 255);
 Result.green := EnsureRange(round(aColor.blue*aPercent), 0, 255);
 Result.red := EnsureRange(round(aColor.red*aPercent), 0, 255);
 Result.alpha := aColor.alpha;
end;



// transforme une image en son équivalent monochrome de couleur unique. La transparence est conservée
// ENTREE:   aImage   = l'image qu'on veut transformer
//           aCouleur = couleur de remplacement de tous les pixels
//           aOpacite = nouvelle opacité voulue. -1 garde l'opacité courante des pixels
procedure ImageMonochrome(aImage: TBGRABitmap; aCouleur: TBGRAPixel; aOpacite : integer ) ;
var yy , xx : integer ;
    p : PBGRAPixel ;
    alpha : byte ;
begin
 if aOpacite = 0 then aOpacite := 1 ;
 if aOpacite = -1
   then begin // on ne touche pas à l'opacité
         for yy:=0 to aImage.Height-1 do
          begin
           p := aImage.ScanLine[yy] ;
           for xx:=1 to aImage.Width do
            begin
             p^.blue := aCouleur.blue ;
             p^.green := aCouleur.green ;
             p^.red := aCouleur.red ;
             inc ( p ) ;
            end;
          end;
        end
   else begin  // on fixe une opacité
         alpha := aOpacite ;
         for yy:=0 to aImage.Height-1 do
          begin
           p := aImage.ScanLine[yy] ;
           for xx:=1 to aImage.Width do
            begin
             p^.blue := aCouleur.blue ;
             p^.green := aCouleur.green ;
             p^.red := aCouleur.red ;
             if p^.alpha > 0 then p^.alpha := alpha ;  // on ne touche pas aux pixels transparent
             inc ( p ) ;
            end;
          end;
        end;
 aImage.InvalidateBitmap ;
end;


// permet de coloriser une image qui n'a que des contours noirs antialiasés sur du blanc.
// Au plus les pixels d'origine sont noirs, au plus les pixels générés seront opaques.
// Au plus les pixels d'origine sont blancs, au plus le pixels générés seront transparents.
procedure ColoriseUnTracerNoir(aImage: TBGRABitmap; aCouleur: TBGRAPixel);
var yy , xx : integer ;
    p : PBGRAPixel ;
    alpha : byte ;
begin
 for yy:=0 to aImage.Height-1 do
  begin
   p := aImage.ScanLine[yy] ;
   for xx:=1 to aImage.Width do
    begin
     alpha := 255 - ( p^.blue + p^.green + p^.red ) div 3 ;
     p^.blue := aCouleur.blue ;
     p^.green := aCouleur.green ;
     p^.red := aCouleur.red ;
     if p^.alpha > 0 then p^.alpha := alpha ;  // on ne touche pas aux pixels transparent
     inc ( p ) ;
    end;
  end;
end;

// la version contours blancs sur fond noir
procedure ColoriseUnTracerBlanc(aImage: TBGRABitmap; aCouleur: TBGRAPixel);
var yy , xx : integer ;
    p : PBGRAPixel ;
    alpha : byte ;
begin
 for yy:=0 to aImage.Height-1 do
  begin
   p := aImage.ScanLine[yy] ;
   for xx:=1 to aImage.Width do
    begin
     alpha := ( p^.blue + p^.green + p^.red ) div 3 ;
     p^.blue := aCouleur.blue ;
     p^.green := aCouleur.green ;
     p^.red := aCouleur.red ;
     if p^.alpha > 0 then p^.alpha := alpha ;  // on ne touche pas aux pixels transparent
     inc ( p ) ;
    end;
  end;
end;

{ TFormateTexte }

constructor TFormateTexte.Create;
begin
 FLignesAffichable := TStringList.Create ;
 FImaTampon := TBGRABitmap.Create ( 1 , 1 ) ;
end;

destructor TFormateTexte.Destroy;
begin
 FLignesAffichable.Free ;
 FImaTampon.Free ;
 Finalize ( TabloXYMot ) ;
 inherited Destroy;
end;

function TFormateTexte.GetLigne(Index: integer): string;
begin
 Result := FLignesAffichable.Strings[Index] ;
end;

function TFormateTexte.GetLargeurLettre(Lettre: string): integer;
begin
 Result := FImaTampon.TextSize ( Lettre ).cx ;
end;

function TFormateTexte.GetHauteurLettre(Lettre: string): integer;
begin
 Result := FImaTampon.TextSize ( Lettre ).cy ;
end;

function TFormateTexte.getMot(Index: integer): string;
begin
 Result := FMotsDuTexte[Index] ;
end;

function TFormateTexte.getXYMot(Index: integer): TPoint;
begin
 Result.x := TabloXYMot[Index].x ;
 Result.y := TabloXYMot[Index].y ;
end;

function TFormateTexte.GetNBLignes: integer;
begin
 Result := FLignesAffichable.Count ;
end;

function TFormateTexte.GetNBMots: integer;
begin
 Result := Length(FMotsDuTexte) ;
end;


procedure TFormateTexte.Formate(aTexte: string; aRect : TRect;
  aFontName: string; aFontStyle: TFontStyles; aAlignement: TAlignement);
begin
 Formate ( aTexte , aRect.Left , aRect.Top, aRect.Right , aRect.Bottom , aFontName , aFontStyle , aAlignement ) ;
end;

procedure TFormateTexte.Formate(aTexte: string; aXCadre, aYCadre, aLargeurCadre,
      aHauteurCadre: integer; aFontName: string; aFontStyle: TFontStyles; aAlignement : TAlignement);
begin
 // on récupère les données
 FTexte := aTexte ;
 FXCadre := aXCadre ;
 FYCadre := aYCadre ;
 FLargeurCadre := aLargeurCadre ;
 FHauteurCadre := aHauteurCadre ;
 FFontName := aFontName ;
 FFontStyle := aFontStyle ;
 FImaTampon.FontName := aFontName ;
 FImaTampon.FontStyle := aFontStyle ;
 FAlignement := aAlignement ;
 // on découpe le texte en mots distincts
 FMotsDuTexte := aTexte.Split([' ']);

 // on prépare la taille du tableau contenant les coordonnées de chaque mot
 SetLength ( TabloXYMot , Length( FMotsDuTexte ) ) ;

 // on fixe une hauteur de font arbitraire ...?
 FHauteurFont := 50 ;
 SimuleLeTrace ( FHauteurFont ) ;
 if ( FHauteurTrace <= aHauteurCadre ) and ( FLargeurTrace <= aLargeurCadre )
   then begin // le tracé est plus petit que l'espace autorisé => on essai de l'augmenter
         repeat
          inc ( FHauteurFont ) ;
          SimuleLeTrace ( FHauteurFont ) ;
         until ( FHauteurTrace > aHauteurCadre ) or ( FLargeurTrace > aLargeurCadre ) ;
         dec ( FHauteurFont ) ;
         SimuleLeTrace ( FHauteurFont ) ;
        end
   else begin // le tracé est plus grand que l'espace autorisé => on le diminue
         repeat
          dec ( FHauteurFont ) ;
          SimuleLeTrace ( FHauteurFont ) ;
         until ( FHauteurTrace <= aHauteurCadre ) and ( FLargeurTrace <= aLargeurCadre ) ;
        end;

 // ici la hauteur de font a été trouvée
 FDecalageOmbre := FHauteurFont * 5 div 100 ;
 if FDecalageOmbre < 3 then FDecalageOmbre := 3 ;

 // on aligne les mots dans le cadre
 case ord( aAlignement ) of
  ord(alGauche) : AligneLesMotsAGauche ;
  ord(alCentre) : AligneLesMotsAuCentre ;
  ord(alDroite) : AligneLesMotsADroite ;
  ord (alJustifier) : JustifierLesMots ;
 end;//case

 CentreLesMotsVerticalement ;
end;

// force la hauteur de caractère
procedure TFormateTexte.SetHauteurFont(AValue: integer);
begin
 if FHauteurFont = AValue then Exit ;
 FHauteurFont := AValue ;
 SimuleLeTrace ( FHauteurFont ) ;
 // ici la hauteur de font a été trouvée
 FDecalageOmbre := FHauteurFont * 5 div 100 ;
 if FDecalageOmbre < 3 then FDecalageOmbre := 3 ;
 // on aligne les mots dans le cadre
 case ord( FAlignement ) of
  ord(alGauche) : AligneLesMotsAGauche ;
  ord(alCentre) : AligneLesMotsAuCentre ;
  ord(alDroite) : AligneLesMotsADroite ;
  ord (alJustifier) : JustifierLesMots ;
 end;//case aAlignement
 CentreLesMotsVerticalement ;
end;


procedure TFormateTexte.RechercheLesIndexDesMotsDeLaLigne ( aNumLigne : integer ; var iDebut , iFin : integer ) ;
var i : integer ;
begin
 iDebut := -1 ;
 iFin := -1 ;
 for i:=low(TabloXYMot) to high(TabloXYMot) do
  begin
   if TabloXYMot[i].IndexLigne = aNumLigne
     then begin
           if iDebut = -1 then iDebut := i ;
           iFin := i ;
          end
     else if iDebut <> -1 then exit ; // on abrège la recherche
  end;
end;


procedure TFormateTexte.AligneLesMotsAGauche ;
var i , j : integer ;
  larg : integer ;
  idebut , ifin : integer ;
begin
 for i:=0 to NBLignes-1 do
  begin
   RechercheLesIndexDesMotsDeLaLigne ( i , idebut{%H-} , ifin{%H-} ) ;
   larg := 0 ;
   for j:=idebut to ifin do
    begin
     TabloXYMot[j].x := larg ;
     larg := larg + TabloXYMot[j].Largeur + FImaTampon.TextSize ( ' ' ).cx ;
    end;
  end;
end;
procedure TFormateTexte.AligneLesMotsAuCentre ;
var i , j : integer ;
  larg : integer ;
  idebut , ifin : integer ;
begin
 for i:=0 to NBLignes-1 do
  begin
   RechercheLesIndexDesMotsDeLaLigne ( i , idebut{%H-} , ifin{%H-} ) ;
   larg := TabloXYMot[ifin].x + TabloXYMot[ifin].Largeur - TabloXYMot[idebut].x ; // largeur totale de la ligne
   larg := ( FLargeurCadre - larg ) div 2 ;
   for j:=idebut to ifin do
    begin
     TabloXYMot[j].x := larg ;
     larg := larg + TabloXYMot[j].Largeur + FImaTampon.TextSize ( ' ' ).cx ;
    end;
  end;
end;

procedure TFormateTexte.CentreLesMotsVerticalement ;
var i , j : integer ;
  idebut , ifin : integer ;
  haut : integer ;
  yy : integer ;
begin
 haut := 0 ;
 for i:=0 to NBLignes-1 do haut := haut + FImaTampon.TextSize ( Ligne[i] ).cy ;
 yy := ( FHauteurCadre - haut ) div 2 ;
 for i:=0 to NBLignes-1 do
  begin
   RechercheLesIndexDesMotsDeLaLigne ( i , idebut{%H-} , ifin{%H-} ) ;
   for j:=idebut to ifin do TabloXYMot[j].y := yy ;
   yy := yy + FImaTampon.TextSize ( Ligne[i] ).cy ;
  end;
end;


procedure TFormateTexte.AligneLesMotsADroite ;
var i , j : integer ;
  larg : integer ;
  idebut , ifin : integer ;
begin
 for i:=0 to NBLignes-1 do
  begin
   RechercheLesIndexDesMotsDeLaLigne ( i , idebut{%H-} , ifin{%H-} ) ;
   larg := FLargeurCadre + FImaTampon.TextSize ( ' ' ).cx ;
   for j:=ifin downto idebut do
    begin
     larg := larg - TabloXYMot[j].Largeur - FImaTampon.TextSize ( ' ' ).cx ;
     TabloXYMot[j].x := larg ;
    end;
  end;
end;

procedure TFormateTexte.JustifierLesMots;
begin

end;


// simule le tracé des lignes de textes
procedure TFormateTexte.SimuleLeTrace ( aHauteurFont : integer ) ;
var
  i  : integer ;
  xx , yy : integer ;
  largmot : integer ;
  largespace : integer ;
  line : string ;
  indexligne : integer ;
begin
 FImaTampon.FontHeight := aHauteurFont ;
 FLignesAffichable.Clear ;
 FHauteurTrace := 0 ;
 FLargeurTrace := 0 ;
 if Length(FMotsDuTexte) = 0 then exit ;
 line := '' ;
 largespace := FImaTampon.TextSize(' ').cx ; // largeur en pixel d'un espace
 xx := -largespace ;
 yy := 0 ;
 indexligne := 0;
 for i:=0 to Length( FMotsDuTexte )-1 do
  begin
   largmot := FImaTampon.TextSize( FMotsDuTexte[i] ).cx ;
   if xx+largespace+largmot <= FLargeurCadre
     then begin // on peut rester sur la même ligne
           TabloXYMot[i].x := xx + largespace ; TabloXYMot[i].y := yy ; TabloXYMot[i].IndexLigne := indexligne ; TabloXYMot[i].Largeur := largmot ;
           xx := xx + largespace + largmot ;
           if line='' then line := FMotsDuTexte[i]
                      else line := line + ' ' + FMotsDuTexte[i] ;
          end
     else begin  // on passe à la ligne suivante
           if FLargeurTrace < xx then FLargeurTrace := xx ;
           yy := yy + FImaTampon.TextSize( FMotsDuTexte[i] ).cy ;
           inc ( indexligne ) ;
           TabloXYMot[i].x := 0 ; TabloXYMot[i].y := yy ; TabloXYMot[i].IndexLigne := indexligne ; TabloXYMot[i].Largeur := largmot ;
           xx := largmot ;
           FHauteurTrace := FHauteurTrace + FImaTampon.TextSize( FMotsDuTexte[i] ).cy ;
           FLignesAffichable.Add ( line ) ;
           line := FMotsDuTexte[i] ;
          end;
  end;
 if line <> ''
   then begin
         FLignesAffichable.Add ( line ) ;
         FHauteurTrace := FHauteurTrace + FImaTampon.TextSize( line ).cy ;
         if FLargeurTrace < xx then FLargeurTrace := xx ;
        end;
end;



procedure TFormateTexte.AfficheLesLignesSur(aImage: TBGRABitmap; aCouleurLettre: TBGRAPixel);
begin
 AfficheLesLignesSur ( aImage , aCouleurLettre , FALSE , BGRABlack ) ;
end;
procedure TFormateTexte.AfficheLesLignesSur(aImage: TBGRABitmap; aCouleurLettre, aCouleurOmbre: TBGRAPixel);
begin
 AfficheLesLignesSur ( aImage , aCouleurLettre , TRUE , aCouleurOmbre ) ;
end;

procedure TFormateTexte.AfficheLesLignesSur(aImage: TBGRABitmap;aCouleurLettre, aCouleurContour, aCouleurOmbre: TBGRAPixel);
var i : integer ;
  xx , yy : integer ;
  txt : string ;
  renderer: TBGRATextEffectFontRenderer;
begin
 renderer := TBGRATextEffectFontRenderer.Create;
 aImage.FontRenderer := renderer;
 renderer.ShadowVisible := True;
 renderer.ShadowColor := aCouleurOmbre;
 renderer.ShadowOffset.x := round(FHauteurFont*0.06);
 renderer.ShadowOffset.y := round(FHauteurFont*0.06);
 renderer.ShadowRadius := round(FHauteurFont*0.04);
 renderer.OutlineVisible := True;
 renderer.OutlineColor := aCouleurContour;
 renderer.OuterOutlineOnly := True;
 renderer.OutlineWidth := FHauteurFont*0.04;
 aImage.FontQuality:= fqFineAntialiasing;
// aImage.FontAntialias := TRUE ;
 aImage.FontHeight := FHauteurFont ;
 aImage.FontName := FFontName ;
 aImage.FontStyle := FFontStyle ;
 for i:=low(TabloXYMot) to high(TabloXYMot) do
  begin
   txt :=  FMotsDuTexte[i] ;
   xx := FXCadre + TabloXYMot[i].x ;
   yy := FYCadre + TabloXYMot[i].y ;
   aImage.TextOut ( xx , yy , txt , aCouleurLettre ) ;
  end;
end;


procedure TFormateTexte.AfficheLesLignesSur(aImage: TBGRABitmap; aCouleurLettre : TBGRAPixel; aOmbre : boolean ; aCouleurOmbre : TBGRAPixel ) ;
var i : integer ;
  xx , yy : integer ;
  txt : string ;
begin
 aImage.FontAntialias := TRUE ;
 aImage.FontHeight := FHauteurFont ;
 aImage.FontName := FFontName ;
 aImage.FontStyle := FFontStyle ;
 for i:=low(TabloXYMot) to high(TabloXYMot) do
  begin
   txt :=  FMotsDuTexte[i] ;
   xx := FXCadre + TabloXYMot[i].x ;
   yy := FYCadre + TabloXYMot[i].y ;
   if aOmbre then aImage.TextOut ( xx + FDecalageOmbre , yy + FDecalageOmbre , txt , aCouleurOmbre ) ;
   aImage.TextOut ( xx , yy , txt , aCouleurLettre ) ;
  end;
end;


//// TEXTURE
function Interp256(value1,value2,position: integer): integer; inline;
begin
 result := (value1*(256-position) + value2*position) shr 8;
end;

function Interp256(color1,color2: TBGRAPixel; position: integer): TBGRAPixel; inline;
begin
 result.red := Interp256(color1.red,color2.red, position);
 result.green := Interp256(color1.green,color2.green, position);
 result.blue := Interp256(color1.blue,color2.blue, position);
 result.alpha := Interp256(color1.alpha,color2.alpha, position);
end;

function CreateTileableCustomTexture(c1, c2: TBGRAPixel; aWidth, aHeight, finesse: integer): TBGRABitmap;
var
  colorOscillation: integer;
  p: PBGRAPixel;
  i: Integer;
begin
  result := CreateCyclicPerlinNoiseMap(aWidth, aHeight, 1, 1, 1);
  p := result.Data;
  for i := 0 to result.NbPixels-1 do
  begin
    colorOscillation := round(power((sin(p^.red*Pi/finesse)+1)/2,0.2)*256); //80)+1)/2,0.2)*256);
    p^ := Interp256(c1, c2, colorOscillation);
    inc(p);
  end;
end;

function CreateTileableMarbleTexture(aWidth, aHeight, finesse: integer): TBGRABitmap;
begin
 Result := CreateTileableMarbleTexture( BGRA(181,157,105), BGRA(228,227,180), aWidth, aHeight, finesse);
end;

function CreateTileableMarbleTexture(c1, c2: TBGRAPixel; aWidth, aHeight, finesse: integer): TBGRABitmap;
var
   colorOscillation: integer;
   p: PBGRAPixel;
   i: Integer;
begin
 result := CreateCyclicPerlinNoiseMap(aWidth, aHeight,1,1,1);
 p := result.Data;
 for i := 0 to result.NbPixels-1 do
  begin
   colorOscillation := round(power((sin(p^.red*Pi/finesse)+1)/2,0.2)*256);
   p^ := Interp256(c1, c2, colorOscillation);
   inc(p);
 end;
end;

function CreateTileableWoodTexture(aWidth, aHeight, finesse: integer): TBGRABitmap;
begin
 Result := CreateTileableWoodTexture(BGRA(247,188,120), BGRA(255,218,170),
                    BGRA(157,97,60), BGRA(202,145,112), aWidth, aHeight, finesse);
end;

function CreateTileableWoodTexture(c1_1, c1_2, c2_1, c2_2: TBGRAPixel; aWidth, aHeight, finesse: integer ): TBGRABitmap;
var
  colorOscillation, globalColorVariation: integer;
  p: PBGRAPixel;
  i: Integer;
begin
  Result := CreateCyclicPerlinNoiseMap(aWidth, aHeight);
  p := result.Data;
  for i := 0 to result.NbPixels-1 do
  begin
    colorOscillation := round(sqrt((sin(p^.red*Pi/finesse)+1)/2)*256); //16)+1)/2)*256);
    globalColorVariation := p^.red;
    p^:= Interp256( Interp256(c1_1, c1_2, colorOscillation),
                    Interp256(c2_1, c2_2, colorOscillation), globalColorVariation);
    inc(p);
  end;
end;

function CreateTileableVerticalWoodTexture(aWidth, aHeight, finesse: integer): TBGRABitmap;
begin
 Result := CreateTileableVerticalWoodTexture(BGRA(247,188,120),BGRA(255,218,170),
                           BGRA(157,97,60),BGRA(202,145,112), aWidth, aHeight, finesse);
end;

function CreateTileableVerticalWoodTexture(c1_1, c1_2, c2_1, c2_2: TBGRAPixel; aWidth,
  aHeight, finesse: integer): TBGRABitmap;
var
  globalPos: single;
  colorOscillation, globalColorVariation: integer;
  p: PBGRAPixel;
  i: Integer;
  x: integer;
begin
  result := CreateCyclicPerlinNoiseMap(aWidth, aHeight);
  p := result.Data;
  x := 0;
  for i := 0 to result.NbPixels-1 do
  begin
    globalPos := p^.red*Pi/32+x*2*Pi/aWidth*8;
    colorOscillation := round(sqrt((sin(globalPos)+1)/finesse)*256);
    globalColorVariation := round(sin(globalPos/8)*128+128);
    p^:= Interp256( Interp256(c1_1, c1_2,colorOscillation),
                    Interp256(c2_1, c2_2,colorOscillation), globalColorVariation);
    inc(p);
    inc(x);
    if x = aWidth then x := 0;
  end;
end;




end.

