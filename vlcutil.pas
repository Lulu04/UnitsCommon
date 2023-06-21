unit VLCUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Dialogs,
  PasLibVlcUnit;


type

{ TVLCVideoReader }

TVLCVideoReader = class
private
  p_li : libvlc_instance_t_ptr;
  p_mi : libvlc_media_player_t_ptr;
  FHandle: THandle;
  function GetVolume: integer;
  procedure SetVolume(AValue: integer);
public
  constructor Create;
  Destructor Destroy; override;

  procedure Init( aHandle: THandle );
  procedure Play(fileName: WideString);
  procedure Stop;
  property Volume: integer read GetVolume write SetVolume; // [0..100]
end;

implementation

{ TVLCVideoReader }

function TVLCVideoReader.GetVolume: integer;
begin
 if p_mi <> NIL
   then Result := libvlc_audio_get_volume( p_mi )
   else Result := 0;
end;

procedure TVLCVideoReader.SetVolume(AValue: integer);
begin
 if p_mi <> NIL
   then libvlc_audio_set_volume( p_mi, AValue );

end;

constructor TVLCVideoReader.Create;
begin
  p_li := NIL;
  p_mi := NIL;
  FHandle := 0;
  libvlc_dynamic_dll_init();

  if (libvlc_dynamic_dll_error <> '') then
  begin
    MessageDlg(libvlc_dynamic_dll_error, mtError, [mbOK], 0);
    exit;
  end;

  with TArgcArgs.Create([
    WideString(libvlc_dynamic_dll_path),
    '--intf=dummy',
    '--ignore-config',
    '--quiet',
    '--no-video-title-show',
    '--no-video-on-top'
  ]) do
  begin
    p_li := libvlc_new(ARGC, ARGS);
    Free;
  end;

  p_mi := NIL;
end;

destructor TVLCVideoReader.Destroy;
begin
  if (p_li <> NIL) then
  begin
    Stop;
    libvlc_release(p_li);
    p_li := NIL;
  end;
 inherited Destroy;
end;

procedure TVLCVideoReader.Init(aHandle: THandle);
begin
 Stop;
 FHandle := aHandle;
end;

procedure TVLCVideoReader.Play(fileName: WideString);
var
  p_md : libvlc_media_t_ptr;
  a_st : AnsiString;
  p_st : PAnsiChar;
begin
  Stop;

  a_st := UTF8Encode(fileName);
  p_st := PAnsiChar(@a_st[1]);

  p_md := libvlc_media_new_path(p_li, p_st);

  if (p_md <> NIL) then
  begin
    p_mi := libvlc_media_player_new_from_media(p_md);
    if (p_mi <> NIL) then
    begin
      libvlc_video_set_key_input(p_mi, 1);
      libvlc_video_set_mouse_input(p_mi, 1);
      libvlc_media_player_set_display_window(p_mi, FHandle);
    end;
    libvlc_media_player_play(p_mi);
    libvlc_media_release(p_md);
  end;
end;

procedure TVLCVideoReader.Stop;
begin
  if (p_mi <> NIL) then
  begin
    libvlc_media_player_stop(p_mi);
    libvlc_media_player_release(p_mi);
    p_mi := NIL;
  end;
end;

end.

