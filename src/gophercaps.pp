unit GopherCaps;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GopherConsts,
  GopherAction,
  GopherServer;

const
  GopherCapsVersion = '1';

type
  TGopherCaps = class(TGopherAction)
  private class var
    FExpire: Integer;
  public
    procedure Respond(Sender: TObject); override;
    class constructor Create;
    class property Expire: Integer read FExpire write FExpire;
  end;

  TGopherCapsClass = class of TGopherCaps;

function GopherCaps: TGopherCapsClass;

implementation

procedure TGopherCaps.Respond(Sender: TObject);
var
  Caps: TStringList;
begin
  Caps := TStringList.Create;

  IsMenu := False;

  Caps.Add('CAPS');
  Caps.Values['CapsVersion'] := GopherCapsVersion;
  Caps.Values['ExpireCapsAfter'] := IntToStr(FExpire);
  Caps.Values['PathDelimiter'] := gsPath;
  Caps.Values['PathIdentity'] := '.';
  Caps.Values['PathParent'] := '..';
  Caps.Values['PathKeepPreDelimiter'] := 'FALSE';
  Caps.Values['ServerSoftware'] := (Sender as TGopherServer).Version;
  Caps.Values['ServerSoftwareVersion'] := (Sender as TGopherServer).Version;
  Caps.Values['ServerDescription'] := (Sender as TGopherServer).Version;
  Caps.Values['ServerAdmin'] := (Sender as TGopherServer).AdminEmail;

  Caps.SaveToStream(ContentStream);

  Caps.Free
end;

class constructor TGopherCaps.Create;
begin
  Register('caps.txt')
end;

function GopherCaps: TGopherCapsClass;
begin
  Result := TGopherCaps
end;

end.
