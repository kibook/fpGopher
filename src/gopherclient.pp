unit GopherClient;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SSockets,
  GopherConsts;

type
  TGopherMenuItem = record
    GopherType: Char;
    Text: string;
    Selector: string;
    Host: string;
    Port: Word;
  end;

  TGopherClient = class
  private
    FSocket: TInetSocket;
    FHost: string;
    FPort: Word;
    FSelector: string;
    FContents: TStrings;
    function GetMenuItem(Index: Integer): TGopherMenuItem;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure ParseURI(URI: string);
    function Get: string;
    function Get(URI: string): string;
    class function SimpleGet(URI: string): string;
    property Host: string read FHost write FHost;
    property Port: Word read FPort write FPort;
    property Selector: string read FSelector write FSelector;
    property Contents: TStrings read FContents;
    property MenuItems[Index: Integer]: TGopherMenuItem read GetMenuItem;
  end;

implementation

function TGopherClient.GetMenuItem(Index: Integer): TGopherMenuItem;
var
  Fields: TStringList;
begin
  Fields := TStringList.Create;
  Fields.Delimiter := gsField;
  Fields.StrictDelimiter := True;
  Fields.DelimitedText := FContents[Index];

  Result.GopherType := Fields[0][1];
  Result.Text := Copy(Fields[0], 2, Length(Fields[0]));
  Result.Selector := Fields[1];
  Result.Host := Fields[2];
  Result.Port := StrToInt(Fields[3])
end;

procedure TGopherClient.AfterConstruction;
begin
  inherited;
  FHost := '';
  FPort := GopherPort;
  FSelector := '';
  FContents := TStringList.Create
end;

procedure TGopherClient.BeforeDestruction;
begin
  inherited;
  FContents.Free
end;

function TGopherClient.Get: string;
var
  Data: string;
  Part: string;
  Buf: array [0..4095] of Char;
  Count: Integer;
begin
  FSocket := TInetSocket.Create(FHost, FPort);

  Data := FSelector + gsMenuItem;
  Count := Length(Data) + SizeOf(Data[1]);
  FSocket.Write(Data[1], Count);

  Data := '';
  repeat
    Part := '';
    repeat
      Count := FSocket.Read(Buf, SizeOf(Buf));
      Part := Part + Copy(Buf, 0, Count)
    until Count < SizeOf(Buf);
    Data := Data + Part
  until Part = '';

  with FContents do
  begin
    Text := Data;
    if (Count - 1 > 0) and (Strings[Count - 1] = gsTerminate) then
    begin
      Delete(Count - 1)
    end;
    Result := Text
  end;

  FSocket.Free
end;

procedure TGopherClient.ParseURI(URI: string);
begin
  if Pos(GopherURI, URI) <> 0 then
  begin
    URI := Copy(URI, Pos(GopherURI, URI) + Length(GopherURI), Length(URI))
  end;

  if Pos(gsPath, URI) = 0 then
  begin
    FSelector := gsPath
  end else
  begin
    FSelector := Copy(URI, Pos(gsPath, URI), Length(URI))
  end;

  if Pos(':', URI) = 0 then
  begin
    if Pos(gsPath, URI) = 0 then
    begin
      FHost := Copy(URI, 1, Length(URI))
    end else
    begin
      FHost := Copy(URI, 1, Pos(gsPath, URI) - 1)
    end;
    FPort := GopherPort
  end else
  begin
    FHost := Copy(URI, 1, Pos(':', URI) - 1);
    FPort := StrToInt(Copy(URI, Pos(':', URI) + 1, Pos(gsPath, URI) - 1))
  end
end;

function TGopherClient.Get(URI: string): string;
begin
  ParseURI(URI);
  Result := Get
end;

class function TGopherClient.SimpleGet(URI: string): string;
var
  Client: TGopherClient;
begin
  Client := TGopherClient.Create;
  Client.Get(URI);
  Result := Client.Contents.Text;
  Client.Free
end;

end.
