unit GopherSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GopherConsts,
  GopherAction,
  GopherServer,
  GopherClient;

const
  SNoResults = 'No results found';

type
  TGopherSearch = class(TGopherAction)
  private
    FRecursive: Boolean;
  public
    procedure AfterConstruction; override;
    property Recursive: Boolean read FRecursive write FRecursive;
  end;

  TGopherLocalSearch = class(TGopherSearch)
  private
    FSearchDir: string;
    FRootDir: string;
    FGopherFileName: string;
    FCount: Integer;
    FItemTypes: TStringList;
    procedure DoSearch(Dir: string);
    function IsMatch(FileName, Text: string): Boolean;
  public
    procedure Respond(Sender: TObject); override;
    property SearchDir: string read FSearchDir write FSearchDir;
  end;

  TGopherRemoteSearch = class(TGopherSearch)
  private
    FURI: string;
    FHost: string;
    FPort: Word;
    FSelector: string;
    FCount: Integer;
    procedure SearchSelector(Dir: string);
  public
    procedure Respond(Sender: TObject); override;
    property URI: string read FURI write FURI;
    property Host: string read FHost write FHost;
    property Port: Word read FPort write FPort;
    property Selector: string read FSelector write FSelector;
  end;

implementation

uses
  StrUtils,
  IniFiles,
  fpMimeTypes;

procedure TGopherSearch.AfterConstruction;
begin
  inherited;
  FRecursive := True
end;

function TGopherLocalSearch.IsMatch(FileName, Text: string): Boolean;
var
  Content: TStringList;
  i: Integer;
begin
  Content := TStringList.Create;
  Content.LoadFromFile(FileName);

  Result := True;
  i := 0;

  while Result and (i < Search.Count) do
  begin
    Result := AnsiContainsText(FileName, Search[i]) or
              AnsiContainsText(Text, Search[i]) or
              AnsiContainsText(Content.Text, Search[i]);
    Inc(i)
  end;

  Content.Free
end;

procedure TGopherLocalSearch.DoSearch(Dir: string);
var
  SearchRec: TSearchRec;
  Mime: string;
  ItemType: string;
  ItemText: string;
  ItemPath: string;
  Path: string;
  Items: TStringList;
  i: Integer;
  MenuFile: TIniFile;
begin
  Dir := IncludeTrailingPathDelimiter(Dir);

  if FileExists(Dir + FGopherFileName) then
  begin
    MenuFile := TIniFile.Create(Dir + FGopherFileName);
    Items := TStringList.Create;

    MenuFile.ReadSections(Items);

    for i := 0 to Items.Count - 1 do
    begin
      ItemType := MenuFile.ReadString(Items[i], 'type', giPlainText);
      ItemType := GetGopherType(ItemType);

      ItemText := MenuFile.ReadString(Items[i], 'text', Items[i]);
      ItemPath := MenuFile.ReadString(Items[i], 'path', Dir + Items[i]);

      if Recursive and (ItemType = giDirectory) then
      begin
        DoSearch(ItemPath)
      end
      else if FileExists(ItemPath) and IsMatch(ItemPath, ItemText) then
      begin
        Path := Copy(Dir, Length(FRootDir) + 1, Length(Dir)) +
                Items[i];
        Path := IncludeLeadingPathDelimiter(Path);

        WriteItem(
          Path,
          ItemType,
          ItemText,
          Path
        );

        Inc(FCount)
      end
    end;

    MenuFile.Free;
    Items.Free
  end
  else if FindFirst(Dir + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if Recursive and ((SearchRec.Attr and faDirectory) <> 0) then
        begin
          DoSearch(Dir + SearchRec.Name)
        end
        else if IsMatch(Dir + SearchRec.Name, '') then
        begin
          Mime := ''; { bug in 2.6.4 }
          Mime := MimeTypes.GetMimeType(ExtractFileExt(SearchRec.Name));

          if FItemTypes.Values[Mime] = '' then
          begin
            ItemType := giPlainText
          end else
          begin
            ItemType := FItemTypes.Values[Mime]
          end;

          Path := Copy(Dir, Length(FRootDir) + 1, Length(Dir)) +
                  SearchRec.Name;
          Path := IncludeLeadingPathDelimiter(Path);

          WriteItem(
            Path,
            ItemType,
            Path,
            Path
          );

          Inc(FCount)
        end
      end
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec)
  end
end;

procedure TGopherLocalSearch.Respond(Sender: TObject);
begin
  with (Sender as TGopherServer) do
  begin
    FRootDir := RootDir;
    FItemTypes := ItemTypes;
    FGopherFileName := GopherFileName
  end;

  FCount := 0;

  DoSearch(FRootDir + IncludeLeadingPathDelimiter(FSearchDir));

  if FCount = 0 then
  begin
    WriteError('No results found.')
  end
end;

procedure TGopherRemoteSearch.SearchSelector(Dir: string);
var
  Match: Boolean;
  i: Integer;
  k: Integer;
  Item: TGopherMenuItem;
begin
  with TGopherClient.Create do
  begin
    Host := FHost;
    Port := FPort;
    Selector := Dir;
    Get;

    for i := 0 to Contents.Count - 1 do
    begin
      Item := MenuItems[i];

      if Recursive and (Item.GopherType = giDirectory) then
      begin
        SearchSelector(Item.Selector)
      end else
      begin
        Selector := Item.Selector;
        
        Match := True;
        k := 0;

        while Match and (k < Search.Count) do
        begin
          Match := AnsiContainsText(Item.Text, Search[k]) or
                   AnsiContainsText(Item.Selector, Search[k]) or
                   AnsiContainsText(Contents.Text, Search[k]);
          Inc(k)
        end;

        if Match then
        begin
          WriteItem(
            Item.Selector,
            Item.GopherType,
            Item.Text,
            Item.Selector,
            Item.Host,
            Item.Port
          );
          Inc(FCount)
        end
      end
    end;

    Free
  end
end;

procedure TGopherRemoteSearch.Respond(Sender: TObject);
begin
  if FURI <> '' then
  begin
    with TGopherClient.Create do
    begin
      ParseURI(FURI);
      FHost := Host;
      FPort := Port;
      FSelector := Selector;
      Free
    end
  end;

  FCount := 0;

  SearchSelector(FSelector);

  if FCount = 0 then
  begin
    WriteError(SNoResults)
  end
end;

end.
