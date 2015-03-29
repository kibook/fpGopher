unit GopherAction;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  IniFiles,
  fpTemplate,
  GopherConsts;

type
  TGopherAction = class
  private
    FCount: Integer;
    FRemoteAddr: string;
    FSelector: string;
    FStream: TStream;
    FIsMenu: Boolean;
    FGopherFile: TIniFile;
    FQuery: TStringList;
    FParams: TStringList;
    FSearch: TStringList;
    FTemplate: TFPTemplate;
  public
    constructor Create(Sel, QStr, SStr, RAddr: string);
    destructor Destroy; override;
    class procedure Register(Pattern: string);
    procedure WriteInfo(S: string);
    procedure WriteItem(ItemName, ItemType: string);
    procedure WriteItem(ItemName, ItemType, ItemText: string);
    procedure WriteItem(ItemName, ItemType, ItemText, ItemPath: string);
    procedure WriteItem(ItemName, ItemType, ItemText, ItemPath,
      ItemHost: string);
    procedure WriteItem(ItemName, ItemType, ItemText, ItemPath,
      ItemHost: string; ItemPort: Word);
    procedure WriteError(Message: string);
    procedure Write(S: string);
    procedure Respond(Sender: TObject); virtual; abstract;
    procedure UpdateGopherFile;
    property RemoteAddress: string read FRemoteAddr;
    property Selector: string read FSelector;
    property ContentStream: TStream read FStream;
    property IsMenu: Boolean read FIsMenu write FIsMenu;
    property GopherFile: TIniFile read FGopherFile;
    property Query: TStringList read FQuery;
    property Params: TStringList read FParams;
    property Search: TStringList read FSearch;
    property Template: TFPTemplate read FTemplate;
  end;

  TGopherActionClass = class of TGopherAction;

  TGopherActionItem = class
  private
    FName: string;
    FClass: TGopherActionClass;
  public
    constructor Create(AName: string; AClassOf: TGopherActionClass);
    property Name: string read FName write FName;
    property ClassOf: TGopherActionClass read FClass write Fclass;
  end;

  TGopherActions = class
  private
    FActions: TFPObjectList;
    function GetAction(Name: string): TGopherActionClass;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function HasAction(Name: string): Boolean;
    procedure Register(Name: string; ClassOf: TGopherActionClass);
    procedure Unregister(Name: string);
    property Classes[Name: string]: TGopherActionClass
      read GetAction write Register; default;
  end;

var
  GopherActions: TGopherActions;

implementation

uses
  RegExpr;


constructor TGopherAction.Create(Sel, QStr, SStr, RAddr: string);
begin
  inherited Create;

  FSelector := Sel;
  FRemoteAddr := RAddr;
  FStream := TMemoryStream.Create;
  FGopherFile := TIniFile.Create(FStream);
  FTemplate := TFPTemplate.Create;

  FQuery := TStringList.Create;
  FQuery.Delimiter := gsQueryField;
  FQuery.StrictDelimiter := True;
  FQuery.DelimitedText := QStr;

  FParams := TStringList.Create;
  FParams.Delimiter := gsPath;
  FParams.StrictDelimiter := True;
  FParams.DelimitedText := Sel;

  if SStr = '' then
  begin
    SStr := QStr
  end;

  FSearch := TStringList.Create;
  FSearch.Delimiter := gsWord;
  FSearch.StrictDelimiter := True;
  FSearch.DelimitedText := SStr;

  IsMenu := True
end;

destructor TGopherAction.Destroy;
begin
  FGopherFile.Free;
  FStream.Free;
  FTemplate.Free;
  FQuery.Free;
  FParams.Free;
  FSearch.Free;
  inherited
end;

class procedure TGopherAction.Register(Pattern: string);
begin
  Pattern := ExcludeTrailingPathDelimiter(Pattern);
  Pattern := ExcludeLeadingPathDelimiter(Pattern);
  GopherActions['(?i)^' + Pattern + '$'] := self
end;

procedure TGopherAction.WriteInfo(S: string);
begin
  GopherFile.WriteString(IntToStr(FCount), 'text', S);
  Inc(FCount)
end;

procedure TGopherAction.WriteItem(ItemName, ItemType: string);
begin
  GopherFile.WriteString(ItemName, 'type', ItemType)
end;

procedure TGopherAction.WriteItem(ItemName, ItemType, ItemText: string);
begin
  WriteItem(ItemName, ItemType);
  GopherFile.WriteString(ItemName, 'text', ItemText)
end;

procedure TGopherAction.WriteItem(ItemName, ItemType, ItemText,
  ItemPath: string);
begin
  WriteItem(ItemName, ItemType, ItemText);
  GopherFile.WriteString(ItemName, 'path', ItemPath)
end;

procedure TGopherAction.WriteItem(ItemName, ItemType, ItemText,
  ItemPath, ItemHost: string);
begin
  WriteItem(ItemName, ItemType, ItemText, ItemPath);
  GopherFile.WriteString(ItemName, 'host', ItemHost)
end;

procedure TGopherAction.WriteItem(ItemName, ItemType, ItemText,
  ItemPath, ItemHost: string; ItemPort: Word);
begin
  WriteItem(ItemName, ItemType, ItemText, ItemPath, ItemHost);
  GopherFile.WriteInteger(ItemName, 'port', ItemPort)
end;

procedure TGopherAction.WriteError(Message: string);
begin
  GopherFile.WriteString(IntToStr(FCount), 'type', giError);
  GopherFile.WriteString(IntToStr(FCount), 'text', Message);
  Inc(FCount)
end;

procedure TGopherAction.Write(S: string);
begin
  FStream.Write(S[1], Length(S) * SizeOf(S[1]))
end;

procedure TGopherAction.UpdateGopherFile;
begin
  FGopherFile.Free;
  FGopherFile := TIniFile.Create(FStream)
end;

constructor TGopherActionItem.Create(AName: string; AClassOf: TGopherActionClass);
begin
  inherited Create;
  FName := AName;
  FClass := AClassOf
end;

function TGopherActions.GetAction(Name: string): TGopherActionClass;
var
  i: Integer;
  Item: TGopherActionItem;
begin
  Result := nil;
  for i := 0 to FActions.Count - 1 do
  begin
    try
      Item := FActions[i] as TGopherActionItem;
      if ExecRegExpr(Item.Name, Name) then
      begin
        Result := Item.ClassOf;
        Break
      end
    except
      Break
    end
  end
end;

procedure TGopherActions.AfterConstruction;
begin
  inherited;
  FActions := TFPObjectList.Create
end;

procedure TGopherActions.BeforeDestruction;
begin
  inherited;
  FActions.Free
end;

function TGopherActions.HasAction(Name: string): Boolean;
begin
  Result := GetAction(Name) <> nil
end;

procedure TGopherActions.Register(Name: string; ClassOf: TGopherActionClass);
begin
  FActions.Add(TGopherActionItem.Create(Name, ClassOf))
end;

procedure TGopherActions.Unregister(Name: string);
var
  i: Integer;
begin
  for i := 0 to FActions.Count - 1 do
    if (FActions[i] as TGopherActionItem).Name = '(?i)^' + Name + '$' then
    begin
      FActions.Delete(i);
      break
    end
end;

initialization
  GopherActions := TGopherActions.Create

finalization
  GopherActions.Free

end.
