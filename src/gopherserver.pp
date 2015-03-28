unit GopherServer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SSockets,
  EventLog,
  IniFiles,
  GopherConsts,
  GopherAction,
  GopherLog;

const
  SErrNotFound = 'Not Found';
  SErrServer   = 'Server Error';
  SLogNotFound = '%s not found: %s (%s)';
  SLogBadExec  = '%s bad executable: %s (%S) [%d]';
  SLogBadMole  = '%s bad mole script: %s (%s) [%d]';

  GopherServerVersion = 'fpGopher 0.1';

type
  TGopherServer = class(TInetServer)
  private
    FRootDir: string;
    FItemTypes: TStringList;
    FAllowMole: Boolean;
    FMoleExtensions: TStringList;
    FMoleHandlers: TStringList;
    FAdminEmail: string;
    FVersion: string;
    FGopherFile: string;
    FLog: TGopherLog;
    function Field(TypeOf, Name, Path, AHost: string;
      APort: Word): string;
    function Field(TypeOf, Name, Path: string): string;
    function Error(Message: string): string;
    procedure ParseGopher(Config: TIniFile; Selector: string;
      Menu: TStringList);
    procedure Respond(Sender: TObject; Data: TSocketStream);
  public
    constructor Create(const AHost: string); overload;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property RootDir: string read FRootDir write FRootDir;
    property ItemTypes: TStringList read FItemTypes;
    property AllowMole: Boolean read FAllowMole write FAllowMole;
    property MoleExtensions: TStringList read FMoleExtensions;
    property MoleHandlers: TStringList read FMoleHandlers;
    property AdminEmail: string read FAdminEmail write FAdminEmail;
    property Version: string read FVersion write FVersion;
    property GopherFileName: string read FGopherFile write FGopherFile;
    property Log: TGopherLog read FLog write FLog;
  end;

function GetGopherType(Name: string): string;

implementation

uses
  Sockets,
  Process,
  fpMimeTypes;

function TGopherServer.Field(TypeOf, Name, Path, AHost: string;
  APort: Word): string;
begin
  if (TypeOf = giInfo) or (TypeOf = giError) then
  begin
    Result := TypeOf   +
              Name     + gsField +
              'fake'   + gsField +
              '(NULL)' + gsField +
              '0'
  end else
  begin
    Result := TypeOf  +
              Name    + gsField +
              Path    + gsField +
              AHost   + gsField +
              IntToStr(APort)
  end
end;

function TGopherServer.Field(TypeOf, Name, Path: string): string;
begin
  Result := Field(TypeOf, Name, Path, Host, Port)
end;

function TGopherServer.Error(Message: string): string;
begin
  Result := Field(giError, Message, '')
end;

procedure TGopherServer.ParseGopher(Config: TIniFile; Selector: string;
  Menu: TStringList);
var
  Items: TStringList;
  Item: string;
  ItemPath: string;
  ItemType: string;
  ItemText: string;
  ItemHost: string;
  ItemPort: Word;
begin
  Items := TStringList.Create;
  Menu.TextLineBreakStyle := tlbsCRLF;
  Selector := IncludeLeadingPathDelimiter(Selector + gsPath);
  Config.ReadSections(Items);
  for Item in Items do
  begin
    ItemPath := Config.ReadString(Item, 'path', Selector + Item);

    ItemType := Config.ReadString(Item, 'type', giInfo);
    ItemType := GetGopherType(ItemType);

    ItemText := Config.ReadString(Item, 'text', Item);
    ItemHost := Config.ReadString(Item, 'host', Host);

    if ItemHost = Host then
    begin
      ItemPort := Config.ReadInteger(Item, 'port', Port)
    end else
    begin
      ItemPort := Config.ReadInteger(Item, 'port', GopherPort)
    end;

    Menu.Add(Field(
      ItemType,
      ItemText,
      ItemPath,
      ItemHost,
      ItemPort
    ))
  end;
  Menu.Add(gsTerminate);
  Items.Free
end;

procedure TGopherServer.Respond(Sender: TObject; Data: TSocketStream);
var
  Received: TStringList;
  Selector: string;  
  Path: string;
  Info: TSearchRec;
  Menu: TStringList;
  ItemType: string;
  Mime: string;
  Config: TIniFile;
  QueryPos: Integer;
  QueryString: string;
  Exec: TProcess;
  FileStream: TFileStream;
  RemoteAddr: string;
  Ext: string;
  ActionString: string;
  Action: TGopherAction;
  ExitStatus: Integer;
  SearchString: string;
  SearchPos: Integer;
begin
  Received := TStringList.Create;
  Menu := TStringList.Create;

  Received.LoadFromStream(Data);
  RemoteAddr := NetAddrToStr(Data.RemoteAddress.sin_addr);

  Selector := Trim(Received.Text);
  Selector := ExcludeTrailingPathDelimiter(Selector);
  Selector := ExcludeLeadingPathDelimiter(Selector);

  QueryPos := Pos(gsQuery, Selector);
  if QueryPos <> 0 then
  begin
    QueryString := Copy(Selector, QueryPos + 1, Length(Selector));
    Selector := Copy(Selector, 1, QueryPos - 1)
  end;

  SearchPos := Pos(gsSearch, Selector);
  if SearchPos <> 0 then
  begin
    SearchString := Copy(Selector, SearchPos + 1, Length(Selector));
    Selector := Copy(Selector, 1, SearchPos - 1)
  end;

  Path := FRootDir + gsDir + Selector;

  { gopher://host/#/action/$1/$2.../$n }
  ActionString := ExcludeLeadingPathDelimiter(Selector);
  if Pos(gsPath, ActionString) <> 0 then
  begin
    ActionString := Copy(ActionString, 1, Pos(gsPath, ActionString) - 1)
  end;

  FLog.Info(RemoteAddr + ' ' + Selector);

  if GopherActions.HasAction(ActionString) then
  begin
    Action := GopherActions[ActionString].Create(
      Selector,
      QueryString,
      SearchString,
      RemoteAddr
    );
    with Action do
    begin
      Respond(self);
      ContentStream.Position := 0;
      if IsMenu then
      begin
        ParseGopher(GopherFile, Selector, Menu)
      end else
      begin
        Data.CopyFrom(ContentStream, ContentStream.Size)
      end
    end;
    Action.Free
  end
  else if FileExists(Path + gsDir + FGopherFile) then
  begin
    Config := TIniFile.Create(Path + gsDir + FGopherFile);
    ParseGopher(Config, Selector, Menu);
    Config.Free
  end
  else if DirectoryExists(Path) then
  begin
    Menu.TextLineBreakStyle := tlbsCRLF;
    if FindFirst(Path + '/*', faAnyFile, Info) = 0 then
    begin
      repeat
        if (Info.Name <> '.') and (Info.Name <> '..') then
        begin
          {$if FPC_FULLVERSION < 20701}
          Mime := ''; { bug in GetMimeType in 2.6.4 }
          {$endif}
          Mime := MimeTypes.GetMimeType(ExtractFileExt(Info.Name));
          if DirectoryExists(Path + gsDir + Info.Name) then
          begin
            ItemType := giDirectory
          end else
          begin
            if FItemTypes.Values[Mime] <> '' then
            begin
              ItemType := FItemTypes.Values[Mime]
            end else
            begin
              ItemType := giPlainText
            end
          end;
          Menu.Add(Field(
            ItemType,
            Info.Name,
            IncludeLeadingPathDelimiter(Selector + gsPath + Info.Name)
          ))
        end
      until FindNext(Info) <> 0
    end;
    FindClose(Info);
    Menu.Add(gsTerminate)
  end
  else if FileExists(Path) then
  begin
    Ext := ExtractFileExt(Path);
    Exec := TProcess.Create(nil);
    Exec.Options := [poUsePipes, poWaitOnExit, poStdErrToOutput];

    with Exec.Environment do
    begin
      Values['DOCUMENT_ROOT'] := FRootDir;
      Values['QUERY_STRING'] := QueryString;
      Values['REMOTE_ADDR'] := RemoteAddr;
      Values['REMOTE_HOST'] := Host;
      Values['REMOTE_PORT'] := IntToStr(Port);
      Values['SCRIPT_FILENAME'] := ExpandFileName(Path);
      Values['SCRIPT_NAME'] := Path;
      Values['PATH'] := GetCurrentDir;
      Values['SERVER_PORT'] := IntToStr(Port);        
      Values['SERVER_ADMIN'] := FAdminEmail;
      Values['SERVER_NAME'] := Host;
      Values['SERVER_SOFTWARE'] := FVersion
    end;

    if FAllowMole and (FMoleHandlers.Values[Ext] <> '') then
    begin
      Exec.Executable := FMoleHandlers.Values[Ext];
      Exec.Parameters.Add(Path);

      try
        Exec.Execute;
        ExitStatus := Exec.ExitStatus
      except
        ExitStatus := 1
      end;

      if Exec.ExitStatus = 0 then
      begin
        Data.CopyFrom(Exec.Output, Exec.Output.NumBytesAvailable)
      end else
      begin
        Menu.Add(Error(SErrServer));
        FLog.Error(SLogBadExec, [RemoteAddr, Selector, Path, ExitStatus])
      end
    end
    else if FAllowMole and (FMoleExtensions.IndexOf(Ext) <> -1) then
    begin
      Exec.Executable := Path;

      try
        Exec.Execute;
        ExitStatus := Exec.ExitStatus
      except
        ExitStatus := 1
      end;

      if ExitStatus = 0 then
      begin
        Data.CopyFrom(Exec.Output, Exec.Output.NumBytesAvailable)
      end else
      begin
        Menu.Add(Error(SErrServer));
        FLog.Error(SLogBadMole, [RemoteAddr, Selector, Path, ExitStatus])
      end
    end else
    begin
      try
        try
          FileStream := TFileStream.Create(Path, fmOpenRead);
          Data.CopyFrom(FileStream, FileStream.Size);
        finally
          FileStream.Free
        end
      except
        Menu.Add(Error(SErrServer));
        FLog.Error(SErrServer, [])
      end
    end;
    Exec.Free
  end else
  begin
    Menu.Add(Error(SErrNotFound));
    FLog.Error(SLogNotFound, [RemoteAddr, Selector, Path])
  end;

  Menu.SaveToStream(Data);

  Received.Free;
  Menu.Free;
  Data.Free
end;

constructor TGopherServer.Create(const AHost: string);
begin
  Create(AHost, GopherPort)
end;

procedure TGopherServer.AfterConstruction;
begin
  inherited;
  OnConnect := @Respond;

  FItemTypes := TStringList.Create;
  FItemTypes.Sorted := True;
  FItemTypes.Duplicates := dupIgnore;
  FItemTypes.Values['text/plain'] := giPlainText;
  FItemTypes.Values['image/gif'] := giGIF;
  FItemTypes.Values['text/html'] := giHTML;
  FItemTypes.Values['image/jpeg'] := giImage;
  FItemTypes.Values['image/png'] := giImage;
  FItemTypes.Values['audio/x-wav'] := giAudio;
  FItemTypes.Values['audio/mpeg'] := giAudio;
  FItemTypes.Values['audio/ogg'] := giAudio;
  FItemTypes.Values['application/pdf'] := giBinary;

  FMoleExtensions := TStringList.Create;
  FMoleExtensions.Sorted := True;
  FMoleExtensions.Duplicates := dupIgnore;
  FMoleExtensions.Add('.cgi');

  FMoleHandlers := TStringList.Create;
  FMoleHandlers.Sorted := True;
  FMoleHandlers.Duplicates := dupIgnore;

  FVersion := GopherServerVersion;
  FGopherFile := DotGopher;

  FLog := TGopherLog.Create
end;

procedure TGopherServer.BeforeDestruction;
begin
  inherited;
  FItemTypes.Free;
  FMoleExtensions.Free;
  FMoleHandlers.Free;
  FLog.Free
end;

function GetGopherType(Name: string): string;
begin
  case LowerCase(Name) of
    'plaintext' : Result := giPlainText;
    'directory' : Result := giDirectory;
    'cso'       : Result := giCSO;
    'error'     : Result := giError;
    'binhex'    : Result := giBinHex;
    'archive'   : Result := giArchive;
    'uue'       : Result := giUUE;
    'search'    : Result := giSearch;
    'telnet'    : Result := giTelnet;
    'binary'    : Result := giBinary;
    'gif'       : Result := giGIF;
    'html'      : Result := giHTML;
    'info'      : Result := giInfo;
    'image'     : Result := giImage;
    'audio'     : Result := giAudio;
    'tn3270'    : Result := giTn3270
  otherwise
    Result := Name
  end
end;

initialization
  MimeTypes.AddType('text/plain', 'txt;text;conf;def;list;log;in');
  MimeTypes.AddType('image/gif', 'gif');
  MimeTypes.AddType('text/html', 'html;html');
  MimeTypes.AddType('image/jpeg', 'jpeg;jpg;jpe');
  MimeTypes.AddType('image/png', 'png');
  MimeTypes.AddType('audio/x-wav', 'wav');
  MimeTypes.AddType('audio/mpeg', 'mpga;mp2a;mp2;mp3;m2a;m3a');
  MimeTypes.AddType('audio/ogg', 'oga;ogg;spx');
  MimeTypes.AddType('application/pdf', '.pdf')

end.
