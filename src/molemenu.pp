unit MoleMenu;

interface

procedure Info(Message: string);
procedure Item(ItemType, ItemText, ItemPath: string);
procedure Item(ItemType, ItemText, ItemPath, ItemHost: string);
procedure Item(ItemType, ItemText, ItemPath, ItemHost: string;
  ItemPort: Word);
procedure Error(Message: string);

implementation

uses
  SysUtils, GopherConsts, GopherServer;

procedure Info(Message: string);
begin
  Write(giInfo,
        Message, gsField,
        'fake', gsField,
        '(NULL)', gsField,
        '0', gsMenuItem)
end;

procedure Item(ItemType, ItemText, ItemPath: string);
begin
  Write(GetGopherType(ItemType),
        ItemText, gsField,
        ItemPath, gsField,
        GetEnvironmentVariable('REMOTE_HOST'), gsField,
        GetEnvironmentVariable('REMOTE_PORT'), gsMenuItem)
end;

procedure Item(ItemType, ItemText, ItemPath, ItemHost: string);
begin
  Write(GetGopherType(ItemType),
        ItemText, gsField,
        ItemPath, gsField,
        ItemHost, gsField,
        GetEnvironmentVariable('REMOTE_PORT'), gsMenuItem)
end;

procedure Item(ItemType, ItemText, ItemPath, ItemHost: string;
  ItemPort: Word);
begin
  Write(GetGopherType(ItemType),
        ItemText, gsField,
        ItemPath, gsField,
        ItemHost, gsField,
        ItemPort, gsMenuItem)
end;

procedure Error(Message: string);
begin
  Write(giError,
        Message, gsField,
        'fake', gsField,
        '(NULL)', gsField,
        '0', gsMenuItem)
end;

finalization
  Write(gsTerminate)

end.
