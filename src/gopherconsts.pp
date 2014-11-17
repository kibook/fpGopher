unit GopherConsts;

{$mode objfpc}{$H+}

interface

const
  gsField = #9;
  gsPath = '/';
  gsDir = DirectorySeparator;
  gsLine = LineEnding;
  gsMenuItem = #13#10;
  gsQuery = '?';
  gsQueryField = '&';
  gsSearch = #9;
  gsWord = ' ';
  gsTerminate = '.';

  giPlainText      = '0';
  giDirectory = '1';
  giCSO       = '2';
  giError     = '3';
  giBinHex    = '4';
  giArchive   = '5';
  giUUE       = '6';
  giSearch    = '7';
  giTelnet    = '8';
  giBinary    = '9';
  giGIF       = 'g';
  giHTML      = 'h';
  giInfo      = 'i';
  giImage     = 'I';
  giAudio     = 's';
  giTn3270    = 'T';

  GopherItemTypes = [giPlainText, giDirectory, giCSO, giError, giBinHex,
    giArchive, giUUE, giSearch, giTelnet, giBinary, giGIF, giHTML, giInfo,
    giImage, giAudio, giTn3270];

  DotGopher = '.gopher';

  GopherPort = 70;

  GopherURI = 'gopher://';

implementation

end.
