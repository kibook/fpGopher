program fpmake;

uses
  fpmkunit;

begin
  with Installer.AddPackage('fpGopher') do
  begin
    OSes := [win32,openbsd,netbsd,freebsd,darwin,linux];
    FPDocFormat := [ffHtml];
    with Targets do
    begin
      AddUnit('src/gopherconsts.pp');
      AddUnit('src/gopheraction.pp');
      AddUnit('src/gopherlog.pp');
      AddUnit('src/gopherserver.pp');
      AddUnit('src/gophermoleaction.pp');
      AddUnit('src/gophercaps.pp');
      AddUnit('src/gopherclient.pp');      
      AddUnit('src/gophersearch.pp');
      AddFPDoc('src/gopherconsts.pp', 'docsrc/gopherconsts.xml');
      AddFPDoc('src/gopheraction.pp', 'docsrc/gopheraction.xml');
      AddFPDoc('src/gopherlog.pp', 'docsrc/gopherlog.xml');
      AddFPDoc('src/gopherserver.pp', 'docsrc/gopherserver.xml');
      AddFPDoc('src/gophermoleaction.pp', 'docsrc/gophermoleaction.xml');
      AddFPDoc('src/gophercaps.pp', 'docsrc/gophercaps.xml');
      AddFPDoc('src/gopherclient.pp', 'docsrc/gopherclient.xml');      
      AddFPDoc('src/gophersearch.pp', 'docsrc/gophersearch.xml')
    end
  end;
  Installer.Run
end.
