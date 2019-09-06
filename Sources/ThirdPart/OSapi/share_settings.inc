// Einstellungen, diese Datei muss in alle Dateien eingebunden sein

// DelphiUnicode Modus für FPC
// http://wiki.freepascal.org/FPC_New_Features_3.0#New_delphiunicode_syntax_mode
{$IFDEF FPC}{$MODE DelphiUnicode}{$ENDIF}

// Windows Multimedia Subsystem nur unter Delphi sowie FPC unter Windows
{$IFDEF FPC}
  // Freepascal Compiler, Achtung, WINDOWS Switch nur auf FPC vorhanden
  //{$IFDEF WINDOWS}
  //  {$DEFINE ACM}
  //{$ENDIF}
{$ELSE}
  // Delphi Compiler
  {$DEFINE ACM}
{$ENDIF}



