with GNAT.Strings; use GNAT.Strings;

package Static is

  Log_Filename_Prefix_Default : constant String := "static";
  Error_Filename_Prefix_Default : constant String := "error";
  Log_File_Directory_Default : constant String := "logs";

  type Options is private;

  function Read_Commandline_Arguments return Options;

  procedure Process(Selected_Options: in out Options);


private

  type Options is record
    Show_Help : aliased boolean;
    Show_Version : aliased boolean;
    Start_Server : boolean;

    Port: natural;

    Path: aliased String_Access;
    Log_File_Directory: aliased String_Access;
    Error_Filename_Prefix: aliased String_Access;
    Log_Filename_Prefix: aliased String_Access;

    Log: aliased boolean;
    Error_Log: aliased boolean;

    Secure: aliased boolean;


    Open_Browser_Window : aliased boolean;

  end record;

end Static;