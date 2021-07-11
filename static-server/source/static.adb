pragma Ada_2012;
with GNAT.Strings; use GNAT.Strings;
with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with AWS.Config;
with AWS.Config.Set;
with AWS.Server.Log;

with AWS.Services.Page_Server;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Definitions;
with Version;

package body Static is

  use Ada;
  use Ada.Text_IO;
  use Ada.Exceptions;

  use AWS.Config.Set;

   --------------------------------
   -- Read_Commandline_Arguments --
   --------------------------------
   function Read_Commandline_Arguments return Options is

    Config: Command_Line_Configuration;

    Selected_Options: Options;
    Port: aliased integer := 8080;
   begin
    Define_Switch(Config, Selected_Options.Show_Version'Access, "-v", Help => "Display version and exit",
    Long_Switch => "--version");

    Define_Switch(Config, Selected_Options.Show_Help'Access, "-h",
    Help => "Display help and exit",
    Long_Switch => "--help");

    Define_Switch(Config, Selected_Options.Open_Browser_Window'Access, "-o",
    Help => "Open browser window after starting the server",
    Long_Switch => "--open");

    Define_Switch(Config, Selected_Options.Log'Access,"-l",
    Help => "Log activity",
    Long_Switch => "--log");

    Define_Switch(Config, Selected_Options.Error_Log'Access,"-e",
    Help => "Log errors",
    Long_Switch => "--error");

    Define_Switch(Config, Selected_Options.Secure'Access, "-s",
    Help => "Enable https",
    Long_Switch => "--secure");

    Define_Switch(Config, Selected_Options.Path'Access, "-P:",
    Help => "Path to directory to be served (default is current directory)",
    Long_Switch => "--path:",
    Argument => "PATH");

    Define_Switch(Config, Selected_Options.Log_File_Directory'Access, "-ldir:",
    Help => "Path to log directory (default is current directory)",
    Argument => "PATH");

    Define_Switch(Config, Selected_Options.Log_Filename_Prefix'Access, "-lname:",
    Help => "Name of log file, without file extension (default is 'static')",
    Argument => "PATH");

    Define_Switch(Config, Selected_Options.Error_Filename_Prefix'Access, "-ename:",
    Help => "Name of error log file, without file extension (default is 'error')",
    Argument => "PATH");

    Define_Switch(Config, Port'Access, "-p:",
    Help => "Port to use (defaults to 8080)",
    Long_Switch => "--port:",
    Argument => "PORTNUMBER",
    Default => 8080,
    Initial => 8080);

    Set_Usage(Config, "[switches]", "Static_Server is a easy to use http server");

    Getopt(Config);

    Selected_Options.Start_Server := not (Selected_Options.Show_Help or Selected_Options.Show_Version);

    if Port in Natural then
      Selected_Options.Port := Port;
    else
      Ada.Text_IO.Put_Line("Invalid port number");
      Selected_Options.Start_Server := false;
    end if;

    return Selected_Options;

    exception
      when E: Invalid_Switch =>
        -- GNAT.Command_Line already writes this to the cli. No need to do that.
        Selected_Options.Start_Server := false;
        return Selected_Options;

      when E: Invalid_Parameter =>
        Ada.Text_IO.Put_Line("Invalid parameter for switch -" & Full_Switch);
        Selected_Options.Start_Server := false;
        return Selected_Options;

      when E: Exit_From_Command_Line =>
        -- Handle silly exception raised by GNAT.Command_Line when displaying help.
        Selected_Options.Start_Server := false;
        return Selected_Options;

   end Read_Commandline_Arguments;

   -------------------------
   -- Open_Browser_Window --
   -------------------------
   procedure Open_Browser_Window (Selected_Options : Options) is

      Arg1 : constant String := "http://localhost:";
      Arg2 : constant String := Ada.Strings.Fixed.Trim(Natural'image(Selected_Options.Port), Ada.Strings.Left);

      Args : constant Argument_List := (1=> new String'(Arg1&Arg2));
      Success: Boolean;
   begin
      Spawn(Definitions.Open_File_Program_Name, Args, Success);
   end Open_Browser_Window;

   ------------------
   -- Start_Server --
   ------------------
   procedure Start_Server (Selected_Options : Options) is

    WS: AWS.Server.HTTP;
    Config: AWS.Config.Object := AWS.Config.Get_Current;
   begin
    Server_Port(Config, Selected_Options.Port);

    if Selected_Options.Path.all = "" then
      WWW_Root (Config, Definitions.Current_Path);
    else
      WWW_Root(Config, Selected_Options.Path.all);
    end if;

    if Selected_Options.Log_File_Directory.all = "" then
      Log_File_Directory(Config, Log_File_Directory_Default);
    else
      Log_File_Directory(Config, Selected_Options.Log_File_Directory.all);
    end if;

    if Selected_Options.Log_Filename_Prefix.all = "" then
      Log_Filename_Prefix(Config, Log_Filename_Prefix_Default);
    else
      Log_Filename_Prefix(Config, Selected_Options.Log_Filename_Prefix.all);
    end if;

    if Selected_Options.Error_Filename_Prefix.all = "" then
      Error_Log_Filename_Prefix(Config, Error_Filename_Prefix_Default);
    else
      Error_Log_Filename_Prefix(Config, Selected_Options.Error_Filename_Prefix.all);
    end if;

    Log_Activated(Config, Selected_Options.Log);
    Error_Log_Activated(Config, Selected_Options.Error_Log);
    Security(Config, Selected_Options.Secure);

    Text_IO.Put_Line("Serving on port" & Natural'Image(AWS.Config.Server_Port(Config)) & " from location " & AWS.Config.WWW_Root(Config));

    Text_IO.Put_Line("Press Q to stop serving.");

    AWS.Server.Start(WS, AWS.Services.Page_Server.Callback'Access, Config);

    if Selected_Options.Open_Browser_Window then
      Open_Browser_Window(Selected_Options);
    end if;
    AWS.Server.Wait(AWS.Server.Q_Key_Pressed);

    Text_IO.Put("Server shutting down...");
    AWS.Server.Shutdown(WS);
    Text_IO.Put_Line ("done");

   end Start_Server;

   -------------
   -- Process --
   -------------
   procedure Process (Selected_Options : in out Options) is

   begin

    if Selected_Options.Show_Version then
      Text_IO.Put_Line("Static-Server version " & Version.Serve_Version);
    end if;

    if Selected_Options.Start_Server then
       Start_Server(Selected_Options);
    end if;

   end Process;

end Static;