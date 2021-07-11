with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Command_Line;
with GNAT.Command_Line; use GNAT.Command_Line;

with Static;

procedure Start is

  use Ada;
  use Ada.Exceptions;

  Selected_Options : Static.Options := Static.Read_Commandline_Arguments;

begin
  Static.Process(Selected_Options);

  Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Success);

exception

 when E: others =>

  Ada.Text_IO.Put_Line(Exception_Message(E));
  Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end Start;