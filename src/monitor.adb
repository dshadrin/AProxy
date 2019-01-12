----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Logging_Message; use Logging_Message;
with Ada.Text_IO; use Ada.Text_IO;
with Proxy;

------------------------------------------------------------------------------------------------------------------------
procedure Monitor is
   ptr : Proxy.ManagerPtr := Proxy.GetManager;
begin
   ptr.Start;
   LOG_INFO ("LOG ", "Start Manager");

   loop
      declare
         str : String := Get_Line;
      begin
         exit when str = "quit" or else str = "exit";
      end;
   end loop;

   LOG_INFO ("LOG ", "Stop Manager");
   Proxy.DeleteManager;
   ptr := null;
end Monitor;
