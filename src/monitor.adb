with Ada.Text_IO; use Ada.Text_IO;
with Pal; use Pal;
with Proxy;
with TimeStamp; use TimeStamp;
with Interfaces.C;
with Logging_Message; use Logging_Message;

procedure Monitor is
   ptr : Proxy.ManagerPtr := Proxy.GetManager;
begin
   ptr.Start;
   LOG_INFO("LOG ", "Start Manager");

   loop
      declare
         str : String := Get_Line;
      begin
         exit when str = "quit" or else str = "exit";
      end;
   end loop;

   LOG_INFO("LOG ", "Stop Manager");
   Proxy.DeleteManager;
   ptr := null;
end Monitor;
