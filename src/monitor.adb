with Ada.Text_IO; use Ada.Text_IO;
with Pal; use Pal;
with Proxy;

procedure Monitor is
   ptr : Proxy.ManagerPtr := Proxy.GetManager;
begin
   ptr.Start;

   loop
      declare
         str : String := Get_Line;
      begin
         exit when str = "quit" or else str = "exit";
      end;
   end loop;

   Proxy.DeleteManager;
   ptr := null;
end Monitor;
