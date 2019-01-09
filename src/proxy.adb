with Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with ConfigTree; use ConfigTree;

package body Proxy is

   procedure Free is new Unchecked_Deallocation(Configurator, ConfiguratorPtr);
   procedure Free is new Unchecked_Deallocation(Manager, ManagerPtr);

   ---------------------------------
   -- Configurator implementation --
   ---------------------------------
   procedure Initialize (Object : in out Configurator) is
   begin
      Put_Line("Init Configurator");
   end Initialize;
   
   procedure Finalize (Object : in out Configurator) is
   begin
      Put_Line("Stop Configurator");
   end Finalize;
   
   ---------------------------------
   -- Manager implementation      --
   ---------------------------------
   procedure Initialize (Object : in out Manager) is
   begin
      Put_Line("Init Manager");
      Object.config := new Configurator;
   end Initialize;
   
   procedure Finalize (Object : in out Manager) is
   begin
      Free(Object.config); 
      Put_Line("Stop Manager");
   end Finalize;
   
   procedure Start (Object : in out Manager) is
      actors : NodePtr := GetConfig.data.GetChild ("proxy.actors");
   begin
      if actors /= 0 then
         actors := null;
      end if;
      
   end Start;

   ---------------------------------
   -- Package free functions      --
   ---------------------------------
   function GetManager return ManagerPtr is
   begin
      return mgrPtr;
   end GetManager;

   function GetConfig return ConfiguratorPtr is
   begin
      return mgrPtr.config;
   end GetConfig;
   
   procedure DeleteManager is
   begin
      Free(mgrPtr);
   end DeleteManager;

begin
   mgrPtr := new Manager;

end Proxy;
