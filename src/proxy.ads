with Pal; use Pal;
with Ada.Finalization;
with ConfigTree;

package Proxy is
   pragma Elaborate_Body;

   type Configurator is tagged limited private;
   type ConfiguratorPtr is access Configurator;

   type Manager is tagged limited private;
   type ManagerPtr is access all Manager;

   function GetManager return ManagerPtr;
   function GetConfig return ConfiguratorPtr;
   procedure DeleteManager;
   
private
   mgrPtr : ManagerPtr;
   
   ---------------------------------
   -- Configurator declaration    --
   ---------------------------------
   type Configurator is new Ada.Finalization.Limited_Controlled with
      record
         dom : ConfigTree.Tree;
      end record;
      
   procedure Initialize (Object : in out Configurator);
   procedure Finalize (Object : in out Configurator);
   
   ---------------------------------
   -- Manager declaration         --
   ---------------------------------
   type Manager is new Ada.Finalization.Limited_Controlled with
      record
         config : ConfiguratorPtr;
      end record;
      
   procedure Initialize (Object : in out Manager);
   procedure Finalize (Object : in out Manager);

end Proxy;
