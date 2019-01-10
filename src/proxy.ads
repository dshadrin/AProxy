with Pal; use Pal;
with Ada.Finalization;
with ConfigTree;

package Proxy is
   pragma Elaborate_Body;

   type Configurator is limited private;
   type ConfiguratorPtr is access Configurator;
   
   function GetChild(ptr : in ConfiguratorPtr; path : in String) return ConfigTree.NodePtr;
   function GetValue(ptr : in ConfiguratorPtr; path : in String; default : in String := "") return String;

   type Manager is tagged limited private;
   type ManagerPtr is access all Manager;

   function GetManager return ManagerPtr;
   function GetConfig return ConfiguratorPtr;
   procedure DeleteManager;
   procedure Start (Object : in out Manager);
   
private
   mgrPtr : ManagerPtr;
   
   ---------------------------------
   -- Configurator declaration    --
   ---------------------------------
   type Configurator is new Ada.Finalization.Limited_Controlled with
      record
         data : ConfigTree.Tree;
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
