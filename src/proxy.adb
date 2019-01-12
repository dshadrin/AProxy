----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with ConfigTree; use ConfigTree;

------------------------------------------------------------------------------------------------------------------------
package body Proxy is

   procedure Free is new Ada.Unchecked_Deallocation(Configurator, ConfiguratorPtr);
   procedure Free is new Ada.Unchecked_Deallocation(Manager, ManagerPtr);

   ---------------------------------
   -- Configurator implementation --
   ---------------------------------
   procedure Initialize (Object : in out Configurator) is
   begin
      Put_Line("Init Configurator");
   end Initialize;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure Finalize (Object : in out Configurator) is
   begin
      Put_Line("Stop Configurator");
   end Finalize;
   
   ---------------------------------------------------------------------------------------------------------------------
   function GetChild (ptr : in ConfiguratorPtr; path : in String) return NodePtr is
   begin
      return ptr.data.GetChild(path);
   end GetChild;
   
   ---------------------------------------------------------------------------------------------------------------------
   function GetValue (ptr : in ConfiguratorPtr; path : in String; default : in String := "") return String is
   begin
      return ptr.data.GetValue(path, default);
   end GetValue;
         
   ---------------------------------
   -- Manager implementation      --
   ---------------------------------
   procedure Initialize (Object : in out Manager) is
   begin
      Put_Line("Init Manager");
      Object.config := new Configurator;
   end Initialize;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure Finalize (Object : in out Manager) is
   begin
      Free(Object.config); 
      Put_Line("Stop Manager");
   end Finalize;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure Start (Object : in out Manager) is
      actors : NodePtr := GetConfig.data.GetChild ("proxy.actors");
   begin
      if not IsNull(actors) then
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

   ---------------------------------------------------------------------------------------------------------------------
   function GetConfig return ConfiguratorPtr is
   begin
      return mgrPtr.config;
   end GetConfig;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure DeleteManager is
   begin
      Free(mgrPtr);
   end DeleteManager;

begin
   ---------------------------------------------------------------------------------------------------------------------
   mgrPtr := new Manager;

end Proxy;
