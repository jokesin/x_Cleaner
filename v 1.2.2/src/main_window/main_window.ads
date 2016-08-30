--------------------------------------------------------------------------------------
--        x_Cleaner erases all data from selected volume with special algorithm
--
--        Copyright (C) 2016  George Ivanov
--
--        This program is free software: you can redistribute it and/or modify
--        it under the terms of the GNU General Public License as published by
--        the Free Software Foundation, either version 3 of the License, or
--        (at your option) any later version."
--
--        This program is distributed in the hope that it will be useful,
--        but WITHOUT ANY WARRANTY; without even the implied warranty of
--        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--        GNU General Public License for more details.
--
--        You should have received a copy of the GNU General Public License
--        along with this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------------

with GWindows,
     GWindows.Base,
     GWindows.Types,
     GWindows.Windows.Main,
     GWindows.Common_Controls,
     GWindows.Menus;
---
with Interfaces.C;
---
package Main_Window is

   package List_View is

      -- Popup --
      package Popup_Menu is
         procedure Do_Context_Menu(Window : in out GWindows.Base.Base_Window_Type'Class;
                                   X      : in     Integer;
                                   Y      : in     Integer);
      end Popup_Menu;

      -- ListView

      use type Interfaces.C.unsigned;

      LVIF_TEXT               : constant := 16#0001#;
      LVIF_IMAGE              : constant := 16#0002#;
      LVIF_PARAM              : constant := 16#0004#;

      LVS_EX_FULLROWSELECT    : constant := 16#00000020#;

      LVM_FIRST                   : constant := 16#1000#;
      LVM_SETITEMA                : constant := LVM_FIRST + 6;
      LVM_SETITEMW                : constant := LVM_FIRST + 76;

      LVM_INSERTITEMA             : constant := LVM_FIRST + 7;
      LVM_INSERTITEMW             : constant := LVM_FIRST + 77;

      LVM_SETEXTENDEDLISVIEWSTYLE : constant := LVM_FIRST + 54;

      type X_List_View_Type is new GWindows.Common_Controls.List_View_Control_Type with private;
      type X_List_View is access all X_List_View_Type;

      procedure Insert_Item (Control      : in out X_List_View_Type;
                             Mask         : in  Interfaces.C.unsigned := LVIF_TEXT or LVIF_IMAGE;
                             Text         : in  GWindows.GString;
                             LParam       : in  GWindows.Types.Lparam := 0;
                             Index        : in  Integer;
                             Sorted_Index : out Integer;
                             Icon         : in  Integer := 0);

      procedure Insert_Item (Control : in out X_List_View_Type;
                             Mask    : in  Interfaces.C.unsigned := LVIF_TEXT or LVIF_IMAGE;
                             Text    : in  GWindows.GString;
                             LParam  : in  GWindows.Types.Lparam := 0;
                             Index   : in  Integer;
                             Icon    : in  Integer := 0);

      procedure Set_Item (Control : in out X_List_View_Type;
                          Mask    : in  Interfaces.C.unsigned := LVIF_TEXT or LVIF_IMAGE;
                          Text    : in  GWindows.GString;
                          LParam  : in  GWindows.Types.Lparam := 0;
                          Index   : in Integer;
                          Icon    : in Integer := 0);

      procedure Add_Row(Control        : in out X_List_View_Type;
                        Letter         : Character;
                        Size           : String;
                        Algorithm      : String := "";
                        Operation      : String := "";
                        Clean_Progress : Integer := 0);

      procedure Add_Drives(Control : in out X_List_View_Type);

      procedure Set_Algorithm_Name(Control        : in out X_List_View_Type;
                                   Item           : Natural;
                                   Algorithm_Name : String);
      procedure Set_Operation_Name(Control        : in out X_List_View_Type;
                                   Item           : Natural;
                                   Operation_Name : String);
      procedure Set_Progress_Clean_Value(Control        : in out X_List_View_Type;
                                         Item           : Natural;
                                         Progress_Value : Natural);
      overriding
      procedure On_Notify
        (List_View    : in out X_List_View_Type;
         Message      : in     GWindows.Base.Pointer_To_Notification;
         Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
         Return_Value : in out GWindows.Types.Lresult);
      --  Handles Notify Messages

   private
      type X_List_View_Type is new GWindows.Common_Controls.List_View_Control_Type with null record;

   end List_View;


   -- Main Window --
   type X_Main_Window_Type is new GWindows.Windows.Main.Main_Window_Type with private;
   type X_Main_Window is access all X_Main_Window_Type;
   function Get_X_Main return X_Main_Window;
   function Get_Main_Menu(Main_Window : access X_Main_Window_Type) return GWindows.Menus.Menu_Type;
   function Get_Volume_List(Main_Window : access X_Main_Window_Type) return List_View.X_List_View;

   procedure Set_Dialog_Center_Pos(Main_Window : access X_Main_Window_Type; Dialog : GWindows.Windows.Window_Type);
private
   type X_Main_Window_Type is new GWindows.Windows.Main.Main_Window_Type with
      record
         Main_Menu   : GWindows.Menus.Menu_Type;
         Volume_List : aliased List_View.X_List_View_Type;
      end record;

end Main_Window;
