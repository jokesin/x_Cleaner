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

with GWindows.Application,
     GWindows.Buttons,
     GWindows.Constants,
     GWindows.Drawing_Objects,
     GWindows.Windows,
     GWindows.List_Boxes;
---
with Win32,
     Win32.Windef,
     Win32.Winuser;

use Win32,
    Win32.Windef,
    Win32.Winuser;
---
with Main_Window;
---
with Drive_Devices;
---
with Vista_Dynamic;
---
with Interfaces.C;
package body Callbacks is

   package IC renames Interfaces.C;

   procedure Show_About_Dialog;
   -- Menu_Select_Cb --

   --TODO
   --check if cleaning is runnung

   procedure Menu_Select_Cb(Window : in out GWindows.Base.Base_Window_Type'Class;
                            Item   : in     Integer)
   is
      use Main_Window,
          Drive_Devices,
          Drive_Devices.Drive_Device;

      Sys_Drives : Drive_Devices.Drives := Get_Drives;
      Selected_Drive : Integer;

   begin
      case Item is
         when IDM_Exit =>
            GWindows.Application.End_Application;

         when IDM_About=>
            Show_About_Dialog;

         when IDM_HMG_IS5 =>
            Selected_Drive := Sys_Drives.Get_Selected_Index;
            if Selected_Drive >= 0 then
               Sys_Drives.Get_Vector.Element(Selected_Drive).Set_Cleaning_State(True);
               Sys_Drives.Get_Vector.Element(Selected_Drive).Clear(Selected_Drive,
                                                                   Chosen_Algorithm => HMG_IS5);
            end if;

         when IDM_HMG_IS5_ENH =>
            Selected_Drive := Sys_Drives.Get_Selected_Index;
            if Selected_Drive >= 0 then
               Sys_Drives.Get_Vector.Element(Selected_Drive).Clear(Selected_Drive,
                                                                   Chosen_Algorithm => HMG_IS5_ENH);
            end if;

         when IDM_GOST_R50739_95 =>
            Selected_Drive := Sys_Drives.Get_Selected_Index;
            if Selected_Drive >= 0 then
               Sys_Drives.Get_Vector.Element(Selected_Drive).Clear(Selected_Drive,
                                                                   Chosen_Algorithm => GOST_R50739_95);
            end if;

         when IDM_DoD5220_22_M_E =>
            Selected_Drive := Sys_Drives.Get_Selected_Index;
            if Selected_Drive >= 0 then
               Sys_Drives.Get_Vector.Element(Selected_Drive).Clear(Selected_Drive,
                                                                   Chosen_Algorithm => DoD5220_22_M_E);
            end if;

         when IDM_SCHNEIER =>
            Selected_Drive := Sys_Drives.Get_Selected_Index;
            if Selected_Drive >= 0 then
               Sys_Drives.Get_Vector.Element(Selected_Drive).Clear(Selected_Drive,
                                                                   Chosen_Algorithm => SCHNEIER);
            end if;
         when others =>
            null;
      end case;



   end Menu_Select_Cb;

   -- Main_Close_Cb --
   procedure Main_Close_Cb(Window    : in out GWindows.Base.Base_Window_Type'Class;
                           Can_Close : out Boolean)
   is
   begin
      if Vista_Dynamic.Is_Os_Vista_Based then
         Vista_Dynamic.Unload_Libs;
      end if;
      Can_Close := True;
   end Main_Close_Cb;

   -- Show_About_Dialog --
   procedure Show_About_Dialog
   is
      use GWindows.Application,
          GWindows.Buttons,
          GWindows.Constants,
          GWindows.Drawing_Objects,
          GWindows.Windows,
          GWindows.List_Boxes;

      Dialog    : Window_Type;
      List_Box  : List_Box_Type;
      OK_Button : Default_Button_Type;
      Font      : Font_Type;
   begin
      Create_As_Dialog(Dialog,"About x_Cleaner & license",Width => 400,Height => 280);
      Font.Create_Stock_Font(Default_GUI);
      List_Box.Create(Dialog,10,10,380,230,Sort => False,Is_Dynamic => True);
      List_Box.Set_Font(Font);
      List_Box.Add("x_Cleaner clears selected volume with special algorithm");
      List_Box.Add("");
      List_Box.Add("Copyright (C) 2016  George Ivanov");
      List_Box.Add("");
      List_Box.Add("This program is free software: you can redistribute it and/or modify");
      List_Box.Add("it under the terms of the GNU General Public License as published by");
      List_Box.Add("the Free Software Foundation, either version 3 of the License, or");
      List_Box.Add("(at your option) any later version.");
      List_Box.Add("");
      List_Box.Add("This program is distributed in the hope that it will be useful,");
      List_Box.Add("but WITHOUT ANY WARRANTY; without even the implied warranty of");
      List_Box.Add("MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
      List_Box.Add("GNU General Public License for more details.");
      List_Box.Add("");
      List_Box.Add("You should have received a copy of the GNU General Public License");
      List_Box.Add("along with this program.  If not, see <http://www.gnu.org/licenses/>.");
      OK_Button.Create(Dialog,"O&k",250,225,140,25,ID=>GWindows.Constants.IDOK);
      OK_Button.Set_Font(Font);

      -- Set center position --
      Main_Window.Get_X_Main.Set_Dialog_Center_Pos(Dialog);
      -- Show
      Show_Dialog(Dialog);
   end Show_About_Dialog;

end Callbacks;
