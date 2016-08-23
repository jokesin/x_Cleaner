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

with Ada.Unchecked_Conversion,
     Ada.Containers;
---
with GWindows.GStrings,
     GWindows.Menus,
     GWindows.Types;

---
with Win32,
     Win32.Windef,
     Win32.Wingdi,
     Win32.Winuser;


use Win32,
    Win32.Windef,
    Win32.Wingdi,
    Win32.Winuser;
---
with System;
---
with Callbacks;
---
with Drive_Devices;


package body Main_Window is

   package IC renames Interfaces.C;

   package body List_View is
      use Callbacks;
      -- Popup --

      package body Popup_Menu is
         use GWindows.Menus;

         Clear_Menu : Menu_Type;
         Clear_Sub_Menu : Menu_Type;

         procedure Do_Context_Menu(Window : in out GWindows.Base.Base_Window_Type'Class;
                                   X      : in     Integer;
                                   Y      : in     Integer)
         is
            use GWindows.Types;
            Item,SubItem : Integer := 0;
            Cursor_Pos : aliased POINT := POINT'(LONG(X),LONG(Y));
         begin
            if ScreenToClient(HWND(X_Main_Window_Type(Window).Volume_List.Handle),Cursor_Pos'Unchecked_Access) /=
              Win32.TRUE then
               null;
            end if;
            X_Main_Window_Type(Window).Volume_List.Item_At_Position(Point_Type'(Integer(Cursor_Pos.X),
                                                                    Integer(Cursor_Pos.Y)),Item,SubItem);
            Drive_Devices.Get_Drives.Set_Selected_Index(Item);
            Display_Context_Menu(X_Main_Window_Type(Window),Clear_Menu,0,X,Y);
         end Do_Context_Menu;

      begin
         Clear_Menu := Create_Popup;
         Clear_Sub_Menu := Create_Popup;

         Append_Item(Clear_Sub_Menu,"B&ritish HMG-IS5 (Base)           [1 pass]",IDM_HMG_IS5);
         Append_Item(Clear_Sub_Menu,"B&ritish HMG-IS5 (Extended)   [3 passes]",IDM_HMG_IS5_EXT);
         Append_Item(Clear_Sub_Menu,"R&ussian GOST-R-50739-95     [2 passes]",IDM_GOST_R50739_95);
         Append_Item(Clear_Sub_Menu,"B&ruce Schneier's Algorithm   [7 passes]",IDM_SCHNEIER);

         Append_Menu(Clear_Menu,"&Clear drive",Clear_Sub_Menu);
      end Popup_Menu;

      -- ListView --

      type LVITEM is
         record
            Mask      : Interfaces.C.unsigned := 0;
            Item      : Integer := 0;
            SubItem   : Integer := 0;
            State     : Interfaces.C.unsigned := 0;
            StateMask : Interfaces.C.unsigned := 0;
            Text      : GWindows.Common_Controls.LPTSTR := null;
            TextMax   : Integer := 0;
            Image     : Integer;
            lParam    : GWindows.Types.Lparam := 0;
            Indent    : Integer;
            iGroupId  : Integer;
            cColumns  : Interfaces.C.unsigned := 0;
            PuColumns : GWindows.Common_Controls.LPTSTR := null;
         end record;




      type NMHDR is
         record
            HWND_From : Win32.Windef.HWND;
            ID_From   : Win32.UINT_PTR;
            Code      : Win32.UINT;
         end record
        with Convention=>C_PASS_BY_COPY;
      type LPNMHDR is access all NMHDR;

      type NMCUSTOMDRAW is
         record
            hdr         : NMHDR;
            dwDrawStage : DWORD;
            hdc         : Win32.Windef.HDC;
            rc          : aliased RECT;
            dwItemSpec  : DWORD_PTR;
            uItemState  : UINT;
            lItemlParam : LPARAM;
         end record
        with Convention=>C_PASS_BY_COPY;
      type LPNMCUSTOMDRAW is access all NMCUSTOMDRAW;

      type NMLVCUSTOMDRAW is
         record
            nmcd : NMCUSTOMDRAW;
            clrText : COLORREF;
            clrTextBk : COLORREF;
            --#if (_WIN32_IE >= 0x0400)
            iSubItem : int;
            --#endif
            --#if (_WIN32_IE >= 0x0560)
            dwItemType : DWORD;
            clrFace : COLORREF;
            iIconEffect : int;
            iIconPhase : int;
            iPartId : int;
            iStateId : int;
            rcText : RECT;
            uAlign : UINT;
            --#endif
         end record
        with Convention=>C_PASS_BY_COPY;
      type LPNMLVCUSTOMDRAW is access all NMLVCUSTOMDRAW;

      NM_FIRST               : constant := 0;
      NM_CUSTOMDRAW          : constant := NM_FIRST-12;

      CDDS_PREPAINT          : constant := 16#0001#;
      CDDS_ITEM              : constant := 16#00010000#;
      CDDS_SUBITEM           : constant := 16#00020000#;
      CDRF_DODEFAULT         : constant := 16#0#;
      CDRF_SKIPDEFAULT       : constant := 16#00000004#;
      CDRF_NOTIFYITEMDRAW    : constant := 16#00000020#;
      CDRF_NOTIFYSUBITEMDRAW : constant := 16#00000020#;

      CLR_NONE               : constant := 16#FFFFFFFF#;
      CLR_DEFAULT            : constant := 16#ff000000#;

      DT_END_ELLIPSIS        : constant := 16#00008000#;

      function To_LPNMHDR is new Ada.Unchecked_Conversion(GWindows.Base.Pointer_To_Notification,LPNMHDR);
      function To_LPNMLVCUSTOMDRAW is new Ada.Unchecked_Conversion(GWindows.Base.Pointer_To_Notification,LPNMLVCUSTOMDRAW);

      function Handle_Custom_Draw(List_View : in out X_List_View_Type;
                                  PCD       : LPNMLVCUSTOMDRAW)
                               return GWindows.Types.Lresult;
      -- On_Notify --


      overriding procedure On_Notify
        (List_View    : in out X_List_View_Type;
         Message      : in     GWindows.Base.Pointer_To_Notification;
         Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
         Return_Value : in out GWindows.Types.Lresult)
      is
      begin
         case Message.Code is
         when NM_CUSTOMDRAW =>
            Return_Value := List_View.Handle_Custom_Draw(To_LPNMLVCUSTOMDRAW(Message));
         when others =>
            null;
         end case;
      end On_Notify;


      -- Handle_Custom_Draw --

      function Handle_Custom_Draw(List_View : in out X_List_View_Type;
                                  PCD       : LPNMLVCUSTOMDRAW)
                               return GWindows.Types.Lresult
      is
         use type GWindows.Types.Lresult,
             IC.unsigned_long;

      begin
         case PCD.nmcd.dwDrawStage is
         when CDDS_PREPAINT =>
            -- Tell the control we are interested in per-item notifications.
            -- (We need it just to tell the control we want per-subitem
            -- notifications.)
            return (CDRF_DODEFAULT or CDRF_NOTIFYITEMDRAW);
         when (CDDS_ITEM or CDDS_PREPAINT) =>
            -- Tell the control we are interested in per-subitem notifications.
            return (CDRF_DODEFAULT or CDRF_NOTIFYSUBITEMDRAW);
         when (CDDS_ITEM or CDDS_SUBITEM or CDDS_PREPAINT) =>
            case PCD.iSubItem is
               when 0 =>
                  --                    PCD.clrTextBk := 16#00000000#;
                  --                    PCD.clrText := 16#00FFFFFF#;
                  return CDRF_DODEFAULT;
               when 4 =>
                  -- customize "progress" column.
                  declare
                     use System;
                     use type IC.int,IC.long,IC.unsigned;
                     i_Progress : INT := int(PCD.nmcd.lItemlParam);
                     CX : INT;
                     HDC : Win32.Windef.HDC := PCD.nmcd.hdc;
                     Clr_Back : COLORREF := PCD.clrTextBk;
                     H_Back_Brush,
                     H_Progress_Brush,
                     H_Old_Brush : HBRUSH;
                     H_Pen,
                     H_Old_Pen : HPEN;
                     RC : aliased RECT;

                     Progress_Text : IC.char_array := IC.To_C(i_Progress'Img & " %");
                     function To_LPCSTR is new Ada.Unchecked_Conversion(Address,LPCSTR);
                  begin
                     if Clr_Back = CLR_NONE or else
                       Clr_Back = CLR_DEFAULT then
                        Clr_Back := 16#00FFFFFF#;--white
                     end if;
                     H_Back_Brush := CreateSolidBrush(Clr_Back);
                     H_Progress_Brush := CreateSolidBrush(16#00FFA000#);
                     H_Pen := CreatePen(PS_SOLID,0,16#00FF00FF#);

                     H_Old_Brush := SelectObject(hdc,H_Back_Brush);
                     if FillRect(HDC,PCD.nmcd.rc'Access,H_Back_Brush) = 0 then
                        null; -- MessageBox - error
                     end if;

                     CX := INT(PCD.nmcd.rc.right - PCD.nmcd.rc.left - 6);
                     if CX < 0 then
                        CX := 0;
                     end if;

                     RC.left := PCD.nmcd.rc.left + 3;
                     RC.top := PCD.nmcd.rc.top + 2;
                     RC.right := LONG(INT(RC.left) + CX * i_Progress / 100);
                     RC.bottom := PCD.nmcd.rc.bottom - 2;
                     if SelectObject(HDC,H_Progress_Brush) = Null_Address then
                        null;
                     end if;

                     if FillRect(HDC,RC'Unchecked_Access,H_Progress_Brush) = 0 then
                        null; -- MessageBox - error
                     end if;

                     RC.right := PCD.nmcd.rc.right - 3;
                     if SelectObject(HDC,GetStockObject(HOLLOW_BRUSH)) = Null_Address then
                        null;
                     end if;
                     H_Old_Pen := SelectObject(HDC,H_Pen);

                     if Rectangle(HDC,INT(RC.left),INT(RC.top),INT(RC.right),INT(RC.bottom)) /= Win32.TRUE then
                        null;
                     end if;

                     if DrawText(HDC,To_LPCSTR(Progress_Text'Address),-1,RC'Unchecked_Access,
                                 DT_CENTER or DT_VCENTER or DT_NOPREFIX or DT_SINGLELINE or DT_END_ELLIPSIS) = 0 then
                        null;
                     end if;

                     if SelectObject(HDC,H_Old_Brush) = Null_Address or else -- or?
                       DeleteObject(H_Progress_Brush) /= Win32.TRUE or else
                       DeleteObject(H_Back_Brush) /= Win32.TRUE or else
                       SelectObject(HDC,H_Old_Pen) = Null_Address or else
                       DeleteObject(H_Pen) /= Win32.TRUE then
                        null; -- Messagebox error
                     end if;

                     return CDRF_SKIPDEFAULT;
                  end;

               when others =>
                  return CDRF_DODEFAULT;
            end case;

         when others =>
            return CDRF_DODEFAULT;
         end case;

      end Handle_Custom_Draw;

      -- Insert_Item --

      procedure Insert_Item (Control      : in out X_List_View_Type;
                             Mask         : in  Interfaces.C.unsigned;
                             Text         : in  GWindows.GString;
                             LParam       : in  GWindows.Types.Lparam;
                             Index        : in  Integer;
                             Sorted_Index : out Integer;
                             Icon         : in  Integer := 0)
      is
         use GWindows;
         use type IC.unsigned;
         C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);

         Item : LVITEM;

         function SendMessageA
           (hwnd   : GWindows.Types.Handle := Handle (Control);
            uMsg   : Interfaces.C.int      := LVM_INSERTITEMA;
            wParam : GWindows.Types.Wparam := 0;
            lParam : LVITEM                := Item)
         return GWindows.Types.Lparam;
         pragma Import (StdCall, SendMessageA,
                        "SendMessage" & Character_Mode_Identifier);

         function SendMessageW
           (hwnd   : GWindows.Types.Handle := Handle (Control);
            uMsg   : Interfaces.C.int      := LVM_INSERTITEMW;
            wParam : GWindows.Types.Wparam := 0;
            lParam : LVITEM                := Item)
         return GWindows.Types.Lparam;
         pragma Import (StdCall, SendMessageW,
                        "SendMessage" & Character_Mode_Identifier);
      begin
         Item.Mask := Mask;
         Item.Item := Index;
         Item.Image := Icon;
         Item.Text := C_Text (0)'Unchecked_Access;
         Item.lParam := LParam;

         case Character_Mode is
         when Unicode =>
            Sorted_Index := Integer (SendMessageW);
         when ANSI =>
            Sorted_Index := Integer (SendMessageA);
         end case;
      end Insert_Item;

      -- Insert_Item --

      procedure Insert_Item (Control : in out X_List_View_Type;
                             Mask    : in Interfaces.C.unsigned;
                             Text    : in GWindows.GString;
                             LParam  : in  GWindows.Types.Lparam;
                             Index   : in Integer;
                             Icon    : in Integer := 0)
      is
         Sorted_Index : Integer; -- will be ignored
      begin
         Control.Insert_Item(Mask         => Mask,
                             Text         => Text,
                             LParam       => LParam,
                             Index        => Index,
                             Sorted_Index => Sorted_Index,
                             Icon         => Icon);
      end Insert_Item;

      -- Set_Item --

      procedure Set_Item (Control : in out X_List_View_Type;
                          Mask    : in  Interfaces.C.unsigned;
                          Text    : in  GWindows.GString;
                          LParam  : in  GWindows.Types.Lparam;
                          Index   : in Integer;
                          Icon    : in Integer := 0)
      is
         use GWindows;
         C_Text : GString_C := GWindows.GStrings.To_GString_C (Text);

         Item : LVITEM;

         procedure SendMessageA
           (hwnd   : GWindows.Types.Handle := Handle (Control);
            uMsg   : Interfaces.C.int      := LVM_SETITEMA;
            wParam : GWindows.Types.Wparam := 0;
            lParam : LVITEM            := Item);
         pragma Import (StdCall, SendMessageA,
                        "SendMessage" & Character_Mode_Identifier);

         procedure SendMessageW
           (hwnd   : GWindows.Types.Handle := Handle (Control);
            uMsg   : Interfaces.C.int      := LVM_SETITEMW;
            wParam : GWindows.Types.Wparam := 0;
            lParam : LVITEM                := Item);
         pragma Import (StdCall, SendMessageW,
                        "SendMessage" & Character_Mode_Identifier);
      begin
         Item.Mask := Mask;
         Item.Item := Index;
         Item.Image := Icon;
         Item.Text := C_Text (0)'Unchecked_Access;
         Item.lParam := LParam;

         case Character_Mode is
         when Unicode =>
            SendMessageW;
         when ANSI =>
            SendMessageA;
         end case;
      end Set_Item;

      -- Add_Row --

      procedure Add_Row(Control        : in out X_List_View_Type;
                        Letter         : Character;
                        Size           : String;
                        Algorithm      : String := "";
                        Operation      : String := "";
                        Clean_Progress : Integer := 0)
      is
         use type IC.unsigned;
      begin
         Control.Insert_Item(LVIF_TEXT or LVIF_PARAM,String'(1=>Letter),GWindows.Types.Lparam(Clean_Progress),0);
         Control.Set_Sub_Item(Size,0,1);
         Control.Set_Sub_Item(Algorithm,0,2);
         Control.Set_Sub_Item(Operation,0,3);
      end Add_Row;


      -- Add_Drives --

      procedure Add_Drives (Control : in out X_List_View_Type)
      is
         use Drive_Devices.Drive_Device,
             Drive_Devices.Drives_Container;
         use type Ada.Containers.Count_Type;
         Sys_Drives : Drive_Devices.Drives := Drive_Devices.Get_Drives;
         L : Natural;
      begin
         if Sys_Drives.Get_Vector.Length > 0 then
            L := Sys_Drives.Get_Vector.Last_Index;
            for K in reverse 0..L loop
               Control.Add_Row(Letter         => Sys_Drives.Get_Vector.Element(K).Get_Letter,
                               Size           =>
                                 ULONGLONG'Image(Sys_Drives.Get_Vector.Element(K).Get_Size / (1024 * 1024)));
            end loop;
         end if;

      end Add_Drives;

      -- Set_Operation_Name --
      procedure Set_Algorithm_Name(Control        : in out X_List_View_Type;
                                   Item           : Natural;
                                   Algorithm_Name : String)
      is
      begin
         Control.Set_Sub_Item(Algorithm_Name,Item,2);
      end Set_Algorithm_Name;

      -- Set_Operation_Name --
      procedure Set_Operation_Name(Control        : in out X_List_View_Type;
                                   Item           : Natural;
                                   Operation_Name : String)
      is
      begin
         Control.Set_Sub_Item(Operation_Name,Item,3);
      end Set_Operation_Name;

      -- Set_Progress_Clean_Value --

      procedure Set_Progress_Clean_Value(Control        : in out X_List_View_Type;
                                         Item           : Natural;
                                         Progress_Value : Natural)
      is
      begin
         Control.Set_Item(Mask   => LVIF_PARAM,
                          Text   => "",
                          LParam => GWindows.Types.Lparam(Progress_Value),
                          Index  => Item);
      end Set_Progress_Clean_Value;

   end List_View;

   ---------------------------------------

   X_Main : aliased X_Main_Window_Type;

   -- Get_X_Main --

   function Get_X_Main return X_Main_Window
   is
   begin
      return X_Main'Access;
   end Get_X_Main;

   -- Get_Main_Menu --

   function Get_Main_Menu(Main_Window : access X_Main_Window_Type) return GWindows.Menus.Menu_Type
   is
   begin
      return Main_Window.Main_Menu;
   end Get_Main_Menu;

   -- Get_Volume_List --
   function Get_Volume_List(Main_Window : access X_Main_Window_Type) return List_View.X_List_View
   is
   begin
      return Main_Window.Volume_List'Access;
   end Get_Volume_List;

end Main_Window;
