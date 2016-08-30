with Ada.Unchecked_Conversion;
---
with Win32.Objbase,
     Win32.Winbase,
     Win32.Winerror,
     Win32.Wingdi,
     Win32.Winuser;

use Win32.Objbase,
    Win32.Winbase,
    Win32.Winerror,
    Win32.Wingdi,
    Win32.Winuser;
---
with GWindows.Message_Boxes;
---
with System;
---
with Interfaces.C;
---
with Main_Window;
---
with System.Machine_Code;
use System.Machine_Code;

package body Progress_Bars is

   use Win32,
       Win32.Windef,
       Vista_Dynamic;

   package IC renames Interfaces.C;

   DT_END_ELLIPSIS : constant := 16#00008000#;


   Draw_Proc : Draw_Progress_Bar;

   -- Draw --

   procedure Draw
     (Window    : GWindows.Base.Base_Window_Type;
      HDC       : Win32.Windef.HDC;
      RC        : Win32.Windef.LPRECT;
      iProgress : Win32.INT)
   is
   begin
      Draw_Progress_Bar(Draw_Proc)(Window,HDC,RC,iProgress);
   end Draw;

   -- Draw_Progress_Bar_Vista --

   procedure Draw_Progress_Bar_Vista(Window    : GWindows.Base.Base_Window_Type;
                                     HDC       : Win32.Windef.HDC;
                                     RC        : Win32.Windef.LPRECT;
                                     iProgress : Win32.INT)
   is
      use GWindows.Base,
          GWindows.Message_Boxes,
          System;

      use type IC.int,IC.long,Win32.Winerror.HRESULT;
      Progress_Class : IC.wchar_array := IC.TO_C(Wide_String'("Progress"),Append_Nul=>True);
      Theme : HTHEME := Open_Theme_Data(HWND(Window.Handle),To_PCWSTR(Progress_Class'Address));
      RC_Context,
      RC_Chunk      : aliased RECT;
      Rgn           : HRGN;
      Progress_Text : IC.char_array := IC.To_C(iProgress'Img & " %");
      function To_LPCSTR is new Ada.Unchecked_Conversion(Address,LPCSTR);
   begin
      if Get_Theme_Background_Content_Rect(Theme,HDC,PP_BAR,0,Win32.Objbase.LPCRECT(RC),RC_Context'Unchecked_Access) =
        Win32.Winerror.S_OK then
         Rgn := CreateRoundRectRgn(INT(RC.left),INT(RC.top),INT(RC.right+1),INT(RC.bottom+1),4,4);
         if SelectClipRgn(HDC,Rgn) /= ERROR and then
           Draw_Theme_Background(Theme,HDC,PP_BAR,0,RC,null) = Win32.Winerror.S_OK and then
           SelectClipRgn(HDC,Null_Address) /= ERROR and then
           DeleteObject(Rgn) /= Win32.FALSE then
            -- Draw the smooth progress-bar percentage
            RC_Chunk := RC_Context;
            -- Set progress value
            RC_Chunk.right :=
              LONG(RC_Context.left + LONG(Float((RC_Context.right-RC_Context.left) * LONG(iProgress)) / 100.0));
            if Draw_Theme_Background(Theme,HDC,PP_CHUNK,0,RC_Chunk'Unchecked_Access,null) /= Win32.Winerror.S_OK or else
              DrawText(HDC,To_LPCSTR(Progress_Text'Address),-1,RC_Context'Unchecked_Access,
                       DT_CENTER or DT_VCENTER or DT_NOPREFIX or DT_SINGLELINE or DT_END_ELLIPSIS) = 0 then
               Message_Box(Window,
                           "Error!","Error drawing progressbar!",
                           Icon     => Error_Icon);
            end if;
         else
            Message_Box(Window,
                        "Error!","Error while preparing drawing progressbar!",
                        Icon     => Error_Icon);
         end if;
      else
         Message_Box(Window,
                     "Error!","Error : GetThemeBackgroundContentRect!",
                     Icon     => Error_Icon);
      end if;
   end Draw_Progress_Bar_Vista;


   -- Draw_Progress_Bar_XP --

   procedure Draw_Progress_Bar_XP(Window    : GWindows.Base.Base_Window_Type;
                                  HDC       : Win32.Windef.HDC;
                                  RC        : Win32.Windef.LPRECT;
                                  iProgress : Win32.INT)
   is
      use System;
      use type IC.int,IC.long;
      RC_Context    : aliased RECT;
      CX            : Win32.INT;
      Clr_Back      : COLORREF := 16#00CCAACC#;--16#00FFFFFF#; --white
      H_Back_Brush,
      H_Progress_Brush,
      H_Old_Brush   : HBRUSH;
      H_Pen,
      H_Old_Pen     : HPEN;
      Progress_Text : IC.char_array := IC.To_C(iProgress'Img & " %");
      function To_LPCSTR is new Ada.Unchecked_Conversion(Address,LPCSTR);
   begin
      H_Back_Brush := CreateSolidBrush(Clr_Back);
      H_Progress_Brush := CreateSolidBrush(16#00FFA000#);
      H_Pen := CreatePen(PS_SOLID,0,16#00FF00FF#);

      H_Old_Brush := SelectObject(HDC,H_Back_Brush);
      if FillRect(HDC,ac_RECT_t(RC),H_Back_Brush) /= 0 then
         CX := INT(RC.right - RC.left - 6);
         if CX < 0 then
            CX := 0;
         end if;

         RC_Context.left := RC.left + 3;
         RC_Context.top := RC.top + 2;
         RC_Context.right := LONG(INT(RC_Context.left) + CX * iProgress / 100);
         RC_Context.bottom := RC.bottom - 2;

         if SelectObject(HDC,H_Progress_Brush) /= Null_Address and then
           FillRect(HDC,RC_Context'Unchecked_Access,H_Progress_Brush) /= 0 then
            RC_Context.right := RC.right - 3;
            if SelectObject(HDC,GetStockObject(HOLLOW_BRUSH)) /= Null_Address then
               H_Old_Pen := SelectObject(HDC,H_Pen);
               if Rectangle(HDC,INT(RC.left),INT(RC.top),INT(RC.right),INT(RC.bottom)) /= Win32.FALSE then
                  if DrawText(HDC,To_LPCSTR(Progress_Text'Address),-1,RC_Context'Unchecked_Access,
                              DT_CENTER or DT_VCENTER or DT_NOPREFIX or DT_SINGLELINE or DT_END_ELLIPSIS) /= 0 then
                     if SelectObject(HDC,H_Old_Brush) = Null_Address or else -- or?
                       DeleteObject(H_Progress_Brush) /= Win32.TRUE or else
                       DeleteObject(H_Back_Brush) /= Win32.TRUE or else
                       SelectObject(HDC,H_Old_Pen) = Null_Address or else
                       DeleteObject(H_Pen) /= Win32.TRUE then
                        null; -- Messagebox error
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Draw_Progress_Bar_XP;
   ---------------------------------------
   --- ELABORATION ---

--  DwMajorVersion:DWORD
--  Windows 95 - 4
--  Windows 98 - 4
--  Windows Me - 4
--  Windows NT 3.51 - 3
--  Windows NT 4.0 - 4
--  Windows 2000 - 5
--  Windows XP - 5
--  Windows Vista 6
--  Windows Seven 7

begin
   if Vista_Dynamic.Is_Os_Vista_Based then
      Draw_Proc := Draw_Progress_Bar_Vista'Access;
   else -- XP or lower
      Draw_Proc := Draw_Progress_Bar_XP'Access;
   end if;
end Progress_Bars;
