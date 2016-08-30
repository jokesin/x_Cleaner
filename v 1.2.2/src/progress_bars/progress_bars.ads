with Win32,
     Win32.Windef;
---
with GWindows.Base;
---
with Vista_Dynamic;
package Progress_Bars is

   type Draw_Progress_Bar is access procedure
     (Window    : GWindows.Base.Base_Window_Type;
      HDC       : Win32.Windef.HDC;
      RC        : Win32.Windef.LPRECT;
      iProgress : Win32.INT);

   procedure Draw(Window    : GWindows.Base.Base_Window_Type;
                  HDC       : Win32.Windef.HDC;
                  RC        : Win32.Windef.LPRECT;
                  iProgress : Win32.INT);

end Progress_Bars;
