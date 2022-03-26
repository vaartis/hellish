with Ada.Text_Io; use Ada.Text_Io;

package body Markdown is
   procedure Html_Processor(Input : Chars_Ptr; Size : Unsigned; Userdata : in out Data) is
      Str : String := Value(Input);
   begin
      -- Use the size, because not the whole string is actually what we want, there's trailing garbage.
      Userdata := Userdata & Str(1..Integer(Size));
   end Html_Processor;

   function To_Html(Input : String; Parse_Flags : Parser_Flag := 0) return String is
      Input_C_Str : Chars_Ptr := New_String(Input);
      Userdata : Unbounded_String;
      Result : Int := To_Html(Input_C_Str, Strlen(Input_C_Str),
                              Html_Processor'Access, Data(Userdata),
                              Parse_Flags,
                              0);
   begin
      Free(Input_C_Str);
      return To_String(Userdata);
   end;
end Markdown;
