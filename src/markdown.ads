with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with
  Interfaces.C,
  Interfaces.C.Strings;
use
  Interfaces.C,
  Interfaces.C.Strings;

package Markdown is
   type Parser_Flag is mod 2 ** 16;

   Md_Flag_Collapse_Whitespace        : Parser_Flag := 16#0001#;
   Md_Flag_Permissive_Atx_Headers     : Parser_Flag := 16#0002#;
   Md_Flag_Permissive_Url_Autolinks   : Parser_Flag := 16#0004#;
   Md_Flag_Permissive_Email_Autolinks : Parser_Flag := 16#0008#;
   Md_Flag_No_Indented_Codeblocks     : Parser_Flag := 16#0010#;
   Md_Flag_No_Html_Blocks             : Parser_Flag := 16#0020#;
   Md_Flag_No_Html_Spans              : Parser_Flag := 16#0040#;
   Md_Flag_Tables                     : Parser_Flag := 16#0100#;
   Md_Flag_Strikethrough              : Parser_Flag := 16#0200#;
   Md_Flag_Permissive_Www_Autolinks   : Parser_Flag := 16#0400#;
   Md_Flag_Tasklists                  : Parser_Flag := 16#0800#;
   Md_Flag_Latex_Math_Spans           : Parser_Flag := 16#1000#;
   Md_Flag_Wiki_Links                 : Parser_Flag := 16#2000#;
   Md_Flag_Underline                  : Parser_Flag := 16#4000#;  
   
   function To_Html(Input : String; Parse_Flags : Parser_Flag := 0) return String;
private
   type Data is new Unbounded_String;
   
   type Html_Process_Function is access procedure(Input : Chars_Ptr; Size : Unsigned; Userdata : in out Data) with Convention => C;
   
   function To_Html(Input : Chars_Ptr;
                    Input_Size : Size_T;
                    Process_Function : Html_Process_Function;
                    Userdata : in out Data;
                    Parser_Flags : Parser_Flag;
                    Renderer_Flags : Unsigned) return Int
   with
     Import => True,
     Convention => C,
     External_Name => "md_html";   
   
   procedure Html_Processor(Input : Chars_Ptr; Size : Unsigned; Userdata : in out Data) with Convention => C;
end Markdown;
