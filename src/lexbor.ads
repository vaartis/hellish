with Ada.Finalization; use Ada.Finalization;
with System; use System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Lexbor is
   type Html_Document is private;

   type Lxb_Status is
     (
      Lxb_Status_Ok,
      Lxb_Status_Error,
      Lxb_Status_Error_Memory_Allocation,
      Lxb_Status_Error_Object_Is_Null,
      Lxb_Status_Error_Small_Buffer,
      Lxb_Status_Error_Incomplete_Object,
      Lxb_Status_Error_No_Free_Slot,
      Lxb_Status_Error_Too_Small_Size,
      Lxb_Status_Error_Not_Exists,
      Lxb_Status_Error_Wrong_Args,
      Lxb_Status_Error_Wrong_Stage,
      Lxb_Status_Error_Unexpected_Result,
      Lxb_Status_Error_Unexpected_Data,
      Lxb_Status_Error_Overflow,
      Lxb_Status_Continue,
      Lxb_Status_Small_Buffer,
      Lxb_Status_Aborted,
      Lxb_Status_Stopped,
      Lxb_Status_Next,
      Lxb_Status_Stop
     )
   with Convention => C;

   procedure Parse(Doc : Html_Document; Html : String);
   function Title(Doc : Html_Document) return String;

   Lexbor_Error : exception;
private
   type Html_Document_Ptr is new Address;

   type Html_Document is new Controlled with record
      Ptr : Html_Document_Ptr;
   end record;
   overriding procedure Initialize(Doc : in out Html_Document);
   overriding procedure Finalize(Doc : in out Html_Document);

   function Html_Document_Create return Html_Document_Ptr
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_html_document_create";

   function Html_Document_Parse(Doc : Html_Document_Ptr; Html : Chars_Ptr; Html_Len : Size_T) return Lxb_Status
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_html_document_parse";

   procedure Html_Document_Destroy(Doc : Html_Document_Ptr)
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_html_document_destroy";

   function Html_Document_Title(Doc : Html_Document_Ptr; Title_Len : out Size_T) return Chars_ptr
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_html_document_title";
end Lexbor;
