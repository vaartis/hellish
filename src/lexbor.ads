with Ada.Finalization; use Ada.Finalization;
with System; use System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Lexbor is
   type Html_Document is private;
   type Dom_Element is tagged private;
   type Dom_Collection is tagged private;

   No_Dom_Element : constant Dom_Element;

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
   function Document_Head(Doc : Html_Document) return Dom_Element'Class;
   function Get_Attribute(Elem : Dom_Element'Class; Attr : String) return String;

   function Make_Dom_Collection(Doc : Html_Document; Size : Natural) return Dom_Collection'Class;
   function Length(Col : Dom_Collection'Class) return Natural;
   function Element(Col : Dom_Collection'Class; Elem : Natural) return Dom_Element'Class;
   procedure Clean(Col : Dom_Collection'Class);

   procedure Dom_Elements_By_Attr(Root : Dom_Element; Collection : Dom_Collection'Class;
                                  Qualified_Name : String; Value : String;
                                  Case_Sensetive : Boolean);

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

   -- Elements

   type Dom_Element_Ptr is new Address;
   type Dom_Element is new Controlled with record
      Ptr : Dom_Element_Ptr;
   end record;
   No_Dom_Element : constant Dom_Element := (Controlled with Ptr => Dom_Element_Ptr(Null_Address));

   function Lxb_Html_Document_Head_Element(Doc : Html_Document_Ptr) return Dom_Element_Ptr
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_html_document_head_element_noi";

   type Dom_Collection_Ptr is new Address;
   type Dom_Collection is new Controlled with record
      Ptr : Dom_Collection_Ptr;
   end record;
   overriding procedure Finalize(Col : in out Dom_Collection);

   function Lxb_Dom_Elements_By_Attr(Root : Dom_Element_Ptr;
                                     Collection : Dom_Collection_Ptr;
                                     Qualified_Name : Chars_Ptr; Qname_Len : Size_T;
                                     Value : Chars_Ptr; Value_Len : Size_T;
                                     Case_Insensitive : Int) return Lxb_Status
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_dom_elements_by_attr";

   function Lxb_Dom_Element_Get_Attribute(Elem : Dom_Element_Ptr;
                                          Name : Chars_Ptr; Name_Length : Size_T;
                                          Value_Len : out Size_T) return Chars_Ptr
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_dom_element_get_attribute";

   function Lxb_Dom_Collection_Make(Doc : Html_Document_Ptr; Size : Size_T) return Dom_Collection_Ptr
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_dom_collection_make_noi";
   procedure Lxb_Dom_Collection_Destroy(Col : Dom_Collection_Ptr; Self_Destroy : Int)
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_dom_collection_destroy";

   function Lxb_Dom_Collection_Length(Col : Dom_Collection_Ptr) return Size_T
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_dom_collection_length_noi";
   function Lxb_Dom_Collection_Element(Col: Dom_Collection_Ptr; Elem : Size_T) return Dom_Element_Ptr
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_dom_collection_element_noi";
   procedure Lxb_Dom_Collection_Clean(Col: Dom_Collection_Ptr)
   with
     Import => True,
     Convention => C,
     External_Name => "lxb_dom_collection_clean_noi";
end Lexbor;
