package body Lexbor is
   overriding procedure Initialize(Doc : in out Html_Document) is
   begin
      Doc.Ptr := Html_Document_Create;
   end Initialize;

   overriding procedure Finalize(Doc : in out Html_Document) is
   begin
      html_document_destroy(Doc.Ptr);
   end Finalize;

   procedure Parse(Doc : Html_Document; Html : String) is
      C_Html : Chars_Ptr := New_String(Html);
      Result : Lxb_Status := Html_Document_Parse(Doc.Ptr, C_Html, Strlen(C_Html));
   begin
      Free(C_Html);

      if Result /= Lxb_Status_Ok then
         raise Lexbor_Error with Result'Image;
      end if;
   end Parse;

   function Title(Doc : Html_Document) return String is
      Title_Len : Size_T;
      Title_Str : Chars_Ptr := Html_Document_Title(Doc.Ptr, Title_Len);
   begin
      return Value(Title_Str, Title_Len);
   end;

   function Document_Head(Doc : Html_Document) return Dom_Element'Class is
   begin
      return Result : Dom_Element do
         Result.Ptr := lxb_html_document_head_element(Doc.Ptr);
      end return;
   end;

   function Get_Attribute(Elem : Dom_Element'Class; Attr : String) return String is
      Attr_Str : Chars_Ptr := New_String(Attr);
      Attr_Len : Size_T := Size_T(Attr'Length);

      Result_Len : Size_T;
      Result : Chars_Ptr := Lxb_Dom_Element_Get_Attribute(Elem.Ptr, Attr_Str, Attr_Len, Result_Len);
   begin
      return Res_Str : String := Value(Result, Result_Len) do
         Free(Attr_Str);
      end return;
   end Get_Attribute;

   function Make_Dom_Collection(Doc : Html_Document; Size : Natural) return Dom_Collection'Class is
   begin
      return Result : Dom_Collection do
         Result.Ptr := Lxb_Dom_Collection_Make(Doc.Ptr, Size_T(Size));
      end return;
   end Make_Dom_Collection;

   overriding procedure Finalize(Col : in out Dom_collection) is
   begin
      Lxb_Dom_Collection_Destroy(Col.Ptr, 1);
   end Finalize;

   function Length(Col : Dom_Collection'Class) return Natural is (Integer(Lxb_Dom_Collection_Length(Col.Ptr)));
   procedure Clean(Col : Dom_Collection'Class) is
   begin
      Lxb_Dom_Collection_Clean(Col.Ptr);
   end Clean;
   function Element(Col : Dom_Collection'Class; Elem : Natural) return Dom_Element'Class is
   begin
      return Result : Dom_Element do
         Result.Ptr := lxb_dom_collection_element (Col.ptr, Size_T(Elem));
      end return;
   end Element;

   procedure Dom_Elements_By_Attr(Root : Dom_Element; Collection : Dom_Collection'Class;
                                  Qualified_Name : String; Value : String;
                                  Case_Sensetive : Boolean) is
      Qname_C_Str : Chars_Ptr := New_String(Qualified_Name);
      Qname_Length : Natural := Qualified_Name'Length;
      Value_C_Str : Chars_Ptr := New_String(Value);
      Value_Length : Natural := Value'Length;

      Result : Lxb_Status := Lxb_Dom_Elements_By_Attr(Root.Ptr, Collection.Ptr,
                                                      Qname_C_Str, Size_T(Qname_Length),
                                                      Value_C_Str, Size_T(Value_Length),
                                                      (if Case_Sensetive then 1 else 0));
   begin
      Free(Qname_C_Str);
      Free(Value_C_Str);

      if Result /= Lxb_Status_Ok then
         raise Lexbor_Error with Result'Image;
      end if;
   end Dom_Elements_By_Attr;
end Lexbor;
