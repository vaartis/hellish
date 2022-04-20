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
end Lexbor;
