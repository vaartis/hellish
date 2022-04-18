with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

package body Hellish_Web.Bencoder is
   function Encode(Value : Long_Long_Integer) return Holder is
   begin
      return
        To_Holder(Bencode_Integer'(Value => Value,
                                   Encoded => To_Unbounded_String("i" & Trim(Value'Image, Ada.Strings.Left) & 'e')));
   end Encode;

   function Encode(Value : Unbounded_String) return Holder is
      String_Length : constant Natural := Length(Value);
      Formatted_String : constant Unbounded_String := Trim(String_Length'Image, Ada.Strings.Left) & ":" & Value;
   begin
      return
        To_Holder(Bencode_String'(Value => Value,
                                  Encoded => Formatted_String));
   end Encode;

   function Encode(Value : String) return Holder is
      String_Length : constant Natural := Value'Length;
      Formatted_String : constant String := Trim(String_Length'Image, Ada.Strings.Left) & ":" & Value;
   begin
      return
        To_Holder(Bencode_String'(Value => To_Unbounded_String(Value),
                                  Encoded => To_Unbounded_String(Formatted_String)));
   end Encode;

   function Encode(Value : Bencode_Vectors.Vector) return Holder is
      Result : Unbounded_String;
   begin
      Append(Result, "l");
      for Element of Value loop
         -- Append to the resulting string
         Append(Result, Element.Element.Encoded);
      end loop;
      Append(Result, "e");

      return To_Holder(Bencode_List'(Value => Value, Encoded => Result));
   end Encode;

   function Encode_Map(The_Map : Bencode_Maps.Map) return Unbounded_String is
      Result : Unbounded_String;
   begin
      Append(Result, "d");
      for Kv in The_Map.Iterate loop
         -- Key
         Append(Result, Encode(Key(Kv)).Element.Encoded);
         -- Value
         Append(Result, The_Map(Kv).Element.Element.Encoded);
      end loop;
      Append(Result, "e");

      return Result;
   end;

   function Encode(The_Map : Bencode_Maps.Map) return Holder is
   begin
      return To_Holder(Bencode_Dict'(Value => The_Map, Encoded => Encode_Map(The_Map)));
   end Encode;

   procedure Include(The_Map : in out Bencode_Dict; Key : String; Value : Bencode_Value_Holders.Holder) is
   begin
      The_Map.Value.Include(To_Unbounded_String(Key), Value);
      -- Update the encoded value
      The_Map.Encoded := Encode_Map(The_Map.Value);
   end;

   function With_Failure_Reason(Reason : String) return Holder is
      Result : Bencode_Maps.Map;
   begin
      Include(Result, To_Unbounded_String("failure reason"), Encode(Reason));

      return Encode(Result);
   end With_Failure_Reason;

   function File_At(File : File_Type) return String is
   begin
      return " at " & Line(File)'Image & ":" & Col(File)'Image;
   end;

   function Decode(File : File_Type) return Bencode_Value_Holders.Holder is
      Char : Character;
      Eol : Boolean;
   begin
      Look_Ahead(File, Char, Eol);

      declare
         Unused_Tmp: Integer;
      begin
         Unused_Tmp := Integer'Value(( 1 => Char ));
         -- Confirmed that the appropriate type is a string
         return Decode_String(File);
      exception
         when Constraint_Error =>
            -- Not a string, must be something else
            case Char is
               when 'i' =>
                  return Decode_Integer(File);
               when 'l' =>
                  return Decode_List(File);
               when 'd' =>
                  return Decode_Dict(File);
               when others =>
                  raise Decode_Error with "Unknown character determining value type: " & Char & File_At(File);
            end case;
      end;
   end Decode;

   function Decode_String(File : File_Type) return Bencode_Value_Holders.Holder is
      Length : Natural;
      Char : Character;
      Eol : Boolean;
   begin
      declare
         Reading : Unbounded_String;
      begin
         loop
            -- This is stupid but it just refuses to read the number
            Look_Ahead(File, Char, Eol);
            exit when Char = ':';
            Get_Immediate(File, Char);

            Append(Reading, (1 => Char));
         end loop;
         Length := Natural'Value(To_String(Reading));
      end;

      Get_Immediate(File, Char);
      if Char /= ':' then
         raise Decode_Error with "Expected number to be followed by a :, but instead found " & Char & File_At(File);
      end if;
      declare
         Content : String (1..Length);
      begin
         for I in 1..Length loop
            Get_Immediate(File, Content(I));
         end loop;

         return Encode(Content);
      end;
   end Decode_String;

   function Decode_Integer(File : File_Type) return Bencode_Value_Holders.Holder is
      Char : Character;
      Result : Long_Long_Integer;
   begin
      Get_Immediate(File, Char);
      if Char /= 'i' then
         raise Decode_Error with "Expected a number to begin with i, but instead found" & Char & File_At(File);
      end if;

      declare
         Reading : Unbounded_String;
         Eol : Boolean;
      begin
         loop
            -- This is stupid but it just refuses to read the number
            Look_Ahead(File, Char, Eol);
            exit when Char = 'e';
            Get_Immediate(File, Char);

            Append(Reading, (1 => Char));
         end loop;
         Result := Long_Long_Integer'Value(To_String(Reading));
      end;

      Get_Immediate(File, Char);
      if Char /= 'e' then
         raise Decode_Error with "Expected a number to end with e, but instead found" & Char & File_At(File);
      end if;

      return Encode(Result);
   end Decode_Integer;

   function Decode_List(File : File_Type) return Bencode_Value_Holders.Holder is
      Char : Character;
      Eol : Boolean;

      Result : Bencode_Vectors.Vector;
   begin
      Get_Immediate(File, Char);
      if Char /= 'l' then
         raise Decode_Error with "Expected a list to begin with l, but instead found" & Char & File_At(File);
      end if;
      loop
         Look_Ahead(File, Char, Eol);
         exit when Char = 'e';

         Result.Append(Decode(File));
      end loop;
      -- Always 'e'
      Get_Immediate(File, Char);
      if Char /= 'e' then
         raise Decode_Error with "Expected a list to end with e, but instead found" & Char & File_At(File);
      end if;

      return Encode(Result);
   end Decode_List;

   function Decode_Dict(File : File_Type) return Bencode_Value_Holders.Holder is
      Char : Character;
      Eol : Boolean;

      Result : Bencode_Maps.Map;
   begin
      Get_Immediate(File, Char);
      if Char /= 'd' then
         raise Decode_Error with "Expected a dict to begin with d, but instead found" & Char & File_At(File);
      end if;
      loop
         Look_Ahead(File, Char, Eol);
         exit when Char = 'e';

         Result.Include(Bencode_String((Decode_String(File).Element)).Value,
                        Decode(File));
      end loop;

      -- Always 'e'
      Get_Immediate(File, Char);
      if Char /= 'e' then
         raise Decode_Error with "Expected a list to end with e, but instead found" & Char & File_At(File);
      end if;

      return Encode(Result);
   end Decode_Dict;

end Hellish_Web.Bencoder;
