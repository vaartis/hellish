package body Hellish_Web.Bencoder is
   -- Encode

   function Encode(Value : Json_Types.JSON_Value) return Bencode_Value'Class is
      use Json_Types;
   begin
      case Value.Kind is
         when String_Kind => return Encode(String'(Value.Value));
         when Integer_Kind => return Encode(Integer'(Value.Value));
         when Array_Kind =>
            declare
               Result : Unbounded_String;
            begin
               Append(Result, "l");
               for Element of Value loop
                  Append(Result, Encode(Element).Encoded);
               end loop;
               Append(Result, "e");

               return Bencode_List'(Value => Value, Encoded => Result);
            end;
         when Object_Kind =>
            declare
               Result : Unbounded_String;
            begin
               Append(Result, "d");
               for Element of Value loop
                  -- Key
                  Append(Result, Encode(Element).Encoded);
                  -- Value
                  Append(Result, Encode(Value(String'(Element.Value))).Encoded);
               end loop;
               Append(Result, "e");

               return Bencode_Dictionary'(Value => Value, Encoded => Result);
            end;
         when others => return Encode(1);
      end case;
   end Encode;

   function Encode(Value : String) return Bencode_String is
      String_Length : constant Natural := Value'Length;
      Formatted_String : constant String := Trim(String_Length'Image, Ada.Strings.Left) & ":" & Value;
   begin
      return (Value => To_Unbounded_String(Value), Encoded => To_Unbounded_String(Formatted_String));
   end Encode;

   function Encode(Value : Integer) return Bencode_Integer is
   begin
      return
        (Value => Value, Encoded => To_Unbounded_String("i" & Trim(Value'Image, Ada.Strings.Left) & 'e'));
   end Encode;

   -- Image

   function Image(Value : Bencode_Value) return String is
   begin
      return To_String(Value.Encoded);
   end Image;
end Hellish_Web.Bencoder;
