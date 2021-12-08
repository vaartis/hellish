package body Hellish_Web.Bencoder is
   -- Encode

   function Encode(Value : Json_Types.JSON_Value) return Bencode_Value is
      use Json_Types;
   begin
      case Value.Kind is
         when String_Kind =>
            declare
               Value_String : constant String := Value.Value;
               String_Length : constant Natural := Value_String'Length;
               Formatted_String : constant String := Trim(String_Length'Image, Ada.Strings.Left) & ":" & Value_String;
            begin
               return (Kind => Value.Kind,
                       Value => Value, Encoded => To_Unbounded_String(Formatted_String));
            end;
         when Integer_Kind =>
            return (Kind => Value.Kind,
                    Value => Value, Encoded => To_Unbounded_String("i" & Trim(Value.Image, Ada.Strings.Left) & 'e'));
         when Array_Kind =>
            declare
               Result : Unbounded_String;
            begin
               Append(Result, "l");
               for Element of Value loop
                  Append(Result, Encode(Element).Encoded);
               end loop;
               Append(Result, "e");

               return (Kind => Value.Kind,
                       Value => Value, Encoded => Result);
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

               return (Kind => Value.Kind,
                       Value => Value, Encoded => Result);
            end;
         when others => raise Encode_Error with (Value.Kind'Image & " is not allowed");
      end case;
   end Encode;

   -- Image

   function Image(Value : Bencode_Value) return String is
   begin
      return To_String(Value.Encoded);
   end Image;
end Hellish_Web.Bencoder;
