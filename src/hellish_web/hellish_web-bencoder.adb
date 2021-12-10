package body Hellish_Web.Bencoder is
   function Encode(Value : Natural) return Holder is
   begin
      return
        To_Holder(Bencode_Integer'(Value => Value,
                                   Encoded => To_Unbounded_String("i" & Trim(Value'Image, Ada.Strings.Left) & 'e')));
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
      The_Map.Value.Include(Key, Value);
      -- Update the encoded value
      The_Map.Encoded := Encode_Map(The_Map.Value);
   end;

   function With_Failure_Reason(reason : String) return Holder is
      Result : Bencode_Maps.Map;
   begin
      Include(Result, "failure reason", Encode(Reason));

      return Encode(Result);
   end With_Failure_Reason;
end Hellish_Web.Bencoder;
