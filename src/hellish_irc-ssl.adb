pragma Unsuppress(All_Checks);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System; use System;

separate (Hellish_Irc)
package body Ssl is
   type Ssl_Context is new Address;
   type Ssl_Method is new Address;

   type Cert_File_Type is (Pem)
   with
     Convention => C;
   for Cert_File_Type use (Pem => 1);

   function Tls_Server_Method return Ssl_Method
   with
     Import => True,
     Convention => C,
     External_Name => "TLS_server_method";

   function Ssl_Ctx_New(Method : Ssl_Method) return Ssl_Context
   with
     Import => True,
     Convention => C,
     External_Name => "SSL_CTX_new";

   function Ssl_Ctx_Use_Certificate_Chain_File(Ctx : Ssl_Context;
                                               Filename : Chars_Ptr;
                                               Filetype : Cert_File_Type) return Integer
   with
     Import => True,
     Convention => C,
     External_Name => "SSL_CTX_use_certificate_chain_file";

   function Ssl_Ctx_Use_Private_Key_File(Ctx : Ssl_Context;
                                         Filename : Chars_Ptr;
                                         Filetype : Cert_File_Type) return Integer
   with
     Import => True,
     Convention => C,
     External_Name => "SSL_CTX_use_PrivateKey_file";

   function Err_Get_Error return Unsigned_Long
   with
     Import => True,
     Convention => C,
     External_Name => "ERR_get_error";
   function Err_Error_String(Error_Num : Unsigned_Long; Buf : Chars_Ptr) return Chars_Ptr
   with
     Import => True,
     Convention => C,
     External_Name => "ERR_error_string";

   function Get_Error_String return String is
   begin
      return Value(Err_Error_String(Err_Get_Error, Null_Ptr));
   end Get_Error_String;

   -- Not gonna bother with freeing the context, because it lives for the whole duration of the
   -- program's life
   Ctx : Ssl_Context;

   -- null = error
   function Ssl_New(Ctx : Ssl_Context) return Ssl
   with
     Import => True,
     Convention => C,
     External_Name => "SSL_new";
   -- 0 = error, 1 = ok
   function Ssl_Set_Fd(The_Ssl : Ssl; Fd : Integer) return Integer
   with
     Import => True,
     Convention => C,
     External_Name => "SSL_set_fd";
   procedure Ssl_Shutdown(The_Ssl : Ssl)
   with
     Import => True,
     Convention => C,
     External_Name => "SSL_shutdown";
   procedure Ssl_Free(The_Ssl : Ssl)
   with
     Import => True,
     Convention => C,
     External_Name => "SSL_free";
   function SSL_write(The_Ssl : Ssl;
                       Reply : Chars_Ptr;
                       Reply_Length : Size_T) return Integer
   with
     Import => True,
     Convention => C,
     External_Name => "SSL_write";
   function SSL_accept(The_Ssl : ssl) return Integer
   with
     Import => True,
     Convention => C,
     External_Name => "SSL_accept";
   function SSL_read(The_Ssl : Ssl;
                     Buffer : Chars_Ptr;
                     Buf_size : Integer) return Integer
   with
     Import => True,
     Convention => C,
     External_Name => "SSL_read";

   function Create_Ssl return Ssl is
   begin
      return Result : Ssl := Ssl_New(Ctx) do
         if Address(Result) = Null_Address then
            raise Ssl_Error with Get_Error_String;
         end if;
      end return;
   end Create_Ssl;

   procedure Set_Ssl_Fd(The_Ssl : Ssl; Socket : Socket_Type) is
      Sock_Fd : Integer := To_C(Socket);
      Result : Integer := Ssl_Set_Fd (The_ssl, Sock_Fd);
   begin
      if Result = 0 then
         raise Ssl_Error with Get_Error_String;
      end if;
   end Set_Ssl_Fd;

   procedure Free_Ssl(The_Ssl : Ssl) is
   begin
      Ssl_Shutdown(The_Ssl);
      Ssl_Free(The_Ssl);
   end Free_Ssl;

   function Send_Ssl(The_Ssl : Ssl; Reply : String) return Integer is
      Chars : Chars_Ptr := New_String(Reply);
      Result : Integer := Ssl_Write(The_Ssl, Chars, Strlen(Chars));
   begin
      if Result > 0 then
         return Result;
      else
         raise Ssl_Error with Get_Error_String;
      end if;
   end Send_Ssl;

   function Receive_Ssl(The_Ssl : Ssl; Max : Natural; Amount : out Integer) return String is
      pragma Warnings(Off, "* is read but never assigned");
      Data_Result : String (1..Max);
      -- No idea if there's a better way to allocate a buffer
      pragma Warnings(On, "* is read but never assigned");

      C_Str : Chars_Ptr := New_String(Data_Result);

   begin
      Amount := Ssl_Read(The_Ssl, C_Str, Max);

      if Amount <= 0 then
         Free(C_Str);
         raise Ssl_Error with Get_Error_String;
      else
         return Data : String := Value(C_Str, Size_T(Amount)) do
            Free(C_Str);
         end return;
      end if;
   end Receive_Ssl;

   procedure Accept_Ssl(The_Ssl : Ssl) is
      Result : Integer := Ssl_Accept(The_Ssl);
   begin
      if Result <= 0 then
         raise Ssl_Error with Get_Error_String;
      end if;
   end Accept_Ssl;

   procedure Initialize(Cert, Key : String) is
      Method : Ssl_Method := Tls_Server_Method;
   begin
      Ctx := Ssl_Ctx_New(Method);
      if Address(Ctx) = Null_Address then
         raise Ssl_Error with Get_Error_String;
      end if;

      declare
         Cert_Str : Chars_Ptr := New_String(Cert);
         Key_Str : Chars_Ptr := New_String(Key);

         Cert_Result : Integer := Ssl_Ctx_Use_Certificate_Chain_File(Ctx, Cert_Str, Pem);
         Key_Result : Integer := Ssl_Ctx_Use_Private_Key_File(Ctx, Key_Str, Pem);
      begin
         Free(Cert_Str);
         Free(Key_Str);

         if Cert_Result <= 0 or Key_Result <= 0 then
            raise Ssl_Error with Get_Error_String;
         end if;
      end;
   end;
end Ssl;
