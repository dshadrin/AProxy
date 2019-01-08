with Interfaces;

package Pal is

   subtype int8_t is Interfaces.Integer_8;
   subtype int16_t is Interfaces.Integer_16;
   subtype int32_t is Interfaces.Integer_32;
   subtype int64_t is Interfaces.Integer_64;

   subtype uint8_t  is Interfaces.Unsigned_8;
   subtype uint16_t is Interfaces.Unsigned_16;
   subtype uint32_t is Interfaces.Unsigned_32;
   subtype uint64_t is Interfaces.Unsigned_64;

end Pal;
