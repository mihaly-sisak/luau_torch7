/* C code produced by gperf version 3.1 */
/* Command-line: gperf torch_types.gperf  */
/* Computed positions: -k'7' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gperf@gnu.org>."
#endif

#line 7 "torch_types.gperf"

#include <string.h>
#include "torch_types.h"
#line 11 "torch_types.gperf"
struct torch_type_entry;

#define TOTAL_KEYWORDS 19
#define MIN_WORD_LENGTH 11
#define MAX_WORD_LENGTH 19
#define MIN_HASH_VALUE 11
#define MAX_HASH_VALUE 42
/* maximum key range = 32, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register size_t len;
{
  static const unsigned char asso_values[] =
    {
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 10, 25, 20,  5, 43,
      15,  5, 10,  0, 43, 43,  5, 43, 43, 43,
      43, 43, 43,  0,  0, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
      43, 43, 43, 43, 43, 43
    };
  return len + asso_values[(unsigned char)str[6]];
}

static const struct torch_type_entry wordlist[] =
  {
    {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
    {""}, {""},
#line 29 "torch_types.gperf"
    {"torch.Timer",         torch_type_Timer},
    {""}, {""}, {""},
#line 24 "torch_types.gperf"
    {"torch.IntTensor",     torch_type_IntTensor},
#line 16 "torch_types.gperf"
    {"torch.IntStorage",    torch_type_IntStorage},
#line 23 "torch_types.gperf"
    {"torch.ShortTensor",   torch_type_ShortTensor},
#line 15 "torch_types.gperf"
    {"torch.ShortStorage",  torch_type_ShortStorage},
    {""},
#line 30 "torch_types.gperf"
    {"torch.Generator",     torch_type_Generator},
#line 25 "torch_types.gperf"
    {"torch.LongTensor",    torch_type_LongTensor},
#line 17 "torch_types.gperf"
    {"torch.LongStorage",   torch_type_LongStorage},
#line 27 "torch_types.gperf"
    {"torch.DoubleTensor",  torch_type_DoubleTensor},
#line 19 "torch_types.gperf"
    {"torch.DoubleStorage", torch_type_DoubleStorage},
#line 31 "torch_types.gperf"
    {"torch.Allocator",     torch_type_Allocator},
#line 28 "torch_types.gperf"
    {"torch.HalfTensor",    torch_type_HalfTensor},
#line 20 "torch_types.gperf"
    {"torch.HalfStorage",   torch_type_HalfStorage},
    {""}, {""}, {""}, {""},
#line 26 "torch_types.gperf"
    {"torch.FloatTensor",   torch_type_FloatTensor},
#line 18 "torch_types.gperf"
    {"torch.FloatStorage",  torch_type_FloatStorage},
    {""}, {""},
#line 22 "torch_types.gperf"
    {"torch.CharTensor",    torch_type_CharTensor},
#line 14 "torch_types.gperf"
    {"torch.CharStorage",   torch_type_CharStorage},
    {""}, {""}, {""},
#line 21 "torch_types.gperf"
    {"torch.ByteTensor",    torch_type_ByteTensor},
#line 13 "torch_types.gperf"
    {"torch.ByteStorage",   torch_type_ByteStorage}
  };

const struct torch_type_entry *
get_torch_type_entry (str, len)
     register const char *str;
     register size_t len;
{
  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register unsigned int key = hash (str, len);

      if (key <= MAX_HASH_VALUE)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strncmp (str + 1, s + 1, len - 1) && s[len] == '\0')
            return &wordlist[key];
        }
    }
  return 0;
}
