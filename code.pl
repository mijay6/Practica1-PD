:- module(_,_,[pure,assertions,regtypes]).

% AUTHOR INFORMATION
author_data('Dobra','','Mihai','240912').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% @pred bit(B)
% @arg B A binary bit represented as b(0) or b(1)
% Defines the binary bit type. A bit can only be 0 or 1, represented
% structurally as b(0) or b(1).
bit(b(0)).
bit(b(1)).

% @pred binary_byte(B)
% @arg B A list of 8 bits, from most significant bit (position 7) to least significant (position 0)
% Defines a binary byte as an ordered list of 8 binary bits.
% The first element is the most significant bit (position 7),
% and the last element is the least significant bit (position 0).
binary_byte([B7 , B6 , B5 , B4 , B3 , B2 , B1 , B0]) :-
    bit(B7),
    bit(B6),
    bit(B5),
    bit(B4),
    bit(B3),
    bit(B2),
    bit(B1),
    bit(B0).

% @pred hexd(H)
% @arg H A hexadecimal digit (nibble) represented structurally as h(X)
% Defines a hexadecimal digit (nibble) type.
% Each hex digit represents a 4-bit value (0-15).
% Note that while we use numeric constants (0-9) and alphabetic constants (a-f),
% they are treated as symbolic constants, not as decimal numbers.
hexd(h(0)).
hexd(h(1)).
hexd(h(2)).
hexd(h(3)).
hexd(h(4)).
hexd(h(5)).
hexd(h(6)).
hexd(h(7)).
hexd(h(8)).
hexd(h(9)).
hexd(h(a)).
hexd(h(b)).
hexd(h(c)).
hexd(h(d)).
hexd(h(e)).
hexd(h(f)).

% @pred hex_byte(B)
% @arg B A list of 2 hex digits (nibbles), from most significant (position 1) to least significant (position 0)
% Defines a hexadecimal byte as an ordered list of 2 hex digits (nibbles).
% The first element is the most significant nibble (position 1),
% and the second element is the least significant nibble (position 0).
hex_byte([H1, H0]) :-
    hexd(H1),
    hexd(H0).

% @pred byte(B)
% @arg B Either a binary byte or a hexadecimal byte
% Defines the byte type as either a binary byte or a hexadecimal byte.
% This abstraction allows for multiple representations of the same concept.
byte(BB) :-
    binary_byte(BB).
byte(HB) :-
    hex_byte(HB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%--------------------------------------------
% Predicate 1: byte_list/1
%--------------------------------------------

% @pred byte_list(L)
% @arg L A list of bytes (either binary or hexadecimal)

% This predicate is true when L is a list of valid bytes.
% Each element of the list must be either a binary byte (8 bits)
% or a hexadecimal byte (2 nibbles).

% The implementation uses recursion with two clauses:
% 1. Base case: An empty list is a valid byte list
% 2. Recursive case: For a non-empty list, check that the first element
%    is a valid byte and that the rest of the list is a valid byte list

byte_list([]).                        
byte_list([B|Bs]) :-                  
    byte(B),
    byte_list(Bs).


%--------------------------------------------
% Predicate 2: byte_convert/2
%--------------------------------------------

% @pred byte_convert(HexByte, BinByte)
% @arg HexByte A hexadecimal byte represented as a list of two hex digits
% @arg BinByte A binary byte represented as a list of eight bits

% This predicate is true when HexByte (in hexadecimal) represents
% the same value as BinByte (in binary).

% The implementation converts each hex digit to its 4-bit binary representation
% and then concatenates these bits to form the complete 8-bit binary byte.

byte_convert([H1, H0], BinByte) :-
    hex_byte([H1, H0]),
    nibble_to_bits(H1, [B7, B6, B5, B4]),
    nibble_to_bits(H0, [B3, B2, B1, B0]),
    BinByte = [B7, B6, B5, B4, B3, B2, B1, B0],
    binary_byte(BinByte).

%--------------------------------------------
% Predicate 3: byte_list_convert/2
%--------------------------------------------

% @pred byte_list_convert(HexList, BinList)
% @arg HexList A list of hexadecimal bytes (each represented as a list of two hex digits)
% @arg BinList A list of binary bytes (each represented as a list of eight bits)

% This predicate is true when HexList (a list of hexadecimal bytes)
% represents the same value as BinList (a list of binary bytes).

% The implementation uses recursion wtith two clauses:
% 1. Base case: An empty list of hexadecimal bytes is converted to an empty list of binary bytes
% 2. Recursive case: For a non-empty list, convert the first hexadecimal byte to its binary representation and recursively convert the rest of the list

byte_list_convert([],[]).
byte_list_convert([H|Hs], [B|Bs]) :-
    byte_convert(H, B),
    byte_list_convert(Hs, Bs).

%--------------------------------------------
% Predicate 4: get_nth_bit_from_byte/3
%--------------------------------------------

% @pred get_nth_bit_from_byte(N, B, BN)
% @arg N A Peano number representing the position of the bit (0-7)
% @arg B A byte (either binary or hexadecimal)
% @arg BN The N-th bit of the byte B

% This predicate is true when BN is the N-th bit of the byte B.
% The predicate uses two clauses:
% 1. If B is a binary byte, reverse the byte and get the N-th bit
% 2. If B is a hexadecimal byte, convert it to binary and then get the N-th bit

% The implementation uses the byte_convert/2 predicate to convert the hexadecimal byte to binary,
% and then uses reverse_list/2 to reverse the binary byte.
% The get_nth_bit/3 predicate is used to extract the N-th bit from the reversed byte.

get_nth_bit_from_byte(N, B, BN) :-
    binary_byte(B),
    reverse_list(B, RevB),
    get_nth_bit(N, RevB, BN).
get_nth_bit_from_byte(N, B, BN) :-
    hex_byte(B),
    byte_convert(B, BinB),
    reverse_list(BinB, RevB),
    get_nth_bit(N, RevB, BN).

%--------------------------------------------
% Predicate 5:  byte_list_clsh/2
%--------------------------------------------

% @pred byte_list_clsh(L, CLShL)
% @arg L A list of bytes (either binary or hexadecimal)
% @arg CLShL A list of bytes (either binary or hexadecimal) after a circular left shift

% This predicate is true when CLShL is the result of a circular left shift of the list of bytes L.
% The predicate uses two clauses:
% 1. If L is a list of binary bytes, convert the list to bits, perform the circular left shift, and convert back to bytes
% 2. If L is a list of hexadecimal bytes, convert the list to binary bytes, perform the circular left shift, and convert back to hexadecimal bytes

% The implementation uses the bytes_to_bits/2 predicate to convert the list of bytes to bits,
% the rotate_left/2 predicate to perform the circular left shift, and the bits_to_bytes/2 predicate
% to convert the bits back to bytes.
% The byte_list_convert/2 predicate is used to convert the hexadecimal bytes to binary bytes.

byte_list_clsh(L, CLShL):-
   bytes_to_bits(L, AllBits),
   rotate_left(AllBits, RotatedBits),
   bits_to_bytes(RotatedBits, CLShL).
byte_list_clsh(L, CLShL):-
   byte_list_convert(L, BinList),
   bytes_to_bits(BinList, AllBits),
   rotate_left(AllBits, RotatedBits),
   bits_to_bytes(RotatedBits, BinBytes),
   byte_list_convert(CLShL, BinBytes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AUXILIARY PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------
% Auxiliaries for byte_convert/2
%--------------------------------------------

% @pred nibble_to_bits(Nibble, Bits) 
% @arg Nibble A hexadecimal digit (0-f)
% @arg Bits List of 4 bits representing the nibble

% Converts a hexadecimal digit (nibble) to its 4-bit binary representation.
% Each hex digit maps to a unique pattern of 4 bits.

nibble_to_bits(h(0), [b(0), b(0), b(0), b(0)]).
nibble_to_bits(h(1), [b(0), b(0), b(0), b(1)]).
nibble_to_bits(h(2), [b(0), b(0), b(1), b(0)]).
nibble_to_bits(h(3), [b(0), b(0), b(1), b(1)]).
nibble_to_bits(h(4), [b(0), b(1), b(0), b(0)]).
nibble_to_bits(h(5), [b(0), b(1), b(0), b(1)]).
nibble_to_bits(h(6), [b(0), b(1), b(1), b(0)]).
nibble_to_bits(h(7), [b(0), b(1), b(1), b(1)]).
nibble_to_bits(h(8), [b(1), b(0), b(0), b(0)]).
nibble_to_bits(h(9), [b(1), b(0), b(0), b(1)]).
nibble_to_bits(h(a), [b(1), b(0), b(1), b(0)]).
nibble_to_bits(h(b), [b(1), b(0), b(1), b(1)]).
nibble_to_bits(h(c), [b(1), b(1), b(0), b(0)]).
nibble_to_bits(h(d), [b(1), b(1), b(0), b(1)]).
nibble_to_bits(h(e), [b(1), b(1), b(1), b(0)]).
nibble_to_bits(h(f), [b(1), b(1), b(1), b(1)]).

%--------------------------------------------
% Auxiliaries for get_nth_bit_from_byte/3
%--------------------------------------------

% @pred reverse_list(List, Reversed)
% @arg List A list to be reversed
% @arg Reversed The reversed version of the list

% This predicate is true when Reversed is the reverse of List.
% The implementation uses an accumulator to build the reversed list.

reverse_list(List, Reversed) :-
    reverse_acc(List, [], Reversed).

% @pred reverse_acc(List, Acc, Reversed)
% @arg List A list to be reversed
% @arg Acc An accumulator for building the reversed list
% @arg Reversed The reversed version of the list

% This predicate is true when the accumulator Acc contains the reversed version of List.
% The implementation uses tail recursion with two clauses:
% 1. Base case: An empty list is reversed to the accumulator
% 2. Recursive case: For a non-empty list, prepend the head of the list to the accumulator and recursively reverse the tail of the list.

reverse_acc([], Acc, Acc).
reverse_acc([X|Xs], Acc, Reversed) :-
    reverse_acc(Xs, [X|Acc], Reversed).

% @pred get_nth_bit(N, Bits, BN)
% @arg N A Peano number representing the position of the bit (0-7)
% @arg Bits A list of bits (binary byte)
% @arg BN The N-th bit of the byte

% This predicate is true when BN is the N-th bit of the inversed byte Bits.
% The implementation uses recursion with two clauses:
% 1. Base case: If N is 0, the first element of Bits is the desired bit
% 2. Recursive case: For a non-zero N, decrement N and recursively call the predicate with the tail of the list (Bits) to find the next bit.

get_nth_bit(0, [BN|_], BN).
get_nth_bit(s(N), [_|Bits], BN) :-
    get_nth_bit(N, Bits, BN).

% @pred my_append(List1, List2, Result)
% @arg List1 The first list
% @arg List2 The second list
% @arg Result The concatenation of List1 and List2

% This predicate is true when Result is the concatenation of List1 and List2.
% The implementation uses recursion with two clauses:
% 1. Base case: An empty list concatenated with another list is the second list
% 2. Recursive case: For a non-empty list, prepend the head of List1 to the result and recursively concatenate the tail of List1 with List2.

my_append([], L, L).
my_append([H|T], L, [H|R]) :-
   my_append(T, L, R).


%---------------------------------------------
% Auxiliaries for byte_list_clsh/2
%---------------------------------------------

% @pred bits_to_bytes(Bits, Bytes)
% @arg Bits A list of bits (binary bytes)
% @arg Bytes A list of bytes (binary bytes)

% This predicate is true when Bytes is the list of bytes represented by Bits.
% The implementation uses recursion with two clauses:
% 1. Base case: An empty list of bits is converted to an empty list of bytes
% 2. Recursive case: For a non-empty list, convert the first 8 bits to a byte and recursively convert the rest of the bits to bytes.

% The implementation uses the binary_byte/1 predicate to check that the first 8 bits form a valid binary byte.
% The my_append/3 predicate is used to concatenate the bits into a single byte.

bytes_to_bits([], []).
bytes_to_bits([B|Bs], Bits) :-
   binary_byte(B),
   bytes_to_bits(Bs, RestBits),
   my_append(B, RestBits, Bits).

% @pred rotate_left(List, Rotated)
% @arg List A list of bits
% @arg Rotated The list of bits after a circular left shift

% This predicate is true when Rotated is the result of a circular left shift of List.

% The implementation uses two clauses:
% 1. An empty list is rotated to an empty list
% 2. For a non-empty list, append the tail of the list to the head and return the rotated list, using the my_append/3 predicate.

rotate_left([], []).
rotate_left([H|T], Rotated) :-
   my_append(T, [H], Rotated).

% @pred bits_to_bytes(Bits, Bytes)
% @arg Bits A list of bits
% @arg Bytes A list of binary bytes

% This predicate is true when Bytes is the list of bytes represented by Bits.
% The implementation uses recursion with two clauses:
% 1. Base case: An empty list of bits is converted to an empty list of bytes
% 2. Recursive case: For a non-empty list, convert the first 8 bits to a byte and recursively convert the rest of the bits to bytes.

% The my_append/3 predicate is used to concatenate the bits into a single byte.

bits_to_bytes([], []).
bits_to_bytes([B1, B2, B3, B4, B5, B6, B7, B8|RestBits], [H|RestBytes]) :-
   my_append([B1, B2, B3, B4], [B5, B6, B7, B8], H),
   bits_to_bytes(RestBits, RestBytes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------
% test for byte_list/1
%--------------------------------------------

% Test empty list
:- test byte_list(L) : (L = []) + not_fails.     
% Test list with a single binary byte
:- test byte_list(L) : (L = [[h(0), h(5)], [h(6), h(f)]]) + not_fails.
% Test list with a single binary byte
:- test byte_list(L) : (L = [[b(1), b(0), b(1), b(0), b(1), b(0), b(1), b(0)]]) + not_fails.
% Test mixed list (binary and hex bytes)
:- test byte_list(L) : (L = [[b(1), b(0), b(1), b(0), b(1), b(0), b(1), b(0)], [h(a), h(b)]]) + not_fails.
% Test invalid list (not bytes)
:- test byte_list(L) : (L = [1, 2, 3]) + fails.
% Test partially valid list (with non-byte element)
:- test byte_list(L) : (L = [[h(a), h(b)], 42]) + fails.
% Test invalid binary byte (too few bits)
:- test byte_list(L) : (L = [[b(1), b(0), b(1), b(0), b(1), b(0), b(1)]]) + fails.
% Test invalid hex byte (invalid digit)
:- test byte_list(L) : (L = [[h(g), h(5)]]) + fails.

%--------------------------------------------
% test for byte_convert/2
%--------------------------------------------

% Test converting 0x00 to binary
:- test byte_convert(HB, BB) : (HB = [h(0), h(0)]) 
   => (BB = [b(0), b(0), b(0), b(0), b(0), b(0), b(0), b(0)]) + not_fails.
% Test converting 0xFF to binary
:- test byte_convert(HB, BB) : (HB = [h(f), h(f)]) 
   => (BB = [b(1), b(1), b(1), b(1), b(1), b(1), b(1), b(1)]) + not_fails.
% Test converting 0x5A to binary
:- test byte_convert(HB, BB) : (HB = [h(5), h(a)]) 
   => (BB = [b(0), b(1), b(0), b(1), b(1), b(0), b(1), b(0)]) + not_fails.
% Test converting 0xA5 to binary
:- test byte_convert(HB, BB) : (HB = [h(a), h(5)]) 
   => (BB = [b(1), b(0), b(1), b(0), b(0), b(1), b(0), b(1)]) + not_fails.
% Test invalid hex byte (non-hex digit)
:- test byte_convert(HB, BB) : (HB = [h(g), h(5)]) + fails.
% Test invalid hex byte (too few digits)
:- test byte_convert(HB, BB) : (HB = [h(a)]) + fails.
% Test bidirectionality (verify binary to hex conversion works)
:- test byte_convert(HB, BB) : (BB = [b(1), b(0), b(0), b(1), b(0), b(1), b(1), b(0)]) 
   => (HB = [h(9), h(6)]) + not_fails.

%--------------------------------------------
% test for byte_list_convert/2
%--------------------------------------------

% Test empty list conversion
:- test byte_list_convert(HL, BL) : (HL = []) 
   => (BL = []) + not_fails.
% Test conversion of single byte
:- test byte_list_convert(HL, BL) : (HL = [[h(0), h(0)]]) 
   => (BL = [[b(0), b(0), b(0), b(0), b(0), b(0), b(0), b(0)]]) + not_fails.
% Test conversion of multiple bytes
:- test byte_list_convert(HL, BL) : (HL = [[h(f), h(f)], [h(0), h(0)]]) 
   => (BL = [[b(1), b(1), b(1), b(1), b(1), b(1), b(1), b(1)], 
             [b(0), b(0), b(0), b(0), b(0), b(0), b(0), b(0)]]) + not_fails.
% Test conversion of a sequence of bytes
:- test byte_list_convert(HL, BL) : (HL = [[h(a), h(b)], [h(c), h(d)], [h(e), h(f)]]) 
   => (BL = [[b(1), b(0), b(1), b(0), b(1), b(0), b(1), b(1)], 
             [b(1), b(1), b(0), b(0), b(1), b(1), b(0), b(1)], 
             [b(1), b(1), b(1), b(0), b(1), b(1), b(1), b(1)]]) + not_fails.
% Test invalid input: list with non-hex byte
:- test byte_list_convert(HL, BL) : (HL = [[h(a), h(b)], [h(g), h(h)]]) + fails.
% Test invalid input: incomplete hex byte
:- test byte_list_convert(HL, BL) : (HL = [[h(a), h(b)], [h(c)]]) 
   + fails.
% Test bidirectionality: determine hex bytes from binary bytes
:- test byte_list_convert(HL, BL) : (BL = [[b(0), b(1), b(0), b(1), b(1), b(0), b(1), b(0)], [b(1), b(0), b(0), b(1), b(0), b(1), b(1), b(0)]]) 
   => (HL = [[h(5), h(a)], [h(9), h(6)]]) + not_fails.

%--------------------------------------------
% test for get_nth_bit_from_byte/3
%--------------------------------------------

% Test get 0th bit from binary byte
:- test get_nth_bit_from_byte(N, B, BN) : (N = 0, B = [b(1), b(0), b(1), b(0), b(1), b(0), b(1), b(0)]) 
   => (Bit = b(0)) + not_fails.
% Test get 3rd bit from binary byte
:- test get_nth_bit_from_byte(N, B, BN) : (N = s(s(s(0))), B = [b(1), b(0), b(1), b(0), b(1), b(0), b(1), b(0)]) 
   => (Bit = b(1)) + not_fails.
%  Test get 7th (most significant) bit from binary byte
:- test get_nth_bit_from_byte(N, B, BN) : (N = s(s(s(s(s(s(s(0))))))), B = [b(1), b(0), b(1), b(0), b(1), b(0), b(1), b(0)]) 
   => (Bit = b(1)) + not_fails.
% Test get 0th bit from hex byte
:- test get_nth_bit_from_byte(N, B, BN) : (N = 0, B = [h(a), h(5)]) 
   => (Bit = b(1)) + not_fails.
% Test get 4rd bit from hex byte
:- test get_nth_bit_from_byte(N, B, BN) : (N = s(s(s(s(0)))), B = [h(a), h(5)]) 
   => (Bit = b(0)) + not_fails.
% Test get 7th (least significant) bit from hex byte
:- test get_nth_bit_from_byte(N, B, BN) : (N = s(s(s(s(s(s(s(0))))))), B = [h(a), h(5)]) 
   => (Bit = b(1)) + not_fails.
% Test with position out of range (greater than 7)
:- test get_nth_bit_from_byte(N, B, BN) : (N = s(s(s(s(s(s(s(s(0)))))))), B = [b(1), b(0), b(1), b(0), b(1), b(0), b(1), b(0)]) 
   + fails.
% Test with position out of range (negative)
:- test get_nth_bit_from_byte(N, B, BN) : (N = -1, B = [b(1), b(0), b(1), b(0), b(1), b(0), b(1), b(0)]) 
   + fails.
% Test with incomplete binary byte
:- test get_nth_bit_from_byte(N, B, BN) : (N = 0, B = [b(1), b(0), b(1), b(0), b(1), b(0), b(1)]) 
   + fails.
% Test with incomplete hex byte
:- test get_nth_bit_from_byte(N, B, BN) : (N = 0, B = [h(a)]) 
   + fails.
% Test with invalid hex byte
:- test get_nth_bit_from_byte(N, B, BN) : (N = 0, B = [h(x), h(y)]) 
   + fails.
% Test with uninitialized position (find position)
:- test get_nth_bit_from_byte(N, B, BN) : (B = [h(5), h(5)], BN = b(1)) 
   => (N = 0 ; N = s(s(0)) ; N = s(s(s(s(0)))) ; N = s(s(s(s(s(s(0))))))) 
   + not_fails.
% Test with uninitialized position and byte (find all combinations)
:- test get_nth_bit_from_byte(N, B, BN) : (BN = b(1)) 
   + not_fails.
% Test with uninitialized position and bit (find all bits of a hexbyte)
:- test get_nth_bit_from_byte(N, B, Bit) : 
   (B = [h(a), h(5)]) 
   => (
      (N = 0, Bit = b(1)) ; 
      (N = s(0), Bit = b(0)) ; 
      (N = s(s(0)), Bit = b(1)) ; 
      (N = s(s(s(0))), Bit = b(0)) ; 
      (N = s(s(s(s(0)))), Bit = b(0)) ; 
      (N = s(s(s(s(s(0))))), Bit = b(1)) ; 
      (N = s(s(s(s(s(s(0)))))), Bit = b(0)) ; 
      (N = s(s(s(s(s(s(s(0))))))), Bit = b(1))
   ) + not_fails.
% Test with uninitializaed byte, find a specific byte with a specific bit in a specific position
:- test get_nth_bit_from_byte(N, B, Bit) : 
   (N = s(s(s(0))), Bit = b(1)) 
   + not_fails.


%--------------------------------------------
% test for byte_list_clsh/2
%--------------------------------------------

% Test empty list
:- test byte_list_clsh(L, CLShL) : (L = []) 
   => (CLShL = []) + not_fails.
% Test single binary byte
:- test byte_list_clsh(L, CLShL) : (L = [[b(1),b(0),b(1),b(0),b(1),b(0),b(1),b(0)]]) 
   => (CLShL = [[b(0),b(1),b(0),b(1),b(0),b(1),b(0),b(1)]]) + not_fails.
% Test single hex byte
:- test byte_list_clsh(L, CLShL) : (L = [[h(5),h(a)]]) 
   => (CLShL = [[h(b),h(4)]]) + not_fails.
% Test multiple binary bytes
:- test byte_list_clsh(L, CLShL) : (L = [[b(1),b(0),b(1),b(0),b(1),b(0),b(1),b(0)], [b(1),b(1),b(0),b(0),b(1),b(1),b(0),b(0)]]) 
   => (CLShL = [[b(0),b(1),b(0),b(1),b(0),b(1),b(0),b(1)], 
                [b(1),b(0),b(0),b(1),b(1),b(0),b(0),b(1)]]) + not_fails.
% Test with the example from the statement
:- test byte_list_clsh(L, CLShL) : (L = [[h(5),h(a)], [h(2),h(3)], [h(5),h(5)], [h(3),h(7)]]) 
   => (CLShL = [[h(b),h(4)], [h(4),h(6)], [h(a),h(a)], [h(6),h(e)]]) + not_fails.
% Test with a binary byte all zeros (keep it the same)
:- test byte_list_clsh(L, CLShL) : (L = [[b(0),b(0),b(0),b(0),b(0),b(0),b(0),b(0)]]) 
   => (CLShL = [[b(0),b(0),b(0),b(0),b(0),b(0),b(0),b(0)]]) + not_fails.
% Test with an wrong entry
:- test byte_list_clsh(L, CLShL) : 
   (L = [[h(a),h(5)], [b(1),b(0),b(1),b(0),b(1),b(0),b(1),b(0)]]) + fails.