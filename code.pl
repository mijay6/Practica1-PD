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


% TODO: Se debe hacer con aritmetica de peano?

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


%TODO: Terminar de implementar:

%--------------------------------------------
% Predicate 3: byte_list_convert/2
%--------------------------------------------





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


