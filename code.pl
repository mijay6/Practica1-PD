:- module(_,_,[pure,assertions,regtypes]).

% Command to generate documentation
% lpdoc -t html --doc_mainopts=tests code.pl

:- doc(title, "Práctica 1: Programación Lógica Pura").

:- doc(author_data/4, "Define los datos del autor del módulo").

author_data('Dobra','','Mihai','240912').

:- doc(module, "
@section{Introducción}

Este módulo implementa operaciones básicas para trabajar con bytes en diferentes 
representaciones. Los bytes pueden representarse en formato binario (como 8 bits) 
o en formato hexadecimal (como 2 dígitos hexadecimales).

@section{Representación de datos}

El módulo utiliza dos representaciones principales para los bytes:

@begin{itemize}
  @item @bf{Bytes binarios}: Representados como listas de 8 bits. Cada bit es una 
        estructura b(0) o b(1).
  @item @bf{Bytes hexadecimales}: Representados como listas de 2 dígitos hexadecimales.
        Cada dígito es una estructura h(X) donde X es un valor entre 0 y f.
@end{itemize}

@section{Funcionalidades}

El módulo implementa las siguientes funcionalidades:

@begin{enumerate}
  @item Verificación de listas de bytes
  @item Conversión entre representaciones binarias y hexadecimales
  @item Obtención de un bit específico de un byte
  @item Desplazamiento circular a la izquierda de listas de bytes
  @item Desplazamiento circular a la derecha de listas de bytes
  @item Operación XOR entre bytes
@end{enumerate}


@section{Ejemplos de uso}

A continuación, se presenta un ejemplo de cada funcionalidad principal del módulo.

@bf{Verificación de lista de bytes:}
@begin{verbatim}
?- byte_list([[h(a),h(5)], [b(1),b(0),b(1),b(0),b(1),b(0),b(1),b(0)]]).
false.
@end{verbatim}

@bf{Conversión entre representaciones:}
@begin{verbatim}
?- byte_convert([h(5),h(a)], BinByte).
BinByte = [b(0),b(1),b(0),b(1),b(1),b(0),b(1),b(0)]
@end{verbatim}

@bf{Conversión de listas de bytes:}
@begin{verbatim}
?- byte_list_convert([[h(a),h(b)],[h(c),h(d)]], BinList).
BinList = [[b(1),b(0),b(1),b(0),b(1),b(0),b(1),b(1)],
           [b(1),b(1),b(0),b(0),b(1),b(1),b(0),b(1)]]
@end{verbatim}

@bf{Obtener bits específicos:}
@begin{verbatim}
?- get_nth_bit_from_byte(s(s(s(0))), [h(5),h(a)], Bit).
Bit = b(0)
@end{verbatim}

@bf{Desplazamiento circular a la izquierda:}
@begin{verbatim}
?- byte_list_clsh([[h(5),h(a)], [h(2),h(3)]], CLShL).
CLShL = [[h(b),h(4)], [h(4),h(6)]]
@end{verbatim}

@bf{Desplazamiento circular a la derecha:}
@begin{verbatim}
?- byte_list_crsh([[h(b),h(4)], [h(4),h(6)]], CRShL).
CRShL = [[h(5),h(a)], [h(2),h(3)]]
@end{verbatim}

@bf{Operación XOR entre bytes:}
@begin{verbatim}
?- byte_xor([h(a),h(5)], [h(5),h(a)], Result).
Result = [h(f),h(f)]
@end{verbatim}
").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(bit/1, "Define el tipo bit binario. Un bit solo puede ser 0 o 1, 
                representado estructuralmente como b(0) o b(1).").

:- regtype bit(B) # "@var{B} es un bit binario representado como b(0) o b(1).".

bit(b(0)).
bit(b(1)).

:- doc(binary_byte/1, "Define un byte binario como una lista ordenada de 8 bits.
                       El primer elemento es el bit más significativo (posición 7),
                       y el último elemento es el bit menos significativo (posición 0).").

:- regtype binary_byte(B) # "@var{B} es una lista de 8 bits, del bit más significativo (posición 7) al menos significativo (posición 0).".

binary_byte([B7 , B6 , B5 , B4 , B3 , B2 , B1 , B0]) :-
    bit(B7),
    bit(B6),
    bit(B5),
    bit(B4),
    bit(B3),
    bit(B2),
    bit(B1),
    bit(B0).

:- doc(hexd/1, "Define un dígito hexadecimal (nibble). Cada dígito hex representa un valor 
                de 4 bits (0-15). Aunque usamos constantes numéricas (0-9) y alfabéticas (a-f),
                se tratan como constantes simbólicas, no como números decimales.").

:- regtype hexd(H) # "@var{H} es un dígito hexadecimal (nibble) representado estructuralmente como h(X).".

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

:- doc(hex_byte/1, "Define un byte hexadecimal como una lista ordenada de 2 dígitos hexadecimales.
                    El primer elemento es el nibble más significativo (posición 1),
                    y el segundo elemento es el nibble menos significativo (posición 0).").

:- regtype hex_byte(B) # "@var{B} es una lista de 2 dígitos hexadecimales, del más significativo (posición 1) al menos significativo (posición 0).".

hex_byte([H1, H0]) :-
    hexd(H1),
    hexd(H0).

:- doc(byte/1, "Define el tipo byte como un byte binario o un byte hexadecimal.
                Esta abstracción permite múltiples representaciones del mismo concepto.").

:- regtype byte(B) # "@var{B} es un byte, ya sea en formato binario o hexadecimal.".

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

:- doc(section, main_predicates).
:- doc(byte_list/1, "Este predicado es cierto cuando L es una lista válida de bytes.
                     Cada elemento de la lista debe ser un byte binario (8 bits) 
                     o un byte hexadecimal (2 nibbles).
La implementación utiliza recursión con dos cláusulas:
 1. Caso base: Una lista vacía es una lista de bytes válida
 2. Caso recursivo: Para una lista no vacía, verifique que el primer elemento
 sea un byte válido y que el resto de la lista sea una lista de bytes válida").

:- pred byte_list(L) # "@var{L} es una lista de bytes (binarios o hexadecimales).
                        Su implementación es: @includedef{byte_list/1}".

byte_list([]).                        
byte_list([B|Bs]) :-                  
    byte(B),
    byte_list(Bs).

%--------------------------------------------
% Predicate 2: byte_convert/2
%--------------------------------------------

:- doc(section, main_predicates).
:- doc(byte_convert/2, "Este predicado es cierto cuando HexByte (en hexadecimal) representa
                        el mismo valor que BinByte (en binario).
La implementación convierte cada dígito hexadecimal en su representación binaria de 4 bits y
luego concatena estos bits para formar el byte binario completo de 8 bits.").

:- pred byte_convert(HexByte, BinByte) :: hex_byte * binary_byte
   # "@var{HexByte} es un byte hexadecimal y @var{BinByte} es su representación binaria equivalente.
      Su implementación es: @includedef{byte_convert/2}".

byte_convert([H1, H0], BinByte) :-
    hex_byte([H1, H0]),
    nibble_to_bits(H1, [B7, B6, B5, B4]),
    nibble_to_bits(H0, [B3, B2, B1, B0]),
    BinByte = [B7, B6, B5, B4, B3, B2, B1, B0],
    binary_byte(BinByte).

%--------------------------------------------
% Predicate 3: byte_list_convert/2
%--------------------------------------------

:- doc(section, main_predicates).
:- doc(byte_list_convert/2, "Este predicado es cierto cuando HexList (una lista de bytes hexadecimales)
                             representa el mismo valor que BinList (una lista de bytes binarios).
La implementación utiliza recursión con dos cláusulas:
 1. Caso base: Una lista vacía de bytes hexadecimales se convierte en una lista vacía de bytes binarios.
 2. Caso recursivo: Para una lista no vacía, se convierte el primer byte hexadecimal a su representación binaria y se convierte recursivamente el resto de la lista.").

:- pred byte_list_convert(HexList, BinList) 
   # "@var{HexList} es una lista de bytes hexadecimales y @var{BinList} es su representación binaria equivalente.
      Su implementación es: @includedef{byte_list_convert/2}".

byte_list_convert([],[]).
byte_list_convert([H|Hs], [B|Bs]) :-
    byte_convert(H, B),
    byte_list_convert(Hs, Bs).

%--------------------------------------------
% Predicate 4: get_nth_bit_from_byte/3
%--------------------------------------------

:- doc(section, main_predicates).
:- doc(get_nth_bit_from_byte/3, "Este predicado es cierto cuando BN es el N-ésimo bit del byte B.

El predicado utiliza dos cláusulas:
 1. Si B es un byte binario, invierte el byte y obtiene el N-ésimo bit
 2. Si B es un byte hexadecimal, lo convierte a binario y luego obtiene el N-ésimo bit

La implementación usa el predicado byte_convert/2 para convertir el byte hexadecimal a binario,
y luego usa reverse_list/2 para invertir el byte binario.
El predicado get_nth_bit/3 se usa para extraer el N-ésimo bit del byte invertido.").

:- pred get_nth_bit_from_byte(N, B, BN) 
   # "@var{BN} es el @var{N}-ésimo bit del byte @var{B}.
      Su implementación es: @includedef{get_nth_bit_from_byte/3}".

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

:- doc(section, main_predicates).
:- doc(byte_list_clsh/2, "Este predicado es cierto cuando CLShL es el resultado de un 
                         desplazamiento circular a la izquierda de la lista de bytes L.

El predicado utiliza dos cláusulas:
1. Si L es una lista de bytes binarios, convertir la lista a bits, realizar el desplazamiento circular a la izquierda y volver a convertir a bytes.
2. Si L es una lista de bytes hexadecimales, convertir la lista a bytes binarios, realizar el desplazamiento circular a la izquierda y volver a convertir a bytes hexadecimales.

La implementación utiliza el predicado bytes_to_bits/2 para convertir la lista de bytes a bits, el predicado rotate_left/2 para realizar el desplazamiento circular a la izquierda y el predicado bits_to_bytes/2 para volver a convertir los bits a bytes.
El predicado byte_list_convert/2 se utiliza para convertir los bytes hexadecimales a bytes binarios.").

:- pred byte_list_clsh(L, CLShL) 
   # "@var{CLShL} es el resultado de aplicar un desplazamiento circular a la izquierda a la lista de bytes @var{L}.
      Su implementación es: @includedef{byte_list_clsh/2}".

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

%--------------------------------------------
% Predicate 6: byte_list_crsh/2
%--------------------------------------------

:- doc(section, main_predicates).
:- doc(byte_list_crsh/2, "Este predicado es cierto cuando CRShL es el resultado de un 
                         desplazamiento circular a la derecha de la lista de bytes L.

El predicado utiliza dos cláusulas:
1. Si L es una lista de bytes binarios, convertir la lista a bits, realizar el desplazamiento circular a la derecha y volver a convertir a bytes.
2. Si L es una lista de bytes hexadecimales, convertir la lista a bytes binarios, realizar el desplazamiento circular a la derecha y volver a convertir a bytes hexadecimales.

La implementación utiliza el predicado bytes_to_bits/2 para convertir la lista de bytes a bits, el predicado rotate_right/2 para realizar el desplazamiento circular a la derecha y el predicado bits_to_bytes/2 para volver a convertir los bits a bytes.
El predicado byte_list_convert/2 se utiliza para convertir los bytes hexadecimales a bytes binarios y viceversa.").

:- pred byte_list_crsh(L, CRShL) 
   # "@var{CRShL} es el resultado de aplicar un desplazamiento circular a la derecha a la lista de bytes @var{L}.
      Su implementación es: @includedef{byte_list_crsh/2}".

byte_list_crsh(L, CRShL):-
   bytes_to_bits(L, AllBits),
   rotate_right(AllBits, RotatedBits),
   bits_to_bytes(RotatedBits, CRShL).
byte_list_crsh(L, CRShL):-
   byte_list_convert(L, BinList),
   bytes_to_bits(BinList, AllBits),
   rotate_right(AllBits, RotatedBits),
   bits_to_bytes(RotatedBits, BinBytes),
   byte_list_convert(CRShL, BinBytes).

%--------------------------------------------
% Predicate 7: byte_xor/3
%--------------------------------------------

:- doc(section, main_predicates).
:- doc(byte_xor/3, "Este predicado es cierto cuando B3 es el resultado de la operación XOR entre B1 y B2.

El predicado utiliza dos cláusulas:
1. Si B1 y B2 son bytes binarios, invertir ambos bytes, realizar la operación XOR e invertir el resultado.
2. Si B1 y B2 son bytes hexadecimales, convertirlos a binarios, realizar la operación XOR y volver a convertir el resultado a hexadecimal.

La implementación utiliza byte/1 para comprobar que B1 y B2 son bytes válidos, el predicado byte_convert/2 para convertir el byte hexadecimal a binario y viceversa,
el predicado reverse_list/2 para invertir los bytes y el predicado xor_list/3 para realizar la operación XOR en bits individuales.").

:- pred byte_xor(B1, B2, B3) 
   # "@var{B3} es el resultado de la operación XOR entre @var{B1} y @var{B2}.
      Su implementación es: @includedef{byte_xor/3}".

byte_xor([], [], []).
byte_xor(B1, B2, B3):-
   byte(B1),
   byte(B2),
   reverse_list(B1, RevB1),
   reverse_list(B2, RevB2),
   xor_list(RevB1, RevB2, RevB3),
   reverse_list(RevB3, B3).
byte_xor(B1, B2, B3):-
   byte(B1),
   byte(B2),
   byte_convert(B1, BinB1),
   byte_convert(B2, BinB2),
   reverse_list(BinB1, RevBinB1),
   reverse_list(BinB2, RevBinB2),
   xor_list(RevBinB1, RevBinB2, RevBinB3),
   reverse_list(RevBinB3, RevB3),
   byte_convert(B3, RevB3).   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AUXILIARY PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------
% Auxiliaries for byte_convert/2
%--------------------------------------------

:- doc(section, auxiliary_predicates).
:- doc(nibble_to_bits/2, "Este predicado convierte un dígito hexadecimal (nibble) a su 
                          representación binaria de 4 bits.").

:- pred nibble_to_bits(Nibble, Bits) 
   # "@var{Bits} es la lista de 4 bits que representa al nibble hexadecimal @var{Nibble}.".

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

:- doc(section, auxiliary_predicates).
:- doc(reverse_list/2, "Este predicado es cierto cuando Reversed es la inversión de List.").

:- pred reverse_list(List, Reversed) 
   # "@var{Reversed} es la inversión de la lista @var{List}.
      Su implementación es: @includedef{reverse_list/2}".

reverse_list(List, Reversed) :-
    reverse_acc(List, [], Reversed).

:- doc(section, auxiliary_predicates).
:- doc(reverse_acc/3, "El predicado es cierto cuando Reversed es la concatenación de Acc y List invertida.

La implementación utiliza recursión de cola con dos cláusulas:
 1. Caso base: Una lista vacía invertida es el acumulador actual
 2. Caso recursivo: Para una lista no vacía, se añade el primer elemento al inicio del
    acumulador y se invierte recursivamente el resto de la lista.").

:- pred reverse_acc(List, Acc, Reversed) :: list * list * list
   # "@var{Reversed} es el resultado de invertir la lista @var{List} usando @var{Acc} como acumulador.
      Su implementación es: @includedef{reverse_acc/3}".

reverse_acc([], Acc, Acc).
reverse_acc([X|Xs], Acc, Reversed) :-
    reverse_acc(Xs, [X|Acc], Reversed).

:- doc(section, auxiliary_predicates).
:- doc(get_nth_bit/3, "Este predicado es cierto cuando BN es el N-ésimo bit del byte invertido Bits.

La implementación utiliza recursión con dos cláusulas:
 1. Caso base: Si N es 0, el primer elemento de Bits es el bit deseado.
 2. Caso recursivo: Para un N distinto de cero, decrementar N y
 llamar recursivamente al predicado con la cola de la lista (Bits) para encontrar el siguiente bit.").

:- pred get_nth_bit(N, Bits, BN) 
   # "@var{BN} es el @var{N}-ésimo bit de la lista de bits @var{Bits}.
      Su implementación es: @includedef{get_nth_bit/3}".

get_nth_bit(0, [BN|_], BN).
get_nth_bit(s(N), [_|Bits], BN) :-
    get_nth_bit(N, Bits, BN).

:- doc(section, auxiliary_predicates).
:- doc(my_append/3, "Este predicado es cierto cuando Result es la concatenación de List1 y List2.

La implementación utiliza recursión con dos cláusulas:
 1. Caso base: Una lista vacía concatenada con otra lista es la segunda lista.
 2. Caso recursivo: Para una lista no vacía, anteponga el encabezado de Lista1 al resultado y concatene recursivamente el final de Lista1 con Lista2.").

:- pred my_append(List1, List2, Result) 
   # "@var{Result} es la concatenación de @var{List1} y @var{List2}.
      Su implementación es: @includedef{my_append/3}".

my_append([], L, L).
my_append([H|T], L, [H|R]) :-
   my_append(T, L, R).

%---------------------------------------------
% Auxiliaries for byte_list_clsh/2
%---------------------------------------------

:- doc(section, auxiliary_predicates).
:- doc(bytes_to_bits/2, "Este predicado convierte una lista de bytes a una lista plana de bits.

La implementación utiliza recursión con dos cláusulas:
1. Caso base: Una lista vacía de bits se convierte en una lista vacía de bytes.
2. Caso recursivo: Para una lista no vacía, se convierten los primeros 8 bits en un byte y el resto de los bits en bytes recursivamente.

La implementación utiliza el predicado binary_byte/1 para comprobar que los primeros 8 bits formen un byte binario válido.
El predicado my_append/3 se utiliza para concatenar los bits en un solo byte.").

:- pred bytes_to_bits(Bytes, Bits) 
   # "@var{Bits} es la lista plana de bits correspondiente a la lista de bytes @var{Bytes}.
      Su implementación es: @includedef{bytes_to_bits/2}".

bytes_to_bits([], []).
bytes_to_bits([B|Bs], Bits) :-
   binary_byte(B),
   bytes_to_bits(Bs, RestBits),
   my_append(B, RestBits, Bits).

:- doc(section, auxiliary_predicates).
:- doc(rotate_left/2, "Este predicado realiza una rotación circular a la izquierda de una lista.

% La implementación utiliza dos cláusulas:
% 1. Una lista vacía se rota a una lista vacía
% 2. Para una lista no vacía, se añade el final de la lista al principio y se devuelve la lista rotada, utilizando el predicado my_append/3.").

:- pred rotate_left(List, Rotated) 
   # "@var{Rotated} es el resultado de rotar la lista @var{List} un lugar a la izquierda.
      Su implementación es: @includedef{rotate_left/2}".

rotate_left([], []).
rotate_left([H|T], Rotated) :-
   my_append(T, [H], Rotated).

:- doc(section, auxiliary_predicates).
:- doc(bits_to_bytes/2, "Este predicado convierte una lista plana de bits a una lista de bytes.

La implementación utiliza recursión con dos cláusulas:
1. Caso base: Una lista vacía de bits se convierte en una lista vacía de bytes.
2. Caso recursivo: Para una lista no vacía, se convierten los primeros 8 bits en un byte y el resto de los bits se convierten recursivamente en bytes.

El predicado my_append/3 se utiliza para concatenar los bits en un solo byte.").

:- pred bits_to_bytes(Bits, Bytes) 
   # "@var{Bytes} es la lista de bytes formada a partir de la lista de bits @var{Bits}.
      Su implementación es: @includedef{bits_to_bytes/2}".

bits_to_bytes([], []).
bits_to_bytes([B1, B2, B3, B4, B5, B6, B7, B8|RestBits], [H|RestBytes]) :-
   my_append([B1, B2, B3, B4], [B5, B6, B7, B8], H),
   bits_to_bytes(RestBits, RestBytes).

%---------------------------------------------
% Auxiliaries for byte_list_crsh/2
%---------------------------------------------

:- doc(section, auxiliary_predicates).
:- doc(rotate_right/2, "Este predicado realiza una rotación circular a la derecha de una lista.

El predicado reverse_list/2 se usa para invertir la lista,
y luego el predicado rotate_left/2 se usa para realizar el desplazamiento circular a la izquierda en la lista invertida.
Finalmente, el predicado reverse_list/2 se usa nuevamente para invertir la lista rotada a su orden original.").

:- pred rotate_right(List, Rotated) 
   # "@var{Rotated} es el resultado de rotar la lista @var{List} un lugar a la derecha.
      Su implementación es: @includedef{rotate_right/2}".

rotate_right(L, Rotated) :-
   reverse_list(L, Reversed),
   rotate_left(Reversed, RotatedReversed),
   reverse_list(RotatedReversed, Rotated).

%---------------------------------------------
% Auxiliaries for byte_xor/3
%---------------------------------------------

:- doc(section, auxiliary_predicates).
:- doc(xor_list/3, "Este predicado realiza la operación XOR bit a bit entre dos listas de bits.

La implementación utiliza recursión con dos cláusulas:
1. Caso base: Una lista vacía que se aplica mediante una operación XOR con otra lista vacía es una lista vacía.
2. Caso recursivo: Para una lista no vacía, se realiza la operación XOR en los primeros elementos de ambas listas y se aplica recursivamente la operación XOR al resto de las listas.

El predicado xor_bits/3 se utiliza para realizar la operación XOR en bits individuales.").

:- pred xor_list(List1, List2, Result) 
   # "@var{Result} es el resultado de aplicar la operación XOR bit a bit entre @var{List1} y @var{List2}.
      Su implementación es: @includedef{xor_list/3}".

xor_list([], [], []).
xor_list([B1|Bs1], [B2|Bs2], [B3|Bs3]):-
   xor_bits(B1, B2, B3),
   xor_list(Bs1, Bs2, Bs3).

:- doc(section, auxiliary_predicates).
:- doc(xor_bits/3, "Este predicado realiza la operación XOR entre dos bits.").

:- pred xor_bits(B1, B2, B3) 
   # "@var{B3} es el resultado de la operación XOR entre los bits @var{B1} y @var{B2}.
      Su implementación es: @includedef{xor_bits/3}".

xor_bits(b(0),b(0),b(0)).
xor_bits(b(0),b(1),b(1)).
xor_bits(b(1),b(0),b(1)).
xor_bits(b(1),b(1),b(0)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------
% Tests for byte_list/1
%--------------------------------------------

% Test empty list
:- test byte_list(L) : (L = []) + not_fails.     
% Test list with a single binary byte
:- test byte_list(L) : (L = [[h(0), h(5)], [h(6), h(f)]]) + not_fails.
% Test list with a single binary byte
:- test byte_list(L) : (L = [[b(1), b(0), b(1), b(0), b(0), b(1), b(0), b(1)]]) + not_fails.
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
% Tests for byte_convert/2
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
% Tests for byte_list_convert/2
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
:- test byte_list_convert(HL, BL) : (HL = [[h(a), h(b)], [h(c)]]) + fails.
% Test bidirectionality: determine hex bytes from binary bytes
:- test byte_list_convert(HL, BL) : (BL = [[b(0), b(1), b(0), b(1), b(1), b(0), b(1), b(0)], [b(1), b(0), b(0), b(1), b(0), b(1), b(1), b(0)]]) 
   => (HL = [[h(5), h(a)], [h(9), h(6)]]) + not_fails.

%--------------------------------------------
% Tests for get_nth_bit_from_byte/3
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
:- test get_nth_bit_from_byte(N, B, Bit) : (B = [h(a), h(5)]) 
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
:- test get_nth_bit_from_byte(N, B, Bit) : (N = s(s(s(0))), Bit = b(1)) 
   + not_fails.

%--------------------------------------------
% Tests for byte_list_clsh/2
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
:- test byte_list_clsh(L, CLShL) : (L = [[h(a),h(5)], [b(1),b(0),b(1),b(0),b(1),b(0),b(1),b(0)]]) + fails.

%--------------------------------------------
% Tests for byte_list_crsh/2
%--------------------------------------------

% Test empty list
:- test byte_list_crsh(L, CRShL) : (L = []) 
   => (CRShL = []) + not_fails.
% Test single binary byte
:- test byte_list_crsh(L, CRShL) : (L = [[b(1),b(0),b(1),b(0),b(1),b(0),b(1),b(0)]]) 
   => (CRShL = [[b(0),b(1),b(0),b(1),b(0),b(1),b(0),b(1)]]) + not_fails.
% Test single hex byte
:- test byte_list_crsh(L, CRShL) : (L = [[h(5),h(a)]]) 
   => (CRShL = [[h(2),h(d)]]) + not_fails.
% Test multiple binary bytes
:- test byte_list_crsh(L, CRShL) : (L = [[b(1),b(0),b(1),b(0),b(1),b(0),b(1),b(0)], [b(1),b(1),b(0),b(0),b(1),b(1),b(0),b(0)]]) 
   => (CRShL = [[b(0),b(1),b(0),b(1),b(0),b(1),b(0),b(1)], 
                [b(0),b(1),b(1),b(0),b(0),b(1),b(1),b(0)]]) + not_fails.
% Test with the example from the statement
:- test byte_list_crsh(L, CRShL) : (L = [[h(b), h(4)], [h(4), h(6)], [h(a), h(a)],[h(6), h(e)]])
   => (CRShL = [[h(5),h(a)], [h(2),h(3)], [h(5),h(5)], [h(3),h(7)]]) + not_fails.
% Test with a binary byte all zeros (keep it the same)
:- test byte_list_crsh(L, CRShL) : (L = [[b(0),b(0),b(0),b(0),b(0),b(0),b(0),b(0)]]) 
   => (CRShL = [[b(0),b(0),b(0),b(0),b(0),b(0),b(0),b(0)]]) + not_fails.
% Test with an alternating hex byte (FF)
:- test byte_list_crsh(L, CRShL) : (L = [[h(f),h(f)]]) 
   => (CRShL = [[h(f),h(f)]]) + not_fails.
% Test with mixed entry types (should fail)
:- test byte_list_crsh(L, CRShL) : 
   (L = [[h(a),h(5)], [b(1),b(0),b(1),b(0),b(1),b(0),b(1),b(0)]]) + fails.

%----------------------------------------------
% Tests for byte_xor/3
%----------------------------------------------
% Test empty list
:- test byte_xor(B1, B2, B3) : (B1 = [], B2 = []) 
   => (B3 = []) + not_fails.
% Test with the example form the statement
:- test byte_xor(B1, B2, B3) : (B1 = [h(5), h(a)], B2 = [h(2), h(3)]) 
   => (B3 = [h(7), h(9)]) + not_fails.
% Test with two binary bytes
:- test byte_xor(B1, B2, B3) : (B1 = [b(1), b(0), b(1), b(0), b(1), b(0), b(1), b(0)], B2 = [b(0), b(1), b(0), b(1), b(0), b(1), b(0), b(1)]) 
   => (B3 = [b(1), b(1), b(1), b(1), b(1), b(1), b(1), b(1)]) + not_fails.
% Test with two hex bytes
:- test byte_xor(B1, B2, B3) : (B1 = [h(a), h(a)], B2 = [h(5), h(5)]) 
   => (B3 = [h(f), h(f)]) + not_fails.
% Test with property of XOR: A XOR A = 0
:- test byte_xor(B1, B2, B3) : (B1 = [h(a), h(a)], B2 = [h(a), h(a)]) 
   => (B3 = [h(0), h(0)]) + not_fails.
% Test with property of XOR: A XOR 0 = A
:- test byte_xor(B1, B2, B3) : (B1 = [h(c), h(d)], B2 = [h(0), h(0)]) 
   => (B3 = [h(c), h(d)]) + not_fails.
% Test with binary bytes out of order (should fail)
:- test byte_xor(B1, B2, B3) : (B1 = [b(1), b(0), b(1), b(0), b(1), b(0), b(1), b(0)], B2 = [b(1), b(0), b(1), b(0), b(1), b(0), b(1)]) 
   + fails.
% Test with mixed types (should fail)
:- test byte_xor(B1, B2, B3) : (B1 = [h(a), h(a)], B2 = [b(1), b(0), b(1), b(0), b(1), b(0), b(1), b(0)]) 
   + fails.
% Test bidirectionality (given B1 and B3, find B2)
:- test byte_xor(B1, B2, B3) : (B1 = [h(5), h(5)], B3 = [h(a), h(a)]) 
   => (B2 = [h(f), h(f)]) + not_fails.