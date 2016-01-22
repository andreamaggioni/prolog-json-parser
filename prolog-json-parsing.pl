/*
	Autori:
		Andrea Maggioni
		Paolo Ripamonti
	Parser prolog di stringhe JSON
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Predicato che effettua il parsing della JsonString nei termini prolog
json_parse(JsonString, JsonTerm) :-
    atom_chars(JsonString, JsonChars),
    phrase(object(JsonTerm), JsonChars).

%DCG che rappresenta un generico object
object(Term) -->
    ws,
    ['{'],
    !,
    json_object(Term).

%rappresenta un generico object
object(Term) -->
	ws,
	['['],
	!,
	json_array(Term).

object(Term) -->
	string(Term),
    !.

object(Term) -->
    number(Term),
    !.

%rapprenseta un json_object formato da una almeno un attributo
json_object(json_object(Members)) -->
    members(Members),
    ['}'],
    ws,
    !.

%rappresenta un json_object vuoto.
json_object(json_object([])) -->
    ws,
    ['}'],
    ws,
    !.

%rappresenta una serie di attributi
members([Pair|Members]) -->
    pair(Pair),
    [','],
    !,
    members(Members).

%rappresenta un attributo
members([Pair]) -->
    pair(Pair).

%rappresenta una coppia chiave : valore
pair(Key','Value) -->
    ws,
    string(Key),
    ws,
    [':'],
    ws,
    value(Value),
    ws.

%rapprensenta un json_array contentente almeno un elemento
json_array(json_array(Elements)) -->
    elements(Elements),
    ws,
    [']'],
    !.

%rapprensenta un json_array vuoto
json_array(json_array([])) -->
    ws,
    [']'],
    !.

%rappresenta gli elementi di un json_array
elements([Element|Elements]) -->
    value(Element),
    ws,
    [','],
    !,
    ws,
    elements(Elements).

elements([Element]) -->
    value(Element).

%rapprensenta un generico Value
value(Value) -->
    string(Value),
    !.

value(Value) -->
    number(Value),
    !.

value(Value) -->
    object(Value),
    !.

%rapprensenta un numero intero
number(Number) -->
    digits(Chars),
	{Chars \= []},
    {number_chars(Number, Chars)}.

%rapprensenta una serie di digit che rapprensentano un intero
digits([Digit|Digits]) -->
    digit(Digit),
	!,
    digits(Digits).

digits([]) -->
	[].

%rapprensenta il singolo digit, verificando se si tratta di un numero
digit(Digit) -->
    [Digit],
    {char_type(Digit, digit)}.

%rapprensenta una stringa formata da "caratteri*"
string(Value) -->
    ['"'],
	chars(Value),
    ['"'].

%rappresenta una serie di caratteri
chars(Atom) -->
    chars_aux(Chars),
    {atom_chars(Atom, Chars)}.

chars_aux([Char|Chars]) -->
    ['\\'],
    !,
    escape_sequence(Char),
    chars_aux(Chars).

chars_aux([Char|Chars]) -->
    char(Char),
    !,
    chars_aux(Chars).

chars_aux([]) --> [].

%verifica se il char in oggetto è un carattere di escade
escape_sequence(RealChar) -->
    [Char],
    {valid_escape_char(Char, RealChar)}.

%rapprensenta il singolo char
char(Char) -->
    [Char],
    {valid_char(Char)}.

%rapprensenta lo spazio
ws -->
    space_char,
    !,
    ws.
ws --> [].

%rappresenta i caratteri di spaziatura
space_char -->
    [Char],
    {char_type(Char, space)}.

valid_escape_char('"',  '"').
valid_escape_char('\\', '\\').
valid_escape_char('/',  '/').
valid_escape_char('b',  '\b').
valid_escape_char('f',  '\f').
valid_escape_char('n',  '\n').
valid_escape_char('r',  '\r').
valid_escape_char('t',  '\t').

valid_char(Char) :-
    Char \== '"'.

%caso base, se cerco qualcosa in un oggetto vuoto, ritorno falso
json_dot(json_object([]), _, _) :-
	false.

%ho trovato un primo elemento, ora richieamo json_dot con se stesso come input
%e la restante catena di campi da leggere
json_dot(json_object([(Field, Value)|_]), [Field|OtherFields], Result):-
	!,
	json_dot(Value, OtherFields, Result).

%ho un valore, e la lista della catena è vuota vuol dire che ho letto tutto
json_dot(Value, [], Value) :-
	!.

%richiamo json_dot sugli altri elementi della lista
json_dot(json_object([_ | OtherMembers]), Fields, Result) :-
	!,
	json_dot(json_object(OtherMembers), Fields, Result).

json_dot(json_array([]), _, _) :-
	false.

%json_dot per json_array, mi faccio dare l'elemento di posto Index
%richiamo json_dot su questo elemento passandogli il resto della catena
json_dot(json_array(Elements), [Index | OtherFields], Result) :-
	get_element_at(Elements, Index, Element),
	json_dot(Element, OtherFields, Result).

%restituisce l'elemento di posto Index, false se non lo trova.
get_element_at(Elements, Index, Result) :-
	get_element_at(Elements, 0, Index, Result).

get_element_at([],_,_,_) :-
	false.

get_element_at([Head|_], Count, Count, Result) :-
	!,
	Result = Head.

get_element_at([_|Tail], Count, Out, Result) :-
	Count1 is Count + 1,
	get_element_at(Tail, Count1, Out, Result).
