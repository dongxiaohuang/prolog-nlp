%%%%% Natural Language Program

sentence(S) :-
	noun_phrase(NP),
	verb_phrase(VP),
	append(NP, VP, S).

noun_phrase(NP) :-
	article(A),
	noun(N),
	append(A, N, NP).

verb_phrase(V) :-
	verb(V).
verb_phrase(VP) :-
	verb(V),
	noun_phrase(NP),
	append(V, NP, VP).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* The required predicates and argument positions are:

	a.  conj(Text)
	b.  encode(Text, EncodedText)
	c.  same_actor(Text)

*/

conj(Test) :-
	append(FirstS,[and|RestT],Test),
	sentence(FirstS),
	conj(RestT).
conj(Test) :- sentence(Test),\+member(and,Test).

%encode()
to_conn_code([as|c],[asc]).
to_conn_code([as|b],[asb]).
to_conn_code([al|g],[alg]).
to_conn_code([dl|g],[dlg]).
to_conn_code([dl|s],[dls]).

encoder(Text,Acc,EncodedText) :-
	Text = [Word|Rest],
	noun([Word]),
	animate(Ani),
	member(Word,Ani),
	atom_chars(Word,SpitWord),
	length(SpitWord,Len),
	Len > 3,!,
	SpitWord = [FirstC|RestC],
	EncodedList = [al|FirstC],
	to_conn_code(EncodedList,EncodedW),
	append(Acc,EncodedW,AccRes),
	encoder(Rest,AccRes,EncodedText).

encoder(Text,Acc,EncodedText) :-
	Text = [Word|Rest],
	noun([Word]),
	animate(Ani),
	member(Word,Ani),
	atom_chars(Word,SpitWord),
	length(SpitWord,Len),
	Len < 4,
	SpitWord = [FirstC|RestC],
	EncodedList = [as|FirstC],
	to_conn_code(EncodedList,EncodedW),
	append(Acc,EncodedW,AccRes),
	encoder(Rest,AccRes,EncodedText).
	encoder(Text,Acc,EncodedText) :-
		Text = [Word|Rest],
		noun([Word]),
		atom_chars(Word,SpitWord),
		length(SpitWord,Len),
		Len > 3,
		SpitWord = [FirstC|RestC],
		EncodedList = [dl|FirstC],
		to_conn_code(EncodedList,EncodedW),
		append(Acc,EncodedW,AccRes),
		encoder(Rest,AccRes,EncodedText).
encoder(Text,Acc,EncodedText) :-
	Text = [Word|Rest],
	\+noun([Word]),
	append(Acc,[Word],AccRes),
	encoder(Rest,AccRes,EncodedText).
encode(Text,EncodedText) :- encoder(Text,[],EncodedText).
encoder([],Acc,Acc).


%smae_actor(Text)
same_actor(Text) :-
	conj(Text),
	same_actor_acc(Text,Actor).
%base case
same_actor_acc(Text, Actor) :-
	append([the],[Actor|Rest],Text),
	\+ member(and,Text).
%
same_actor_acc(Text, Actor) :-
	append([the],[Actori|Rest],Text),
	member(Actori,[Actor]),
	append(F,[and|ResS],Text),
	same_actor_acc(ResS, Actor).
