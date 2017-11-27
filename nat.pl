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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%(a)%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% conj(Test) means Test is the conjunction of sentences,
% and each two adjacent sentences are separated by the word “and”.
% A conjunction of sentences can have one or more sentences.
% FirsS means the first sentence
% ResrT means the rest sentence

% base case when the Test is only one sentence or without 'and' in the Test.
conj(Test) :- sentence(Test),\+member(and,Test).

% recursive rules
conj(Test) :-
	append(FirstS,[and|RestT],Test),
	sentence(FirstS),
	conj(RestT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%(b)%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%encode(Test,EncodedText) means Test is some text (list of words, not necessarily a sentence or a conjunction of sentences)
% and EncodedText is an encoding of Test such that
% every noun N in Test is replaced by a constant C constructed as follows.
%C is 3 characters long and is made up as follows:

%• The ﬁrst character of C is ‘a’ if the noun is animate, otherwise it is ‘d’.
%• The second character of C is ‘l’ (for long) if N is longer than 3 letters and ‘s’ (for short) otherwise.
%• The third character of C is the ﬁrst letter of N.

%--main rule--%
% encode(Text,EncodedText) rules, is the when the Accumulator List of encoder is []

encode(Text,EncodedText) :- encoder(Text,[],EncodedText).

%--helper--%
% encoder(Text,Acc,EncodedText) is an encoder which encode the Text word by word
% and append it to the Accumulator(Acc), then when all the words in the Text are Encoded
% which means that the Text is [], then the Accumulator is the EncodedText.

% base case when all the words in Text are encoded.
encoder([],Acc,Acc).

% encoder rule when a word in Text is noun and animate, and the length of the word is large then
% it would be encoded in the rule.

encoder(Text,Acc,EncodedText) :-
	Text = [Word|Rest],
	animate_noun([Word]),
	atom_chars(Word,SpitWord),
	long_len(SpitWord),
	start_encode([a,l],SpitWord,EncodedList,EncodedW),
	append(Acc,[EncodedW],AccRes),
	encoder(Rest,AccRes,EncodedText).

% encoder rule when a word in Text is noun and animate, and the length of the word is short then
% it would be encoded in the rule.

encoder(Text,Acc,EncodedText) :-
	Text = [Word|Rest],
	animate_noun([Word]),
	atom_chars(Word,SpitWord),
	short_len(SpitWord),
	start_encode([a,s],SpitWord,EncodedList,EncodedW),
	append(Acc,[EncodedW],AccRes),
	encoder(Rest,AccRes,EncodedText).

% encoder rule when a word in Text is a noun but not an animate, and the length of the word is long then
% it would be encoded in the rule.

encoder(Text,Acc,EncodedText) :-
	Text = [Word|Rest],
	non_animate_noun([Word]),
	atom_chars(Word,SpitWord),
	long_len(SpitWord),
	start_encode([d,l],SpitWord,EncodedList,EncodedW),
	append(Acc,[EncodedW],AccRes),
	encoder(Rest,AccRes,EncodedText).

% encoder rule when a word in Text is a noun but not an animate, and the length of the word is short then
% it would be encoded in the rule.

encoder(Text,Acc,EncodedText) :-
	Text = [Word|Rest],
	non_animate_noun([Word]),
	atom_chars(Word,SpitWord),
	short_len(SpitWord),
	start_encode([d,s],SpitWord,EncodedList,EncodedW),
	append(Acc,[EncodedW],AccRes),
	encoder(Rest,AccRes,EncodedText).
% encoder rule when a word in Text is not a noun, then the EncodedText is the original word

encoder(Text,Acc,EncodedText) :-
	Text = [Word|Rest],
	\+noun([Word]),
	append(Acc,[Word],AccRes),
	encoder(Rest,AccRes,EncodedText).

% helper function animate_noun([Word|R]) to check if a Word is an animate and a noun
% Word means the Word and R means the rest

animate_noun([Word|R]) :-
	noun([Word]),
	animate(Ani),
	member(Word,Ani).

% helper function non_animate_noun([Word|R]) to check if a Word is not an animate but is a noun
% Word means the Word and R means the rest

non_animate_noun([Word|R]) :-
	noun([Word]),
	animate(Ani),
	\+ member(Word,Ani).

% helper function long_len(List) to check if the word splited into List,
% and the length of the word is bigger than 3

long_len(List) :-
	length(List,Len),
	Len > 3.

% helper function short_len(List) to check if the word splited into List,
% and the length of the word is less than 4

short_len(List) :-
	length(List,Len),
	Len < 4.

% helper function start_encode(FirstTwoCh,SpitWord,EncodedList,EncodedW) to encode the word
% FirstTwoCh means the first two encoded characters
% SpitWord means the List made of split the Word which need to be encoded
% EncodedList means the encoded list
% EncodedW means the encoded word which is connected by the encoded list EncodedList

start_encode(FirstTwoCh,SpitWord,EncodedList,EncodedW) :-
	SpitWord = [FirstC|RestC],
	append(FirstTwoCh,[FirstC],EncodedList),
	atom_chars(EncodedW,EncodedList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%(c)%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% same_actor(Text) means when all the sentences in the conjunction of sentences Text
% refer to the same actor.
% An actor in a sentence is the noun immediately preceding the verb in the sentence.

% rules for same_actor(Text), Text must be conjunction of sentences and with the same actor.
same_actor(Text) :-
	conj(Text),
	same_actor_acc(Text,Actor).

% helper function same_actor_acc(Text, Actor)
% Text is the conjunction sentences
% Actor is the actor of the last sentence in the conjunction sentences

% base case when the Text is the last sentence in the conjunction sentence which means there is no
% 'and' in the sentence, then Actor is assigned.

same_actor_acc(Text, Actor) :-
	append([the],[Actor|Rest],Text),
	\+ member(and,Text).

% recursive case , check if the current actor is the same with the last actor using the function member
% Actori is the actor of the current sentence

same_actor_acc(Text, Actor) :-
	append([the],[Actori|Rest],Text),
	member(Actori,[Actor]),
	append(F,[and|ResS],Text),
	same_actor_acc(ResS, Actor).
