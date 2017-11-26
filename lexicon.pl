article([the]).

noun([grass]).
noun([cow]).
noun([boy]).
noun([girl]).
noun([song]).

verb([eats]).
verb([chews]).
verb([kicks]).
verb([sings]).

animate([cow, boy, girl]).

%turn the encode list into
to_conn_code([as|c],[asc]).
to_conn_code([as|b],[asb]).
to_conn_code([al|g],[alg]).
to_conn_code([dl|g],[dlg]).
to_conn_code([dl|s],[dls]).
