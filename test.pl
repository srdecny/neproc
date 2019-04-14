
parsuj_json(String,V):-
	string_chars(String,SeznamCharu),
	pjson(V,SeznamCharu,[]).

pjson(V)-->
	pnull(V);
	pbool(V);
	pseznam(V);
	pString(V);
	pint(V).

pnull(nic) --> [n,u,l,l].

pbool(ano) --> [t,r,u,e].
pbool(ne) --> [f,a,l,s,e].

pseznam(V) --> 
	['['], pvs(V), [']']; 
	['[', ']'], {V=[]}.

pvs(V) --> 
	pjson(Vj) , {V = [Vj]}; 
	pjson(Vj), [','], pvs(Vs), {V=[Vj|Vs]}.

pString(str(V)) -->
	['"'], pstr(V), ['"'].

pstr(V) -->
	[] , {V=''};
	[C], {C \= '"'}, pstr(Z), {atom_concat(C,Z,V)}.

pint(A, A) --> []. 
pint(V, A) --> [CD], {atom_number(CD, D), A1 is A*10 + D}, pint(V, A1).

%dodatek: integer s aspon jednim cislem
pint(int(V)) --> [CD], {atom_number(CD, D)}, pint(V, D).