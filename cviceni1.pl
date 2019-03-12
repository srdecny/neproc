
% rika(+pohlavi, +SeznamTvrzeni, -SeznamPravdivychTvrzeni)=
rika(muz, SeznamTvrzeni, SeznamTvrzeni).

rika(zena, SeznamTvrzeni, SeznamPravdivychTvrzeni) :- sudePolozky(SeznamTvrzeni, SeznamPravdivychTvrzeni); lichePolozky(SeznamTvrzeni, SeznamPravdivychTvrzeni).

% sudePolozky(+seznam, -sudePolozky)
% vrati seznam, obsahujici polozky prvniho seznamku se sudym indexem (0, 2, ...)
sudePolozky([], []).
sudePolozky([X, _ | L], [X | R]) :- sudePolozky(L, R).

% lichePolozky(+seznam, -lichePolozky)
% vrati seznam, obsahujici polozky prvniho seznamku s lichym indexem (1, 3, ...)
lichePolozky([], []).
lichePolozky([_, X | L], [X | R]) :- lichePolozky(L, R).

% overVyroky(+seznam)
% zavola vsechny vyroky v seznamu
overVyroky([]).
overVyroky([H|T]) :- call(H), overVyroky(T).

% neosetrujeme pripad, kdy jsou oba rodice stejneho pohlavi; i domorodci z dzungle mohou adoptovat deti.
reseniWorfovaProblemu(PohlaviDitete, PohlaviRodice1, PohlaviRodice2) :- rika(Dite, [Dite=PohlaviDitete], DitePravda), % Dite rika, jakeho je pohlavi
                                                                        rika(PohlaviRodice1, [rika(Dite, [muz = PohlaviDitete], _)], Rodic1Pravda), % "Dite reklo, ze je chlapec"
                                                                        rika(PohlaviRodice2, [
                                                                            PohlaviDitete=zena, % "Dite je zena"
                                                                            DitePravda=[]    % "Dite lhalo"
                                                                        ], 
                                                                        Rodic2Pravda),
                                                                        overVyroky(Rodic1Pravda),
                                                                        overVyroky(Rodic2Pravda),
                                                                        print("[Jake pohlavi reklo dite = Jakeho pohlavi dite skutecne je] => "),
                                                                        print(DitePravda).