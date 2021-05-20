:-dynamic piece_position/2.
:-dynamic site/1.

% pola
square(a1,1,1).
square(a2,1,2).
square(a3,1,3).
square(a4,1,4).
square(a5,1,5).
square(a6,1,6).
square(a7,1,7).
square(a8,1,8).
square(b1,2,1).
square(b2,2,2).
square(b3,2,3).
square(b4,2,4).
square(b5,2,5).
square(b6,2,6).
square(b7,2,7).
square(b8,2,8).
square(c1,3,1).
square(c2,3,2).
square(c3,3,3).
square(c4,3,4).
square(c5,3,5).
square(c6,3,6).
square(c7,3,7).
square(c8,3,8).
square(d1,4,1).
square(d2,4,2).
square(d3,4,3).
square(d4,4,4).
square(d5,4,5).
square(d6,4,6).
square(d7,4,7).
square(d8,4,8).
square(e1,5,1).
square(e2,5,2).
square(e3,5,3).
square(e4,5,4).
square(e5,5,5).
square(e6,5,6).
square(e7,5,7).
square(e8,5,8).
square(f1,6,1).
square(f2,6,2).
square(f3,6,3).
square(f4,6,4).
square(f5,6,5).
square(f6,6,6).
square(f7,6,7).
square(f8,6,8).
square(g1,7,1).
square(g2,7,2).
square(g3,7,3).
square(g4,7,4).
square(g5,7,5).
square(g6,7,6).
square(g7,7,7).
square(g8,7,8).
square(h1,8,1).
square(h2,8,2).
square(h3,8,3).
square(h4,8,4).
square(h5,8,5).
square(h6,8,6).
square(h7,8,7).
square(h8,8,8).

%linie
rank(1).
rank(2).
rank(3).
rank(4).
rank(5).
rank(6).
rank(7).
rank(8).

%zmienne potrzebne do obliczenia ruchu króla
king_diff(1).
king_diff(0).
king_diff(-1).

%figury
%nazwa,rodzaj,biale/czarne, nazwa wyswietlana, punkty
piece(wp1, pawn, white, 'P', 1).
piece(wp2, pawn, white, 'P', 1).
piece(wp3, pawn, white, 'P', 1).
piece(wp4, pawn, white, 'P', 1).
piece(wp5, pawn, white, 'P', 1).
piece(wp6, pawn, white, 'P', 1).
piece(wp7, pawn, white, 'P', 1).
piece(wp8, pawn, white, 'P', 1).
piece(wn1, knight, white, 'N', 3).
piece(wn2, knight, white, 'N', 3).
piece(wb1, bishop, white, 'L', 3).
piece(wb2, bishop, white, 'L', 3).
piece(wr1, rook, white, 'R', 5).
piece(wr2, rook, white, 'R', 5).
piece(wq, queen, white, 'Q', 9).
piece(wk, king, white, 'K', 100).

piece(bp1, pawn, black, 'p', 1).
piece(bp2, pawn, black, 'p', 1).
piece(bp3, pawn, black, 'p', 1).
piece(bp4, pawn, black, 'p', 1).
piece(bp5, pawn, black, 'p', 1).
piece(bp6, pawn, black, 'p', 1).
piece(bp7, pawn, black, 'p', 1).
piece(bp8, pawn, black, 'p', 1).
piece(bn1, knight, black, 'n', 3).
piece(bn2, knight, black, 'n', 3).
piece(bb1, bishop, black, 'l', 3).
piece(bb2, bishop, black, 'l', 3).
piece(br1, rook, black, 'r', 5).
piece(br2, rook, black, 'r', 5).
piece(bq, queen, black, 'q', 9).
piece(bk, king, black, 'k', 100).

%bierka techniczna
piece(x,x,x,'-',0).

sites(white,black).
sites(black,white).

%dynamiczne
%czyja kolej


%pozycja bierki
start_position:-
    retractall(piece_position(_,_)),
    retractall(site(__)),

    assert(site(white)),

    assert(piece_position(a1, wr1)),
    assert(piece_position(h1, wr2)),
    assert(piece_position(b1, wn1)),
    assert(piece_position(g1, wn2)),
    assert(piece_position(c1, wb1)),
    assert(piece_position(f1, wb2)),
    assert(piece_position(d1, wq)),
    assert(piece_position(e1, wk)),
    assert(piece_position(a2, wp1)),
    assert(piece_position(b2, wp2)),
    assert(piece_position(c2, wp3)),
    assert(piece_position(d2, wp4)),
    assert(piece_position(e2, wp5)),
    assert(piece_position(f2, wp6)),
    assert(piece_position(g2, wp7)),
    assert(piece_position(h2, wp8)),

    assert(piece_position(a8, br1)),
    assert(piece_position(h8, br2)),
    assert(piece_position(b8, bn1)),
    assert(piece_position(g8, bn2)),
    assert(piece_position(c8, bb1)),
    assert(piece_position(f8, bb2)),
    assert(piece_position(d8, bq)),
    assert(piece_position(e8, bk)),
    assert(piece_position(a7, bp1)),
    assert(piece_position(b7, bp2)),
    assert(piece_position(c7, bp3)),
    assert(piece_position(d7, bp4)),
    assert(piece_position(e7, bp5)),
    assert(piece_position(f7, bp6)),
    assert(piece_position(g7, bp7)),
    assert(piece_position(h7, bp8)).

%pozycje bierek danej strony
site_piece_position(Site,Square,Piece):-
    piece_position(Square,ExactPiece),
    piece(ExactPiece,Piece,Site,_,_).

% mozliwy ruch w zaleznosci od rodzaju figury(strona, figura,pole
% startowe, pole docelowe) skoczek

move_rules(Site, Piece, A, B):-
    Piece = 'knight',
    square(A,X,Y),
    X2 is X+2,
    Y2 is Y+1,
    square(B,X2,Y2).

move_rules(Site, Piece, A, B):-
    Piece = 'knight',
    square(A,X,Y),
    X2 is X+1,
    Y2 is Y+2,
    square(B,X2,Y2).

move_rules(Site, Piece, A, B):-
    Piece = 'knight',
    square(A,X,Y),
    X2 is X-1,
    Y2 is Y+2,
    square(B,X2,Y2).

move_rules(Site, Piece, A, B):-
    Piece = 'knight',
    square(A,X,Y),
    X2 is X-2,
    Y2 is Y+1,
    square(B,X2,Y2).

move_rules(Site, Piece, A, B):-
    Piece = 'knight',
    square(A,X,Y),
    X2 is X+1,
    Y2 is Y-2,
    square(B,X2,Y2).

move_rules(Site, Piece, A, B):-
    Piece = 'knight',
    square(A,X,Y),
    X2 is X+2,
    Y2 is Y-1,
    square(B,X2,Y2).

move_rules(Site, Piece, A, B):-
    Piece = 'knight',
    square(A,X,Y),
    X2 is X-1,
    Y2 is Y-2,
    square(B,X2,Y2).

move_rules(Site, Piece, A, B):-
    Piece = 'knight',
    square(A,X,Y),
    X2 is X-2,
    Y2 is Y-1,
    square(B,X2,Y2).

%wieza
move_rules(Site,Piece,A,B):-
    Piece='rook',
    square(A,X,_),
    square(B,X,_),
    B \= A.

move_rules(Site,Piece,A,B):-
    Piece='rook',
    square(A,_,X),
    square(B,_,X),
    B \= A.

%goniec
move_rules(Site,Piece,A,B):-
    Piece='bishop',
    square(A,X,Y),
    Diff is Y-X,
    rank(X2),
    Y2 is X2+Diff,
    square(B,X2,Y2),
    B \= A.

move_rules(Site,Piece,A,B):-
    Piece='bishop',
    square(A,X,Y),
    Sum is Y+X,
    rank(X2),
    Y2 is Sum-X2,
    square(B,X2,Y2),
    B \= A.

%hetman
move_rules(Site,Piece,A,B):-
    Piece='queen',
    move_rules(Site,'bishop',A,B).

move_rules(Site,Piece,A,B):-
    Piece='queen',
    move_rules(Site,'rook',A,B).


%król
move_rules(Site,Piece,A,B):-
    Piece='king',
    square(A,X,Y),
    king_diff(K1),
    king_diff(K2),
    X2 is X+K1,
    Y2 is Y+K2,
    square(B,X2,Y2),
    B \= A.

%pion bia³ych - ruch na wprost
move_rules(Site,Piece,A,B):-
    Piece='pawn',
    Site='white',
    square(A,X,Y),
    Y2 is Y+1,
    square(B,X,Y2).

move_rules(Site,Piece,A,B):-
    Piece='pawn',
    Site='white',
    square(A,X,Y),
    Y=2,
    Y2 is Y+2,
    square(B,X,Y2).

%pion czarnych - ruch na wprost
move_rules(Site,Piece,A,B):-
    Piece='pawn',
    Site='black',
    square(A,X,Y),
    Y2 is Y-1,
    square(B,X,Y2).

move_rules(Site,Piece,A,B):-
    Piece='pawn',
    Site='black',
    square(A,X,Y),
    Y=7,
    Y2 is Y-2,
    square(B,X,Y2).

%sprawdzamy czy pomiedzy oboma polami stoi jakas inna figura
move_not_clear_way(Piece,A,B):-
    (Piece='rook';Piece='queen'),
    square(A,X1,Y1),
    square(B,X2,Y2),
    X1\=X2,
    Y1=:=Y2,
    Z1 is min(X1,X2)+1,
    Z2 is max(X1,X2)-1,
    between(Z1,Z2,L),
    square(Square,L,Y1),
    piece_position(Square,Sth).

move_not_clear_way(Piece,A,B):-
    (Piece='rook';Piece='pawn';Piece='queen'),
    square(A,X1,Y1),
    square(B,X2,Y2),
    X1=:=X2,
    Y1\=Y2,
    Z1 is min(Y1,Y2)+1,
    Z2 is max(Y1,Y2)-1,
    between(Z1,Z2,L),
    square(Square,X1,L),
    piece_position(Square,Sth).

move_not_clear_way(Piece,A,B):-
    (Piece='bishop';Piece='queen'),
    square(A,X1,Y1),
    square(B,X2,Y2),
    X1-Y1=:=X2-Y2,
    Z1 is min(X1,X2)+1,
    Z2 is max(X1,X2)-1,
    between(Z1,Z2,LX),
    LY is LX-X1+Y1,
    square(Square,LX,LY),
    piece_position(Square,Sth).

move_not_clear_way(Piece,A,B):-
    (Piece='bishop';Piece='queen'),
    square(A,X1,Y1),
    square(B,X2,Y2),
    X1+Y1=:=X2+Y2,
    Z1 is min(X1,X2)+1,
    Z2 is max(X1,X2)-1,
    between(Z1,Z2,LX),
    LY is X1+Y1-LX,
    square(Square,LX,LY),
    piece_position(Square,Sth).

destination_occupied_by_partner(Site,Square):-
    piece(ExactPiece, _, Site, _, _),
    piece_position(Square, ExactPiece).

destination_occupied_by_opponent(Site,Square):-
    sites(Site,SecondSite),
    piece(ExactPiece, _, SecondSite, _, _),
    piece_position(Square, ExactPiece).


%mo¿liwoœæ ruchu
%wszystkie bierki i piony na wprost
move_possibility(Site,Piece,A,B):-
    move_rules(Site,Piece,A,B),
    not(move_not_clear_way(Piece,A,B)),
    not(destination_occupied_by_partner(Site,B)),
    ((Piece='pawn',not(destination_occupied_by_opponent(Site,B)))
    ;Piece\='pawn').

%bicie pionem - bia³e
move_possibility(Site,Piece,A,B):-
    Piece='pawn',
    destination_occupied_by_opponent(Site,B),
    Site='white',
    square(A,X,Y),
    Y2 is Y+1,
    (X2 is X+1;X2 is X-1),
    square(B,X2,Y2).

%bicie pionem - czarne
move_possibility(Site,Piece,A,B):-
    Piece='pawn',
    destination_occupied_by_opponent(Site,B),
    Site='black',
    square(A,X,Y),
    Y2 is Y-1,
    (X2 is X+1;X2 is X-1),
    square(B,X2,Y2).

% sprawdzenie mo¿liwoœciu ruchu z uwzglêdnieniem niemo¿liwoœci wykonania
% ruchu powoduj¹cego szach na w³asnym królu
move_possibility_check_inc(Site,Piece,A,B):-
    site_piece_position(Site,A,Piece),
    piece_position(A,Z),
    move_possibility(Site,Piece,A,B),
    snapshot((
    (retract(piece_position(B,OppPiece));
    not(retract(piece_position(B,_)))),
    retract(piece_position(A,Z)),
    assert(piece_position(B,Z)),
    not(is_checked(Site))
    )).

print_possible_moves:-
    site(Site),
    move_possibility_check_inc(Site,Piece,From,To),
    write(Piece), write(': '), write(From), write('-'), write(To),
    nl.



%ruch bierki(pole wyjsciowe, pole koncowe)
move(A,B):-
    piece_position(A,Z),
    piece(Z,Piece,Site,_,_),
    site(Site),
    move_possibility(Site,Piece,A,B),
    transaction((
    (retract(piece_position(B,OppPiece));
    not(retract(piece_position(B,_)))),
    retract(piece_position(A,_)),
    assert(piece_position(B,Z)),
    not(is_checked(Site)))),
    sites(Site,WaitSite),
    retract(site(_)),
    (is_mated(WaitSite), assert(site('none')), write('Mate!');
      not(is_mated(WaitSite)),assert(site(WaitSite))).


is_checked(CheckedSite):-
    piece(King,king,CheckedSite,_,_),
    piece_position(Square,King),
    sites(CheckedSite,CheckingSite),
    site_piece_position(CheckingSite,StartSquare,Piece),
    move_possibility(CheckingSite,Piece,StartSquare,Square),
    !.

is_mated(MatedSite):-
    is_checked(MatedSite),
    not(move_possibility_check_inc(MatedSite,_,_,_)).

is_mate_in_1(Site,Piece,A,B):-
    sites(Site,WaitingSite),
    site_piece_position(Site,A,Piece),
    piece_position(A,Z),
    move_possibility(Site,Piece,A,B),
    snapshot((
    retract(piece_position(A,Z)),
    assert(piece_position(B,Z)),
    not(is_checked(Site)),
    is_mated(WaitingSite),
    retract(piece_position(B,Z)),
    assert(piece_position(A,Z)))).

find_possible_mates_in_1:-
    site(Site),
    is_mate_in_1(Site,Piece,From,To),
    write(Piece), write(': '), write(From), write('-'), write(To),
    nl.

print:- piece_position(X,Y),write(X),write('  '),write(Y),nl.



%wyswietlanie szachownicy
rank_piece(Rank,PieceList):-
    square(Square,_,Rank),
    (piece_position(Square,ExactPiece);
    (not(piece_position(Square,ExactPiece)), ExactPiece='x')),
    findall(DrawPiece,piece(ExactPiece,_,_,DrawPiece,_),PieceList).

write_list([Head|Tail]):-
	write(Head), write(' '),
	write_list(Tail).

write_line(Rank):-
    rank_piece(Rank,X),
    write_list(X).

show_chessboard:-
    nl,nl,
    (write_line(8);nl),
    (write_line(7);nl),
    (write_line(6);nl),
    (write_line(5);nl),
    (write_line(4);nl),
    (write_line(3);nl),
    (write_line(2);nl),
    (write_line(1);nl),
    nl,nl,
    write('To move: '),
    site(X), write(X).


%wczytywanie plików
load_position(File) :-
        open(File, read, Stream),
        get_char(Stream, Char1),
        process_stream(Char1, Stream),
        close(Stream).


process_stream(end_of_file, _) :- !.

process_stream(Char, Stream) :-
        write(Char),
        get_char(Stream, Char2),
        process_stream(Char2, Stream).




%Menu
menu:-
    nl,
    write('Avaliable options:'),nl,
    write(' - move(from,to)'),nl,
    write(' - start_position'),nl,
    write(' - show_chessboard'),nl,
    write(' - load_position(file)'),nl,
    write(' - print_possible_moves'),nl,
    write(' - find_possible_mates_in_1'),nl.



mate_2_possibility(Site,Piece,A,B,Piece2,A2,B2,Piece3,A3,B3):-
    site_piece_position(Site,A,Piece),
    piece_position(A,Z),
    move_possibility(Site,Piece,A,B),
    snapshot((
    retract(piece_position(A,Z)),
    assert(piece_position(B,Z)),
    not(is_checked(Site)),
    %ruch rywala
    sites(Site,WaitingSite),
    site_piece_position(WaitingSite,A2,Piece2),
    piece_position(A2,Z2),
    move_possibility(WaitingSite,Piece2,A2,B2),
    write('przed otwarciem transakcji'),
    show_chessboard,
    snapshot((
    write('po otwarciu transakcji'),
    show_chessboard,    retract(piece_position(A,Z)),
    retract(piece_position(A2,Z2)),
    assert(piece_position(B2,Z2)),
    not(is_checked(WaitingSite)),
    is_mated(Site)
    %2. ruch
    %site_piece_position(Site,A3,Piece3),
    %piece_position(A3,Z3),
    %move_possibility(Site,Piece3,A3,B3),
    %transaction((
    %retract(piece_position(A3,Z3)),
    %assert(piece_position(B3,Z3)),
    %not(is_checked(Site)),
    %is_mated(WaitingSite),
    %write('XXXXX'),
    %retract(piece_position(B3,Z3)),
    %assert(piece_position(A3,Z3))
    %)),
    %powrót z ruchu rywal
    ))
    %powrót z pierwszego ruchu
    )).












