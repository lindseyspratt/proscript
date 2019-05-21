:- ensure_loaded('../library/object'). % for >>/2.
:- ensure_loaded('../library/listut2'). % for append_lists/2

/*
*/

:- dynamic(is_selected/1).

:- initialization(data_predicate_dynamics).

draw_tile_test :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    assert_data(ts(20, 20, 1, 1, 50, [red, red, green, green], none), 1),
    draw_tile(Ctx, 1).

draw_all_tile_test :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
    assert_data(ts(20, 20, 1, 1, 50, [red, red, green, green], none), 1),
    draw_all_tile(1, Ctx).

draw_all_tiles_test :-
    setup_draw_all_tiles_test(Ctx, W, H, Tiles),
    draw_all_tiles(Tiles, Ctx, W, H).

setup_draw_all_tiles_test(Ctx, W, H, [Tile1, Tile2]) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],

    Tile1 = 1,
    Tile2 = 2,
    assert_data(ts(20, 20, 1, 1, 50, [red, red, green, green], none), 1),
    assert_data(ts(70, 20, 2, 1, 50, [green, red, green, red], none), 2),
% [tile_size, board_left, board_top, board_width, board_height, board_translate, turn, replacements]
    assert_data(g(50, 0, 0, 800, 800, 0, 1, [Tile2]), 1),

    retractall(is_selected(_)),
    asserta(is_selected(Tile1)).

draw_legal_moves_test :-
    setup_legal_moves(Ctx),
    draw_legal_moves([legal_position(1,1)], [legal_position(1,2)], Ctx).

setup_legal_moves(Ctx) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx],
% [tile_size, board_left, board_top, board_width, board_height, board_translate, turn, replacements]
    assert_data(g(50, 10, 10, 200, 200, 1>1, 1, [Tile2]), 1).

% (X > Y) is a point (X,Y).
% Web API method arguments of type number or integer accept arithmetic
% expressions; e.g. (1 + 0.5 * 50).

draw_tile(Ctx, Tile) :-
    tile_x(Tile, X),
    tile_y(Tile, Y),
    tile_size(Tile, Size),
    Corners = [X > Y,X + Size > Y,X + Size > Y + Size, X > Y + Size],
    Center = (X + 0.5 * Size > Y + 0.5 * Size),
    tile_colors(Tile, Colors),
    draw_triangles(Corners, Colors, Center, Ctx),
    tile_bx(Tile, BX),
    tile_by(Tile, BY),
    tile_label(BX, BY, Text),
    Ctx >> [
        save,
        fillStyle <:+ '#000',
        fillText(Text, X+5, Y+10),
        restore
    ].


draw_triangles([P1, P2|OtherCorners], [Color1|OtherColors], Center, Ctx) :-
   draw_triangle(P1, P2, Color1, Center, Ctx),
   draw_triangles1([P2|OtherCorners], OtherColors, P1, Center, Ctx).

draw_triangles1([P1], [Color], P2, Center, Ctx) :-
   draw_triangle(P1, P2, Color, Center, Ctx).
draw_triangles1([P1, P2|OtherCorners], [Color1|OtherColors], FirstP, Center, Ctx) :-
   draw_triangle(P1, P2, Color1, Center, Ctx),
   draw_triangles1([P2|OtherCorners], OtherColors, FirstP, Center, Ctx).

draw_triangle(P1x > P1y, P2x > P2y, Color, CenterX > CenterY, Ctx) :-
    Ctx >> [
        beginPath,
        moveTo(P1x, P1y),
        lineTo(P2x, P2y),
        lineTo(CenterX, CenterY),
        closePath,

        save,
        fillStyle <:+ Color,
        fill,
        stroke,
        restore
    ].

draw_all_tiles(AllTiles, Ctx, CW, CH) :-
    center_board,
    Ctx >> [
        fillStyle <:+ '#999',
        fillRect(0, 0, CW, CH)
    ],
    draw_all_tiles1(AllTiles, Ctx).

draw_all_tiles1([], _).
draw_all_tiles1([H|T], Ctx) :-
    draw_all_tile(H, Ctx),
    draw_all_tiles1(T, Ctx).

draw_all_tile(Tile, Ctx) :-
    (tile_in_inactive_hand(Tile) -> GlobalAlpha = 0.3; GlobalAlpha = 1),
    Ctx >> [
        save,
        globalAlpha <:+ GlobalAlpha
    ],
    draw_tile(Ctx, Tile),
    Ctx >*> restore,
    (is_selected(Tile)
        -> draw_selected_tile_mark(Tile, Ctx)
     ;
     true
    ),
    draw_replacements(Tile, Ctx).

draw_replacements(Tile, Ctx) :-
%    tile_id(R, ID),
%    tile_id(Tile, ID),
    (game_replacements(Rs),
     member(Tile, Rs)
       -> draw_replacement_tile_mark(Tile, Ctx)
    ;
    true
    ).

center_board.

draw_selected_tile_mark(Tile, Ctx) :-
    tile_x(Tile, X),
    tile_y(Tile, Y),
    tile_size(Tile, Size),

    MidX is X + (Size / 2),
    MidY is Y + (Size / 2),
    Adjust is Size / 4,

	VerticalTopX = MidX,
	VerticalTopY is MidY - Adjust,
	VerticalBottomX = MidX,
	VerticalBottomY is MidY + Adjust,
	HorizontalLeftX is MidX-Adjust,
	HorizontalLeftY = MidY,
	HorizontalRightX is MidX+Adjust,
	HorizontalRightY = MidY,

	game_turn(GT),
	highlight_color(GT, Color),

	Ctx >> [
	    save,

	    lineWidth <:+ 3,
	    strokeStyle <:+ Color,
	    beginPath,
	    moveTo(VerticalTopX, VerticalTopY),
	    lineTo(VerticalBottomX, VerticalBottomY),
	    closePath,
	    stroke,

	    beginPath,
	    moveTo(HorizontalLeftX, HorizontalLeftY),
	    lineTo(HorizontalRightX, HorizontalRightY),
	    closePath,
	    stroke,

	    restore
	].

draw_replacement_tile_mark(Tile, Ctx) :-
    tile_x(Tile, X),
    tile_y(Tile, Y),
    tile_size(Tile, Size),

    MidX is X + (Size / 2),
    MidY is Y + (Size / 2),
    Adjust is Size / 4,

	game_turn(GT),
	highlight_color(GT, Color),

	Ctx >> [
	    save,
	    lineWidth <:+ 3,
	    strokeStyle <:+ Color,
	    beginPath,
	    arc(MidX, MidY, Adjust, 0, 2*pi),
	    closePath,
	    stroke,
	    restore
	].

draw_legal_moves(LegalPositions, LegalPositionsWithRotation, Ctx) :-
	game_turn(GT),
	highlight_color(GT, Color),

    Ctx >> [
        save,
        beginPath,
        fillStyle <:+ Color,
        strokeStyle <:+ Color,
        lineWidth <:+ 3,
        globalAlpha <:+ 0.8
    ],

    game_tile_size(TileSize),

    draw_legal_positions_with_rotations(LegalPositionsWithRotation, Ctx, TileSize),

    draw_legal_positions(LegalPositions, Ctx, TileSize),

    Ctx >*> restore.

draw_legal_positions_with_rotations([], _, _).
draw_legal_positions_with_rotations([H|T], Ctx, TileSize) :-
    legal_position_b(H, B),
    get_top_left_board_tile_coords(B, TileSize, X > Y),
    Ctx >*> [
        rect(X, Y, TileSize, TileSize),
        stroke
    ],
    draw_legal_positions_with_rotations(T, Ctx, TileSize).

draw_legal_positions([], _, _).
draw_legal_positions([H|T], Ctx, TileSize) :-
    legal_position_b(H, B),
    get_top_left_board_tile_coords(B, TileSize, X > Y),
    Ctx >*> fillRect(X, Y, TileSize, TileSize),
    draw_legal_positions(T, Ctx, TileSize).

get_top_left_board_tile_coords(BX > BY, TileSize, X > Y) :-
    game_board_left(Left),
    game_board_translate(TX > TY),
    game_board_width(W),
    game_board_top(Top),
    game_board_height(H),
    X  is Left + TX + (W / 2) + (BX - 0.5) * TileSize,
    Y  is Top + TY + (H / 2) + (BY - 0.5) * TileSize.

% ShadowData is in-memory representation of data.
% where ShadowData is a structure of many arguments and
% data is stored as a collection of binary relations where
% each fact shares the same identifier as the first argument.
% For tiles the shadow tile structure functor is 'ts'
% and the arguments are 'x', 'y', etc.
% For the game the shadow game structure functor is 'g'
% and the arguments are 'board_left', 'turn', etc.
% (Note that there is currently only one 'game' so the ID is always '1'.)

assert_datas(Datas) :-
    assert_datas(Datas, 1).

assert_datas([], _).
assert_datas([H|T], J) :-
    assert_data(H, J),
    K is J + 1,
    assert_datas(T, K).

assert_data(ShadowData, ID) :-
    ShadowData =.. [F|Args],
    data_predicates(F, Prefix, Suffixes),
    (length(Suffixes, ArgCount),
     length(Args, ArgCount)
       -> assert_shadow_arguments(Args, Prefix, Suffixes, ID),
          default_asserted_id(Prefix, ID)
    ;
     throw('tile shadow argument count different from count of shadow argument tile_predicates')
    ).

assert_shadow_arguments([], _, [], _).
assert_shadow_arguments([H|T], Prefix, [HP|TP], ID) :-
    assert_shadow_argument(H, Prefix, HP, ID),
    assert_shadow_arguments(T, Prefix, TP, ID).

assert_shadow_argument(Arg, Prefix, Suffix, ID) :-
    construct_data_predicate(Prefix, Suffix, Predicate),
    GoalR =.. [Predicate, ID, _],
    Goal =.. [Predicate, ID, Arg],
    retractall(GoalR),
    assertz(Goal).

default_asserted_id(Prefix, ID) :-
    atom_concat(Prefix, '_default_id', Predicate),
    GoalR =.. [Predicate, _],
    Goal =.. [Predicate, ID],
    retractall(GoalR),
    assertz(Goal).

data_predicate_dynamics :-
    findall(Prefix-Suffixes, data_predicates(_, Prefix, Suffixes), All),
    data_predicate_dynamics(All).

data_predicate_dynamics([]).
data_predicate_dynamics([Prefix-Suffixes|T]) :-
    data_predicate_dynamics(Suffixes, Prefix),
    atom_concat(Prefix, '_default_id', DefaultPredicate),
    (dynamic(DefaultPredicate / 1)),
    data_predicate_dynamics(T).

data_predicate_dynamics([], _).
data_predicate_dynamics([H|T], Prefix) :-
    data_predicate_dynamic(H, Prefix),
    data_predicate_dynamics(T, Prefix).

data_predicate_dynamic(Suffix, Prefix) :-
    construct_data_predicate(Prefix, Suffix, Predicate),
    (dynamic(Predicate / 2)), % BUG: 'dynamic(Predicate/2), foo' is being parsed as dynamic (Predicate/2, foo).
    data_predicate_default_dynamic(Prefix, Predicate).

data_predicate_default_dynamic(Prefix, Predicate) :-
    (dynamic(Predicate /1)), % BUG: 'dynamic(Predicate/1), foo' is being parsed as dynamic (Predicate/1, foo).
    Head =.. [Predicate, Value],
    retractall((Head :- _)),
    atom_concat(Prefix, '_default_id', DefaultPredicate),
    DefaultGoal =.. [DefaultPredicate, DefaultID],
    BinaryGoal =.. [Predicate, DefaultID, Value],
    asserta((Head :- DefaultGoal, BinaryGoal)).

:- dynamic data_predicates/3.

data_predicates(ts, tile,[x, y,bx,by,size,colors,container]). % e.g. tile_x(ID, X), tile_y(ID, Y)...
data_predicates(g, game,[tile_size, board_left, board_top, board_width, board_height, board_translate, turn, replacements]). % e.g. game_board_left(ID, X)...

construct_data_predicate(Prefix, Suffix, Predicate) :-
    atom_concat(Prefix, '_', PrefixExtended),
    atom_concat(PrefixExtended, Suffix, Predicate).

container_id(container(ID, _Type), ID).
container_type(container(_ID, Type), Type).

tile_label(BoardX, BoardY, Text) :-
    number_codes(BoardX, BXCodes),
    number_codes(BoardY, BYCodes),
    append_lists(["x", BXCodes, "y", BYCodes], TextCodes),
    atom_codes(Text, TextCodes).

tile_in_inactive_hand(Tile) :-
    tile_container(Tile, Container),
    container_type(Container, hand),
    game_turn(TurnID),
    container_id(Container, TurnID).

highlight_color(1, '#CCFFCC').
highlight_color(2, '#CCCCFF').

legal_position_bx(legal_position(BX, _BY), BX).
legal_position_by(legal_position(_BX, BY), BY).

legal_position_b(T, BX > BY) :-
    legal_position_bx(T, BX),
    legal_position_by(T, BY).
