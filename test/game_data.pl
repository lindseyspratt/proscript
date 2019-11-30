
model_basics:data_default_id(1).
%success
%:- if(fail).
% model_basics:data_numberOfPlayers

model_basics:data_numberOfPlayers(1,2).

% model_basics:data_trianglesPerTile

model_basics:data_trianglesPerTile(1,4).

% model_basics:data_abstractColors

model_basics:data_abstractColors(1,[1,2,3,4]).

% model_basics:data_handColorIDSequences

model_basics:data_handColorIDSequences(1,[[[1,1,1,1],[1,1,1,1],[2,1,1,1],[2,1,1,1],[2,2,1,1],[1,1,2,2],[2,1,2,1],[1,2,1,2]],[[2,2,2,2],[2,2,2,2],[1,2,2,2],[1,2,2,2],[1,1,2,2],[2,2,1,1],[1,2,1,2],[2,1,2,1]]]).


%failure point
%:- if(fail).

view_basics:data_default_id(1).

% view_basics:data_canvasWidth

view_basics:data_canvasWidth(1,800).

% view_basics:data_canvasHeight

view_basics:data_canvasHeight(1,600).

% view_basics:data_canvasOffsetTop

view_basics:data_canvasOffsetTop(1,269).

% view_basics:data_canvasOffsetLeft

view_basics:data_canvasOffsetLeft(1,60).

% view_basics:data_context

view_basics:data_context(1,'$obj'(obj1)).

% view_basics:data_colors

view_basics:data_colors(1,['#008800','#4444FF']).

% view_basics:data_highlightColors

view_basics:data_highlightColors(1,['#CCFFCC','#CCCCFF']).

% view_basics:data_handTileSize

view_basics:data_handTileSize(1,55).

% view_basics:data_handPadding

view_basics:data_handPadding(1,4).

% view_basics:data_handMargin

view_basics:data_handMargin(1,5).

% view_basics:data_boardTileSize

view_basics:data_boardTileSize(1,75).

% view_basics:data_boardLeft

view_basics:data_boardLeft(1,100).

% view_basics:data_boardTop

view_basics:data_boardTop(1,0).

% view_basics:data_boardWidth

view_basics:data_boardWidth(1,800).

% view_basics:data_boardHeight

view_basics:data_boardHeight(1,600).

% failure point
%:- if(fail).
tile_model:tile_model_default_id(16).


% tile_model:tile_model_gridX

tile_model:tile_model_gridX(1,0).
tile_model:tile_model_gridX(2,0).
tile_model:tile_model_gridX(3,0).
tile_model:tile_model_gridX(4,0).
tile_model:tile_model_gridX(5,0).
tile_model:tile_model_gridX(6,0).
tile_model:tile_model_gridX(7,0).
tile_model:tile_model_gridX(8,0).
tile_model:tile_model_gridX(9,0).
tile_model:tile_model_gridX(10,0).
tile_model:tile_model_gridX(11,0).
tile_model:tile_model_gridX(12,0).
tile_model:tile_model_gridX(13,0).
tile_model:tile_model_gridX(14,0).
tile_model:tile_model_gridX(15,0).
tile_model:tile_model_gridX(16,0).

% tile_model:tile_model_gridY

tile_model:tile_model_gridY(1,0).
tile_model:tile_model_gridY(2,0).
tile_model:tile_model_gridY(3,0).
tile_model:tile_model_gridY(4,0).
tile_model:tile_model_gridY(5,0).
tile_model:tile_model_gridY(6,0).
tile_model:tile_model_gridY(7,0).
tile_model:tile_model_gridY(8,0).
tile_model:tile_model_gridY(9,0).
tile_model:tile_model_gridY(10,0).
tile_model:tile_model_gridY(11,0).
tile_model:tile_model_gridY(12,0).
tile_model:tile_model_gridY(13,0).
tile_model:tile_model_gridY(14,0).
tile_model:tile_model_gridY(15,0).
tile_model:tile_model_gridY(16,0).

% tile_model:tile_model_colors

tile_model:tile_model_colors(1,[1,1,1,1]).
tile_model:tile_model_colors(2,[1,1,1,1]).
tile_model:tile_model_colors(3,[2,1,1,1]).
tile_model:tile_model_colors(4,[2,1,1,1]).
tile_model:tile_model_colors(5,[2,2,1,1]).
tile_model:tile_model_colors(6,[1,1,2,2]).
tile_model:tile_model_colors(7,[2,1,2,1]).
tile_model:tile_model_colors(8,[1,2,1,2]).
tile_model:tile_model_colors(9,[2,2,2,2]).
tile_model:tile_model_colors(10,[2,2,2,2]).
tile_model:tile_model_colors(11,[1,2,2,2]).
tile_model:tile_model_colors(12,[1,2,2,2]).
tile_model:tile_model_colors(13,[1,1,2,2]).
tile_model:tile_model_colors(14,[2,2,1,1]).
tile_model:tile_model_colors(15,[1,2,1,2]).
tile_model:tile_model_colors(16,[2,1,2,1]).

% tile_model:tile_model_container

tile_model:tile_model_container(1,board).
tile_model:tile_model_container(2,hand(1)).
tile_model:tile_model_container(3,hand(1)).
tile_model:tile_model_container(4,hand(1)).
tile_model:tile_model_container(5,hand(1)).
tile_model:tile_model_container(6,hand(1)).
tile_model:tile_model_container(7,hand(1)).
tile_model:tile_model_container(8,hand(1)).
tile_model:tile_model_container(9,hand(2)).
tile_model:tile_model_container(10,hand(2)).
tile_model:tile_model_container(11,hand(2)).
tile_model:tile_model_container(12,hand(2)).
tile_model:tile_model_container(13,hand(2)).
tile_model:tile_model_container(14,hand(2)).
tile_model:tile_model_container(15,hand(2)).
tile_model:tile_model_container(16,hand(2)).

% tile_model:tile_model_replacements

tile_model:tile_model_replacements(1,[]).
tile_model:tile_model_replacements(2,[]).
tile_model:tile_model_replacements(3,[]).
tile_model:tile_model_replacements(4,[]).
tile_model:tile_model_replacements(5,[]).
tile_model:tile_model_replacements(6,[]).
tile_model:tile_model_replacements(7,[]).
tile_model:tile_model_replacements(8,[]).
tile_model:tile_model_replacements(9,[]).
tile_model:tile_model_replacements(10,[]).
tile_model:tile_model_replacements(11,[]).
tile_model:tile_model_replacements(12,[]).
tile_model:tile_model_replacements(13,[]).
tile_model:tile_model_replacements(14,[]).
tile_model:tile_model_replacements(15,[]).
tile_model:tile_model_replacements(16,[]).

% tile_model:tile_model_minimumMismatch

tile_model:tile_model_minimumMismatch(1,[]).
tile_model:tile_model_minimumMismatch(2,[]).
tile_model:tile_model_minimumMismatch(3,[]).
tile_model:tile_model_minimumMismatch(4,[]).
tile_model:tile_model_minimumMismatch(5,[]).
tile_model:tile_model_minimumMismatch(6,[]).
tile_model:tile_model_minimumMismatch(7,[]).
tile_model:tile_model_minimumMismatch(8,[]).
tile_model:tile_model_minimumMismatch(9,[]).
tile_model:tile_model_minimumMismatch(10,[]).
tile_model:tile_model_minimumMismatch(11,[]).
tile_model:tile_model_minimumMismatch(12,[]).
tile_model:tile_model_minimumMismatch(13,[]).
tile_model:tile_model_minimumMismatch(14,[]).
tile_model:tile_model_minimumMismatch(15,[]).
tile_model:tile_model_minimumMismatch(16,[]).

% tile_view:data_displayX

tile_view:data_displayX(1,412.5).
tile_view:data_displayX(2,5).
tile_view:data_displayX(3,5).
tile_view:data_displayX(4,5).
tile_view:data_displayX(5,5).
tile_view:data_displayX(6,5).
tile_view:data_displayX(7,5).
tile_view:data_displayX(8,5).
tile_view:data_displayX(9,740).
tile_view:data_displayX(10,740).
tile_view:data_displayX(11,740).
tile_view:data_displayX(12,740).
tile_view:data_displayX(13,740).
tile_view:data_displayX(14,740).
tile_view:data_displayX(15,740).
tile_view:data_displayX(16,740).

% tile_view:data_displayY

tile_view:data_displayY(1,262.5).
tile_view:data_displayY(2,64).
tile_view:data_displayY(3,123).
tile_view:data_displayY(4,182).
tile_view:data_displayY(5,241).
tile_view:data_displayY(6,300).
tile_view:data_displayY(7,359).
tile_view:data_displayY(8,418).
tile_view:data_displayY(9,5).
tile_view:data_displayY(10,64).
tile_view:data_displayY(11,123).
tile_view:data_displayY(12,182).
tile_view:data_displayY(13,241).
tile_view:data_displayY(14,300).
tile_view:data_displayY(15,359).
tile_view:data_displayY(16,418).

% tile_view:data_size

tile_view:data_size(1,75).
tile_view:data_size(2,55).
tile_view:data_size(3,55).
tile_view:data_size(4,55).
tile_view:data_size(5,55).
tile_view:data_size(6,55).
tile_view:data_size(7,55).
tile_view:data_size(8,55).
tile_view:data_size(9,55).
tile_view:data_size(10,55).
tile_view:data_size(11,55).
tile_view:data_size(12,55).
tile_view:data_size(13,55).
tile_view:data_size(14,55).
tile_view:data_size(15,55).
tile_view:data_size(16,55).

game_model_tiles:data_default_id(1).

% game_model_tiles:data_tile_counter

game_model_tiles:data_tile_counter(1,16).

% game_model_tiles:data_tiles

game_model_tiles:data_tiles(1,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).

% game_model_tiles:data_hands

game_model_tiles:data_hands(1,[[2,3,4,5,6,7,8],[9,10,11,12,13,14,15,16]]).

% game_model_tiles:data_board

game_model_tiles:data_board(1,[1]).

% game_model_tiles:data_tilesPlaced

game_model_tiles:data_tilesPlaced(1,0).

% game_model_tiles:data_boardHash

game_model_tiles:data_boardHash(1,[x0y0-1]).

% game_model_tiles:data_lastPlacedTile1

game_model_tiles:data_lastPlacedTile1(1,none).

% game_model_tiles:data_lastPlacedTile2

game_model_tiles:data_lastPlacedTile2(1,none).

% game_model_tiles:data_lastBuildPhaseTilePlacedID

game_model_tiles:data_lastBuildPhaseTilePlacedID(1,1).

% game_model_tiles:data_turn

game_model_tiles:data_turn(1,2).

% game_model_tiles:data_selectedTileID

game_model_tiles:data_selectedTileID(1,none).

% game_model_tiles:data_replacements

game_model_tiles:data_replacements(1,[]).

% game_model_tiles:data_gamePhase

game_model_tiles:data_gamePhase(1,build).

game_view_tiles:data_default_id(1).

% game_view_tiles:data_translateX

game_view_tiles:data_translateX(1,-50).

% game_view_tiles:data_translateY

game_view_tiles:data_translateY(1,0).

% game_view_tiles:data_targetTranslateX

game_view_tiles:data_targetTranslateX(1,-50).

% game_view_tiles:data_targetTranslateY

game_view_tiles:data_targetTranslateY(1,0).

% location_model:data_gridX

location_model:data_gridX(3,-1).
location_model:data_gridX(5,0).
location_model:data_gridX(6,0).
location_model:data_gridX(8,1).

% location_model:data_gridY

location_model:data_gridY(3,0).
location_model:data_gridY(5,-1).
location_model:data_gridY(6,1).
location_model:data_gridY(8,0).

% location_model:data_neighbors

location_model:data_neighbors(3,1).
location_model:data_neighbors(5,1).
location_model:data_neighbors(6,1).
location_model:data_neighbors(8,1).

% location_model:data_orthogonalNeighbors

location_model:data_orthogonalNeighbors(3,1).
location_model:data_orthogonalNeighbors(5,1).
location_model:data_orthogonalNeighbors(6,1).
location_model:data_orthogonalNeighbors(8,1).

% location_model:data_byLastTilePlaced

location_model:data_byLastTilePlaced(3,true).
location_model:data_byLastTilePlaced(5,true).
location_model:data_byLastTilePlaced(6,true).
location_model:data_byLastTilePlaced(8,true).

% location_model:data_constraints

location_model:data_constraints(3,[-1,1,-1,-1]).
location_model:data_constraints(5,[-1,-1,1,-1]).
location_model:data_constraints(6,[1,-1,-1,-1]).
location_model:data_constraints(8,[-1,-1,-1,1]).

% location_model:data_forcedColors

location_model:data_forcedColors(3,[]).
location_model:data_forcedColors(5,[]).
location_model:data_forcedColors(6,[]).
location_model:data_forcedColors(8,[]).

% location_model:data_replacements

location_model:data_replacements(3,[]).
location_model:data_replacements(5,[]).
location_model:data_replacements(6,[]).
location_model:data_replacements(8,[]).

% location_model:data_minimumMismatch

location_model:data_minimumMismatch(3,[]).
location_model:data_minimumMismatch(5,[]).
location_model:data_minimumMismatch(6,[]).
location_model:data_minimumMismatch(8,[]).
/*
locations:data_default_id(1).

% locations:data_locationCounter

locations:data_locationCounter(1,9).

% locations:data_shapedPositions

locations:data_shapedPositions(1,[8,6,5,3]).

% locations:data_legalPositions

locations:data_legalPositions(1,[]).

% locations:data_legalPositionsWithRotation

locations:data_legalPositionsWithRotation(1,[]).

% locations:data_irreplaceables

locations:data_irreplaceables(1,[]).
%:- endif.
*/