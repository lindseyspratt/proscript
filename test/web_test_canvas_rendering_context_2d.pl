:- ensure_loaded(web_test_utility).
:- ensure_loaded('../library/object').

% Changing the canvas width discards the existing context, if any.
reset_context(Canvas) :-
    Canvas >+> [width <: 100, width <: 300].

% Properties

test('CanvasRenderingContext2D', 'canvas of 2d context of canvas finds htmlcanvaselement', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, canvas, Cvs),
    dom_object_type(Cvs, htmlcanvaselement).

test('CanvasRenderingContext2D', 'fillStyle of 2d context of canvas finds #000000', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, fillStyle, F),
    (F = '#000000'
    ;
    dom_object_type(F, canvaspattern)
    ).

test('CanvasRenderingContext2D', 'font of 2d context of canvas finds 10px sans-serif', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, font, '10px sans-serif').

test('CanvasRenderingContext2D', 'globalCompositeOperation of 2d context of canvas finds source-over', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, globalCompositeOperation, 'source-over').

test('CanvasRenderingContext2D', 'imageSmoothingEnabled of 2d context of canvas finds true', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, imageSmoothingEnabled, true).

test('CanvasRenderingContext2D', 'lineCap of 2d context of canvas finds butt', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, lineCap, butt).

test('CanvasRenderingContext2D', 'lineDashOffset of 2d context of canvas finds 0', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, lineDashOffset, 0).

test('CanvasRenderingContext2D', 'lineJoin of 2d context of canvas finds miter', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, lineJoin, miter).

test('CanvasRenderingContext2D', 'lineWidth of 2d context of canvas finds 1', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, lineWidth, 1).

test('CanvasRenderingContext2D', 'lineWidth of 2d context of canvas finds 1', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, lineWidth, 1).

test('CanvasRenderingContext2D', 'miterLimit of 2d context of canvas finds 10', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, miterLimit, 10).

test('CanvasRenderingContext2D', 'shadowBlur of 2d context of canvas finds 0', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, shadowBlur, 0).

test('CanvasRenderingContext2D', 'shadowColor of 2d context of canvas finds rgba(0, 0, 0, 0)', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, shadowColor, 'rgba(0, 0, 0, 0)').

test('CanvasRenderingContext2D', 'shadowOffsetX of 2d context of canvas finds 0', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, shadowOffsetX, 0).

test('CanvasRenderingContext2D', 'shadowOffsetY of 2d context of canvas finds 0', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, shadowOffsetY, 0).

test('CanvasRenderingContext2D', 'strokeStyle of 2d context of canvas finds #000000', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, strokeStyle, '#000000').

test('CanvasRenderingContext2D', 'textAlign of 2d context of canvas finds start', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, textAlign, start).

test('CanvasRenderingContext2D', 'textBaseline of 2d context of canvas finds alphabetic', succeeded) :-
    dom_element_attribute_value(E, id, canvas),
    dom_object_method(E, getContext('2d', Ctx)),
    dom_object_property(_, Ctx, textBaseline, alphabetic).

% Methods

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/arc
test('CanvasRenderingContext2D', 'arc of 2d context of canvas draws circle', succeeded) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],

    Ctx >*> clearRect(0, 0, W, H),

    X is 2 * pi,
    Ctx >> [beginPath, arc(100, 75, 50, 0, X), stroke].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/arcTo
test('CanvasRenderingContext2D', 'arcTo of 2d context of canvas draws arc with control points', succeeded) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],

    Ctx >*> clearRect(0, 0, W, H),

    % Tangential lines
    Ctx >> [
        beginPath,
        strokeStyle <:+ gray,
        lineWidth <:+ 1,
        moveTo(200, 20),
        lineTo(200, 130),
        lineTo(50, 20),
        moveTo(200, 20),
        stroke],

    % Arc
    Ctx >> [
        beginPath,
        strokeStyle <:+ black,
        lineWidth <:+ 5,
        moveTo(200, 20),
        arcTo(200,130, 50,20, 40),
        stroke
        ],

    X is 2 * pi,

    % Start point
    Ctx >> [
        beginPath,
        fillStyle <:+ blue,
        arc(200, 20, 5, 0, X),
        fill
    ],

     % Control points
     Ctx >> [
        beginPath,
        fillStyle <:+ red,
        arc(200,130, 5, 0, X),
        arc(50,20, 5, 0, X),
        fill
     ].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/bezierCurveTo
test('CanvasRenderingContext2D', 'bezierCurveTo of 2d context of canvas draws re-curved line with control points', succeeded) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],

    Ctx >*> clearRect(0, 0, W, H),

    StartX = 50, StartY = 20,
    Cp1X= 230, Cp1Y = 30,
    Cp2X = 150, Cp2Y = 80,
    EndX = 250, EndY = 100,
    TwoPI is 2 * pi,

    % Cubic Bézier curve
    Ctx >> [
        beginPath,
        moveTo(StartX, StartY),
        bezierCurveTo(Cp1X, Cp1Y, Cp2X, Cp2Y, EndX, EndY),
        stroke
    ],

    % Start and end points
    Ctx >> [
        fillStyle <:+ blue,
        beginPath,
        arc(StartX, StartY, 5, 0, TwoPI),   % start point
        arc(EndX, EndY, 5, 0, TwoPI),       % end point
        fill
    ],

    % Control points
    Ctx >> [
        fillStyle <:+ red,
        beginPath,
        arc(Cp1X, Cp1Y, 5, 0, TwoPI),  % Control point one
        arc(Cp2X, Cp2Y, 5, 0, TwoPI),  % Control point two
        fill
    ].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/clip
test('CanvasRenderingContext2D', 'clip of 2d context of canvas draws disk with clipped rectangle', succeeded) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],

    Ctx >*> clearRect(0, 0, W, H),

    TwoPI is 2 * pi,

    % Create circular clipping region
    Ctx >> [
        beginPath,
        arc(100, 75, 50, 0, TwoPI),
        clip
    ],

    % Draw stuff that gets clipped
    Ctx >> [
        fillStyle <:+ blue,
        fillRect(0, 0, W, H),
        fillStyle <:+ orange,
        fillRect(0, 0, 100, 100)
    ].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/closePath
test('CanvasRenderingContext2D', 'closePath of 2d context of canvas draws triangle', succeeded) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],

    Ctx >*> clearRect(0, 0, W, H),

    Ctx >> [
        beginPath,
        moveTo(20, 140),     % Move pen to bottom-left corner
        lineTo(120, 10),     % Line to top corner
        lineTo(220, 140),    % Line to bottom-right corner
        closePath,           % Line to bottom-left corner
        stroke
    ].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/createImageData
test('CanvasRenderingContext2D', 'createImageData of 2d context of canvas gets imageData, which is modified and displayed', succeeded) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],

    Ctx >*> clearRect(0, 0, W, H),

    Ctx >*> createImageData(100, 100) :> ImageData,
    ImageData >+> data :> Array,
    Array >+> length :> Length,
    Limit is Length / 4 - 1,
    apply_set(0, Limit, Array),
    Ctx >*> putImageData(ImageData, 20, 20).

apply_set(N, N, _):- !.
apply_set(N, L, Array) :-
    Array >*> set([190, 0, 210, 255], N*4),
    K is N+1,
    apply_set(K, L, Array).

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/createLinearGradient
test('CanvasRenderingContext2D', 'createLinearGradient of 2d context of canvas creates gradient, which is displayed', succeeded) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],

    Ctx >*> clearRect(0, 0, W, H),

    Ctx >*> createLinearGradient(20,0, 220,0) :> Gradient,

    Gradient >*> [addColorStop(0, green), addColorStop(0.5, cyan), addColorStop(1, green)],

    Ctx >> [fillStyle <:+ Gradient, fillRect(20, 20, 200, 100)].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/createPattern
test('CanvasRenderingContext2D', 'createPattern of 2d context of canvas creates pattern, which is displayed', succeeded) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],

    Ctx >*> clearRect(0, 0, W, H),

    create_dom_element(img, Img),
    Img >+> src <: "https://mdn.mozillademos.org/files/222/Canvas_createpattern.png",
    Img >*> addEventListener(load,
        (Ctx >> [
            createPattern(Img, repeat) *:> Pattern,
            fillStyle <:+ Pattern,
            fillRect(0, 0, 300, 300)] )
    ).

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/createRadialGradient
test('CanvasRenderingContext2D', 'createRadialGradient of 2d context of canvas creates gradient, which is displayed', succeeded) :-
    _ >> [id -:> canvas, getContext('2d') *:> Ctx, width +:> W, height +:> H],

    Ctx >*> clearRect(0, 0, W, H),

    % Create a radial gradient
    % The inner circle is at x=110, y=90, with radius=30
    % The outer circle is at x=100, y=100, with radius=70
    Ctx >*> createRadialGradient(110,90,30, 100,100,70) :> Gradient,

    % Add three color stops
    Gradient >*> [addColorStop(0, pink), addColorStop(0.9, white), addColorStop(1, green)],

    % Set the fill style and draw a rectangle
    Ctx >> [fillStyle <:+ Gradient, fillRect(20, 20, 160, 160)].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/drawFocusIfNeeded
test('CanvasRenderingContext2D', 'drawFocusIfNeeded of 2d context of canvas draws a rectangle on a button', succeeded) :-
    setup_canvas_context(_, Ctx),

    Button >> [id -:> button, focus],

    Ctx >> [
        beginPath,
        rect(10, 10, 30, 30),
        drawFocusIfNeeded(Button)
    ].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/drawImage
test('CanvasRenderingContext2D', 'drawImage of 2d context of canvas displays a picture]', succeeded) :-
    setup_canvas_context(_, Ctx),

    Image >-> id :> source,

    Ctx >*> drawImage(Image, 33, 71, 104, 124, 21, 20, 87, 104).


% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/ellipse
test('CanvasRenderingContext2D', 'ellipse of 2d context of canvas draws ellipse with axis line', succeeded) :-
    setup_canvas_context(_, Ctx),

    % Draw the ellipse
    Ctx >> [
        beginPath,
        ellipse(100, 100, 50, 75, pi / 4, 0, 2 * pi),
        stroke
    ],

    % Draw the ellipse's line of reflection
    Ctx >> [
        beginPath,
        setLineDash([5, 5]),
        moveTo(0, 200),
        lineTo(200, 0),
        stroke
    ].

% fill, fillStyle, fillRect do not have specific tests because they are used in other tests.


% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/fillText
test('CanvasRenderingContext2D', 'fillText of 2d context of canvas draws text', succeeded) :-
    setup_canvas_context(_, Ctx),

    % Draw the text
    Ctx >> [
        font <:+ '50px serif',
        fillText('Hello world', 50, 90)
    ].
/*
% This test fails when run as part of run_tests with:
% SecurityError: The operation is insecure.
%
% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/getImageData
test('CanvasRenderingContext2D', 'getImageData of 2d context of canvas display black squares', succeeded) :-
    setup_canvas_context(_, Ctx),

    Ctx >> [
        rect(10, 10, 100, 100),
        fill,
        getImageData(60, 60, 200, 100) *:> ImageData
    ],

    Ctx >*> putImageData(ImageData, 150, 10).
*/

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/getLineDash
test('CanvasRenderingContext2D', 'getLineDash of 2d context of canvas returns 10,20', succeeded) :-
    setup_canvas_context(_, Ctx),

    Ctx >> [
        setLineDash([10, 20]),
        getLineDash *:> [10, 20]
        ],
    Ctx >> [
        beginPath,
        moveTo(0, 50),
        lineTo(300, 50),
        stroke
    ].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/isPointInPath
test('CanvasRenderingContext2D', 'isPointInPathX of 2d context of canvas succeeds', succeeded) :-
    setup_canvas_context(Canvas, Ctx),

    % Create circle
    dom_create_object(path2d, Circle),
    Circle >*> arc(150, 75, 50, 0, 2 * pi),

    Ctx >> [
        fillStyle <:+ red,
        fillPath(Circle)
        ],
    mouse_in_circle(150, 75, Circle, Ctx, Canvas).
%    Canvas >*> addEventListener(mousemove, [object-Event] ^ mouse_in_circle(Event, Circle, Ctx, Canvas)).

mouse_in_circle(Event, Circle, Ctx, Canvas) :-
    % Check whether point is inside circle
    Event >+> [clientX :> X, clientY :> Y],
    dom_release_object(Event),
    mouse_in_circle(X, Y, Circle, Ctx, Canvas).

mouse_in_circle(X, Y, Circle, Ctx, Canvas) :-
    (Ctx >*> isPointInPathX(Circle, X, Y)
      -> Color = green
     ;
      Color = red
    ),
    Ctx >+> fillStyle <: Color,

    % Draw circle
    Canvas >+> [width :> W, height :> H],

    Ctx >*> [
        clearRect(0, 0, W, H),
        fillPath(Circle)
    ].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/isPointInStroke
test('CanvasRenderingContext2D', 'isPointInStroke of 2d context of canvas succeeds', succeeded) :-
    setup_canvas_context(_, Ctx),

    Ctx >> [
        rect(10, 10, 100, 100),
        stroke,
        isPointInStroke(50, 10)
        ].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/isPointInStroke
test('CanvasRenderingContext2D', 'isPointInStrokePath of 2d context of canvas succeeds', succeeded) :-
    setup_canvas_context(Canvas, Ctx),

    % Create ellipse
    dom_create_object(path2d, Ellipse),
    Ellipse >*> ellipse(150, 75, 40, 60, pi * 0.25, 0, 2 * pi),

    Ctx >> [
        lineWidth <:+ 25,
        strokeStyle <:+ red,
        fillPath(Ellipse),
        stroke(Ellipse)
        ],
    mouse_in_stroke(150, 120, Ellipse, Ctx, Canvas).
%    Canvas >*> addEventListener(mousemove, [object-Event] ^ mouse_in_stroke(Event, Ellipse, Ctx, Canvas)).

mouse_in_stroke(Event, Ellipse, Ctx, Canvas) :-
    % Check whether point is inside Ellipse
    Event >+> [clientX :> X, clientY :> Y],
    dom_release_object(Event),
    mouse_in_stroke(X, Y, Ellipse, Ctx, Canvas).

mouse_in_stroke(X, Y, Ellipse, Ctx, Canvas) :-
    (Ctx >*> isPointInStrokePath(Ellipse, X, Y)
      -> Color = green
     ;
      Color = red
    ),
    Ctx >+> strokeStyle <: Color,

    % Draw circle
    Canvas >+> [width :> W, height :> H],

    Ctx >*> [
        clearRect(0, 0, W, H),
        fillPath(Ellipse),
        stroke(Ellipse)
    ].

% lineTo tested elsewhere.

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/measureText
test('CanvasRenderingContext2D', 'measureText of 2d context of canvas finds width 49 < W < 50', succeeded) :-
    setup_canvas_context(_, Ctx),
    Ctx >*> measureText("Hello world") :> TextMetrics,
    TextMetrics >+> width :> W,
    W > 49, W < 50. % W varies for different browsers. Safari = 49.462890625, Firefox on Mac = 49.2000007629394

% moveTo, putImageData tested elsewhere.


% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/quadraticCurveTo
test('CanvasRenderingContext2D', 'quadraticCurveTo of 2d context of canvas draws curved line with control points', succeeded) :-
    setup_canvas_context(_, Ctx),

    StartX = 50, StartY = 20,
    Cp1X= 230, Cp1Y = 30,
    EndX = 50, EndY = 100,
    TwoPI is 2 * pi,

    % Cubic Bézier curve
    Ctx >> [
        beginPath,
        moveTo(StartX, StartY),
        quadraticCurveTo(Cp1X, Cp1Y, EndX, EndY),
        stroke
    ],

    % Start and end points
    Ctx >> [
        fillStyle <:+ blue,
        beginPath,
        arc(StartX, StartY, 5, 0, TwoPI),   % start point
        arc(EndX, EndY, 5, 0, TwoPI),       % end point
        fill
    ],

    % Control points
    Ctx >> [
        fillStyle <:+ red,
        beginPath,
        arc(Cp1X, Cp1Y, 5, 0, TwoPI),  % Control point
        fill
    ].

% rect tested elsewhere.


% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/restore
test('CanvasRenderingContext2D', 'restore/save of 2d context of canvas display green and black squares', succeeded) :-
    setup_canvas_context(_, Ctx),
    Ctx >> [save,
        fillStyle <:+ green,
        fillRect(10, 10, 100, 100),
        restore,
        fillRect(150, 40, 100, 100)
        ].

% save tested elsewhere.

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/scale
test('CanvasRenderingContext2D', 'scale of 2d context of canvas displays red and gray rectangles', succeeded) :-
    setup_canvas_context(_, Ctx),

    % Scaled rectangle
    Ctx >> [
            scale(9, 3),
            fillStyle <:+ red,
            fillRect(10, 10, 8, 20)
        ],

    % Reset transformation matrix to the identity matrix
    Ctx >*> setTransform(1, 0, 0, 1, 0, 0),

    % Non-scaled rectangle
    Ctx >> [
        fillStyle <:+ gray,
        fillRect(10, 10, 8, 20)
    ].


% setLineDash tested elsewhere.

% setTransform tested elsewhere.
% stroke tested elsewhere.

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/strokeRect
test('CanvasRenderingContext2D', 'strokeRect of 2d context of canvas displays green rectangle', succeeded) :-
    setup_canvas_context(_, Ctx),

    Ctx >> [
            strokeStyle <:+ green,
            strokeRect(20, 10, 160, 100)
        ].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/strokeRect
test('CanvasRenderingContext2D', 'strokeText of 2d context of canvas displays "Hello world"', succeeded) :-
    setup_canvas_context(_, Ctx),

    Ctx >> [
            font <:+ '50px serif',
            strokeText('Hello world', 50, 90)
        ].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/strokeRect
test('CanvasRenderingContext2D', 'transform of 2d context of canvas displays black parallelogram', succeeded) :-
    setup_canvas_context(_, Ctx),

    Ctx >> [
            transform(1, 0.2, 0.8, 1, 0, 0),
            fillRect(0, 0, 100, 100)
        ].

% Test runs example from https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/strokeRect
test('CanvasRenderingContext2D', 'translate of 2d context of canvas displays gray and red squares', succeeded) :-
    setup_canvas_context(_, Ctx),

    % Moved square
    Ctx >> [
          translate(110, 30),
          fillStyle <:+ red,
          fillRect(0, 0, 80, 80)
      ],

    % Reset current transformation matrix to the identity matrix
    Ctx >*> setTransform(1, 0, 0, 1, 0, 0),

    % Unmoved square
    Ctx >> [
          fillStyle <:+ gray,
          fillRect(0, 0, 80, 80)
      ].

setup_canvas_context(Canvas, Ctx) :-
    Canvas >-> id :> canvas,
    W = 300,
    Canvas >> [width <:+ 100, width <:+ W,getContext('2d') *:> Ctx, height +:> H],
    Ctx >*> clearRect(0, 0, W, H).
