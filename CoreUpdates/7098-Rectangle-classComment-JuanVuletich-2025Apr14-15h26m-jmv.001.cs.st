'From Cuis7.3 [latest update: #7097] on 14 April 2025 at 3:40:26 pm'!

!Rectangle commentStamp: 'jmv 4/14/2025 15:38:38' prior: 0!
My instances represent a rectangular area of the screen. Arithmetic functions take points as arguments and carry out scaling and translating operations to create new instances of me. Rectangle functions create new instances by determining intersections of rectangles with rectangles.

Instance variables:
	origin	<Point> the coordinates of corner having smallest coordinates (top left in Screen coordinates)
	corner	<Point> the coordinates of corner having largest coordinates (bottom right in Screen coordinates)

Important notes:

 1: only rectangles parallel to reference frame (Screen) can be represented by this class.

 2: the Rectangle is represented by two extremities of one diagonal. By convention, it must be the diagonal:
	from rectangle origin (the point having smallest coordinates in reference frame),
	to rectangle corner (the point having largest coordinates in reference frame).

 3: Screen coordinates conventions are:
	x is horizontal axis, zero at left border, oriented toward right;
	y is vertical axis, zero at top border, oriented toward bottom.

 4: the Rectangle extent is obtained by subtracting rectangle origin to rectangle corner coordinates.
If this leads to a negative width (extent x coordinate) and/or a negative height (extent y coordinate), then the Rectangle is degenerated and considered empty.

 5: Instances of Rectangle contain the top left 'origin', but do not include the bottom right 'corner'. See for instance #containsPoint: . This has been like this since Smalltalk-80, and most likely since earlier. This convention is useful for working with pixel areas on the Display. The Rectangle `0@0 corner: 100@100` will have width = 100 and cover exactly 100 pixels wide. It will include point 0@0, but will not include point 100@100. Rectangles `0@0 corner: 100@100` and `100@0 corner: 200@100` do not overlap!!
!

