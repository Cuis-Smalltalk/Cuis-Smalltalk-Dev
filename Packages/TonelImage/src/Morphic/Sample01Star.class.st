"
This class is a very simple Morph. It shows how to build your own Morph, and some of the features the framework provides.
This morph includes one important method: #drawOn: , that specifies the morph appareance.

You don't need to write any additional code, and yet this morph can:
- Be drawn with high quality, anti-aliased vector graphics, including translucency
- Be picked by the Hand (click and drag) only when you touch it, regardless of complex geometry
- Show the coordinate system in which it is drawn (open the halo with the middle mouse button)
- Be scaled and rotated (use the zoom or rotate halo handle)
- Be copied, explored, inspected, and browsed (use the appropriate halo handle)
- Be embedded in other morphs, and hold embedded morphs (use the menu halo handle)
- Be embedded in text (see below)

This class also includes #yAxisPointsUp. By default, like in most computer software, Y axis points down. By it is nicer to think of it increasing upwards, like in Math. This method allows for that, meaning a nicer #drawOn:.
Additionally, #rotationCenter is not really required, but it helps the star rotate neatly. See what happens if you delete it, and rotate the star.

Evaluate:
Sample01Star new openInWorld

Or try
Sample01Star new copyToClipboard
and then paste into the About Cuis text, or any text editor. See how you can still use the halo to scale and rotate. See how the morph is moved around as you edit the text.
"
Class {
	#name : 'Sample01Star',
	#superclass : 'PlacedMorph',
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'drawing' }
Sample01Star >> drawOn: aCanvas [
	"A 5 point star.
	Feel free to experiment with this code.
	The worst thing that could happen is that your morph breaks. That will only mean extra fun!
	Sample01Star new openInHand
	"

	aCanvas strokeWidth: 12 color: `Color lightOrange` do: [
	"aCanvas fillColor: Color lightOrange do: ["
	"aCanvas strokeWidth: 12 color: Color lightOrange fillColor: Color yellow do: ["
		aCanvas
				moveTo: `(Point rho: 100 theta: 90 degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*2+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*4+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*6+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*8+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: 90 degreesToRadians)`.
		].
]

{ #category : 'geometry' }
Sample01Star >> rotationCenter [
	"Smooth rotation around the origin.
	(Optional. See what happens if you delete this method)"

	^0@0
]

{ #category : 'geometry testing' }
Sample01Star >> yAxisPointsUp [
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^true
]
