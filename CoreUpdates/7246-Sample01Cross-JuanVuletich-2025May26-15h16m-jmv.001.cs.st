'From Cuis7.3 [latest update: #7245] on 26 May 2025 at 3:27:45 pm'!
!classDefinition: #Sample01Cross category: #'Morphic-Examples'!
PlacedMorph subclass: #Sample01Cross
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Examples'!

!Sample01Cross commentStamp: 'jmv 5/26/2025 15:18:32' prior: 0!
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

Evaluate:
Sample01Cross new openInWorld!


!Sample01Cross methodsFor: 'drawing' stamp: 'jmv 5/26/2025 15:25:44'!
drawOn: aCanvas
	"Feel free to experiment with this code.
	The worst thing that could happen is that your morph breaks. That will only mean extra fun!!
	Sample01Cross new openInHand
	"

	aCanvas strokeWidth: 8 color: Color lightOrange fillColor: Color red do: [
		aCanvas polyLine: {
			100@0.
			140@0.
			140@240.
			100@240.
			100@0 } ].
	aCanvas strokeWidth: 8 color: Color lightOrange fillColor: Color red do: [
		aCanvas polyLine: {
			0@100.
			0@140.
			240@140.
			240@100.
			0@100 } ].! !

!methodRemoval: Sample01Cross #rotationCenter stamp: 'jmv 5/26/2025 15:17:39'!
Sample01Cross removeSelector: #rotationCenter!
!methodRemoval: Sample01Cross #yAxisPointsUp stamp: 'jmv 5/26/2025 15:17:41'!
Sample01Cross removeSelector: #yAxisPointsUp!

!Sample01Cross reorganize!
('drawing' drawOn:)
!

