'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 20 April 2008 at 7:27:44 pm'!!OldPasteUpMorph methodsFor: 'drawing' stamp: 'jmv 4/20/2008 18:43'!drawOn: aCanvas 
	"Draw in order:
	- background color
	- grid, if any
	- background sketch, if any
	- Update and draw the turtleTrails form. See the comment in updateTrailsForm.
	- cursor box if any

	Later (in drawSubmorphsOn:) I will skip drawing the background sketch."
	"draw background fill"	(self isWorldMorph and: [aCanvas form == Display] and: [(color := self fillStyle) class == TranslucentColor])		ifTrue: [			(BitBlt current toForm: Display)				clipRect: aCanvas clipRect;				copy: self bounds				from: 0@0 in: nil				fillColor: color rule: Form over]		ifFalse: [ super drawOn: aCanvas]. 

	"draw background sketch."
	backgroundMorph ifNotNil: [
		self clipSubmorphs ifTrue: [
			aCanvas clipBy: self clippingBounds
				during: [ :canvas | canvas fullDrawMorph: backgroundMorph ]]
			ifFalse: [ aCanvas fullDrawMorph: backgroundMorph ]]! !!HoleLW methodsFor: 'drawing' stamp: 'jmv 4/20/2008 18:38'!drawOn: aCanvas 	aCanvas form == Display		ifTrue: [			(BitBlt current toForm: Display)				clipRect: aCanvas clipRect;				copy: bounds				from: 0@0 in: nil				fillColor: Color transparent rule: Form over]		ifFalse: [aCanvas fillRectangle: bounds color: Color blue]! !