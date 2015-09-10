'From Cuis 4.2 of 25 July 2013 [latest update: #2464] on 30 August 2015 at 10:14:24.541488 pm'!

!StrikeFont methodsFor: 'building' stamp: 'jmv 8/30/2015 22:14'!
buildFromForm: allGlyphs data: data name: aString

	| x glyph |
	pointSize _ data first asNumber.
	ascent _ data second asNumber.
	descent _ data third asNumber.
	
	minAscii _ 32.
	maxAscii _ 255.
	name _ aString.
	type _ 0.  "ignored for now"
	superscript _ ascent - descent // 3.	
	subscript _ descent - ascent // 3.	
	emphasis _ 0.

	xTable _ (Array new: 258) atAllPut: 0.
	maxWidth _ 0.
	glyphs _ allGlyphs depth > 16 ifTrue: [ allGlyphs asFormOfDepth: 16 ] ifFalse: [ allGlyphs ].
	x _ 0.
	minAscii to: maxAscii+1 do: [ :i |
		x _ (data at: i-minAscii+4) asNumber.
		xTable at: i+1 put: x].
	xTable at: 258 put: x.
	self reset.
	derivativeFonts _ nil.
	
	"Set left arrow and up arrow as default glyphs"
	glyph _ self glyphAt: (Character value: 130).
	self glyphAt: (Character value: 94) put: glyph.
	glyph _ self glyphAt: (Character value: 131).
	self glyphAt: (Character value: 95) put: glyph.! !

