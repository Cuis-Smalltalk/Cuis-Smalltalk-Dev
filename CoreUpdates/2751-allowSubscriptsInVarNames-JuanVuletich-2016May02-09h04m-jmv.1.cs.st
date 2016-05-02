'From Cuis 4.2 of 25 July 2013 [latest update: #2749] on 2 May 2016 at 9:33:29.934455 am'!
"Change Set:		2750-CuisCore-AuthorName-2016May02-09h04m
Date:			2 May 2016
Author:			Juan Vuletich

<your descriptive text goes here>"
StrikeFont allInstancesDo: [ :font | | shortUnderscore |
	shortUnderscore _ font glyphAt: $_.
	shortUnderscore _ shortUnderscore copy: (0@0 extent: 1@shortUnderscore height).
	font glyphAt: (Character value: 127) put: shortUnderscore ].!

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 5/2/2016 09:32'!
useShortUnderscore
	self characterToGlyphMap.
	characterToGlyphMap at: 96 put: 127! !


!Behavior methodsFor: 'private' stamp: 'jmv 5/2/2016 09:16'!
lastUnderscoreMeansSubscript
	"Redefine this method on the class side of those classes where you want
	a_1 to be shown as 'a subscript 1'"

	^ false! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'jmv 5/2/2016 09:04'!
lastIndexOf: anElement startingAt: lastIndex endingAt: firstIndex do: aBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	lastIndex to: firstIndex by: -1 do: [ :index |
		(self at: index) = anElement ifTrue: [^ aBlock value: index]]! !


!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 5/2/2016 09:25'!
makeControlCharsVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self glyphAt: (Character space).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
	self glyphAt: (Character value: 134) put: glyph.
	
	"Keep tab(9), lf(10), cr(13) and space(32) transparent or whatever the user chose"
	#(0 1 2 3 4 5 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27)
		do: [ :ascii |
			characterToGlyphMap at: ascii + 1 put: 134 ]! !

!StrikeFont methodsFor: 'building' stamp: 'jmv 5/2/2016 09:30'!
buildFromForm: allGlyphs data: data name: aString

	| x shortUnderscore |
	pointSize _ data first asNumber.
	ascent _ data second asNumber.
	descent _ data third asNumber.
	
	minAscii _ 28.
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
	
	"Replace glyph for 127 (a box) with a short version of the underscore (used to optionally mark subscript in code)"
	shortUnderscore _ self glyphAt: $_.
	shortUnderscore _ shortUnderscore copy: (0@0 extent: 1@shortUnderscore height).
	self glyphAt: (Character value: 127) put: shortUnderscore! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 5/2/2016 09:32'!
syntheticSubscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ ((AbstractFont familyName: 'DejaVu' aroundPointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative useShortUnderscore.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.

que todos, menos estos, tengan superscript y subscript en cero. Y en estos, apropiado. y en 'aca' usarlo. y listo
"! !


!Metaclass class methodsFor: 'anti-corruption' stamp: 'jmv 5/2/2016 09:15'!
isScarySelector: newbieSelector

	"Return true if newbieSelector is already a part of Metaclass protocol."
	(#(new new: lastUnderscoreMeansSubscript) pointsTo: newbieSelector) ifTrue: [ ^false ].
	(Metaclass includesSelector: newbieSelector) ifTrue: [^ true].
	(ClassDescription includesSelector: newbieSelector) ifTrue: [^ true].
	(Behavior includesSelector: newbieSelector) ifTrue: [^ true].
	^ false
! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 5/2/2016 09:14'!
setAttributesFromRanges: ranges

	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	ranges do: [ :range |

		(self attributesFor: range rangeType) ifNotNil: [ :attributes |
			attributes do: [ :each |
				formattedText addAttribute: each from: range start to: range end ]].

		classOrMetaClass ifNotNil: [
			classOrMetaClass theNonMetaClass lastUnderscoreMeansSubscript ifTrue: [
				(#( instVar classVar globalVar workspaceVar poolConstant
					patternArg methodArg patternTempVar tempVar		
					blockPatternArg blockArg blockPatternTempVar blockTempVar 
					incompleteIdentifier undefinedIdentifier) pointsTo: range rangeType )
						ifTrue: [
							formattedText lastIndexOf: $_ startingAt: range end endingAt: range start do: [ :i |
								formattedText addAttribute: ShoutTextEmphasis subscript from: i to: range end ] ]]]]! !

