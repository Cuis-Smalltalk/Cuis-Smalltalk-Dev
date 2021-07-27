'From Cuis 5.0 [latest update: #4702] on 27 July 2021 at 7:25:56 pm'!

!Object methodsFor: 'printing' stamp: 'di 6/20/97 08:57'!
fullPrintString
	"Answer a String whose characters are a description of the receiver."

	^ String streamContents: [:s | self printOn: s]! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
reformatMethodAt: selector 
	| newCodeString method | 
	newCodeString _ (self compilerClass new)
		format: (self sourceCodeAt: selector)
		in: self
		notifying: nil.
	method _ self compiledMethodAt: selector.
	method
		putSource: newCodeString
		fromParseNode: nil
		class: self
		category: (self organization categoryOfElement: selector)
		inFile: 2 priorMethod: method! !


!Float methodsFor: 'mathematical functions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
arcCos
	"Answer the angle in radians."

	^ Halfpi - self arcSin! !

!Float methodsFor: 'mathematical functions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
cos
	"Answer the cosine of the receiver taken as an angle in radians."

	^ (self + Halfpi) sin! !

!Float methodsFor: 'mathematical functions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
log
	"Answer the base 10 logarithm of the receiver."

	^ self ln / Ln10! !

!Float methodsFor: 'mathematical functions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
reciprocal
	^ 1.0 / self! !

!Float methodsFor: 'mathematical functions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
tan
	"Answer the tangent of the receiver taken as an angle in radians."

	^ self sin / self cos! !

!Float methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isFloat
	^ true! !


!CompiledMethod methodsFor: 'initialization' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
copyWithTrailerBytes: bytes
"Testing:
	(CompiledMethod compiledMethodAt: #copyWithTrailerBytes:)
		tempNamesPut: 'copy end '
"
	| copy end start |
	start _ self initialPC.
	end _ self endPC.
	copy _ CompiledMethod newMethod: end - start + 1 + bytes size
				header: self header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	start to: end do: [:i | copy at: i put: (self at: i)].
	1 to: bytes size do: [:i | copy at: end + i put: (bytes at: i)].
	^ copy! !


!Text class methodsFor: 'instance creation' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
string: aString attribute: att
	"Answer an instance of me whose characters are aString.
	att is a TextAttribute."

	^self string: aString attributes: (Array with: att)! !

!Text class methodsFor: 'instance creation' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
string: aString attributes: atts
	"Answer an instance of me whose characters are those of aString.
	atts is an array of TextAttributes."

	^self string: aString runs: (RunArray new: aString size withAll: atts)! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 6/30/2011 09:33'!
objectAt: anInteger
	"PRIVATE -- Read & return the object at a given stream position.  08:18 tk  anInteger is a relative file position. "
	| savedPosn anObject refPosn |

	savedPosn _ byteStream position.		"absolute"
	refPosn _ self getCurrentReference.	"relative position"

	byteStream position: anInteger + basePos.	"was relative"
	anObject _ self next.

	self setCurrentReference: refPosn.		"relative position"
	byteStream position: savedPosn.		"absolute"
	^ anObject! !


!VariableNode methodsFor: 'initialization' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
name: string key: object code: byte
	"Only used for initting std variables, nil, true, false, self, etc."
	name _ string.
	key _ object.
	code _ byte! !


!Form methodsFor: 'pixel access' stamp: 'tk 6/20/96'!
pixelValueAt: aPoint put: pixelValue
	"Store the given raw pixel value at the given point. Typical clients use colorAt:put: to store a color. "

	(BitBlt bitPokerToForm: self) pixelAt: aPoint put: pixelValue.
! !


!StrikeFont methodsFor: 'emphasis' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
bonk: glyphForm with: bonkForm
	"Bonking means to run through the glyphs clearing out black pixels
	between characters to prevent them from straying into an adjacent
	character as a result of, eg, bolding or italicizing"
	"Uses the bonkForm to erase at every character boundary in glyphs."
	| bb offset |
	offset _ bonkForm offset x.
	bb _ BitBlt toForm: glyphForm.
	bb sourceForm: bonkForm; sourceRect: bonkForm boundingBox;
		combinationRule: Form erase; destY: 0.
	1 to: xTable size-1 do: [:i | bb destX: (xTable at: i) + offset; copyBits].
! !


!CharacterScanner methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
setActualFont: aFont
	"Set the basal font to an isolated font reference."

	font _ aFont! !


!Morph methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
stepTime
	"Answer the desired time between steps in milliseconds. This default implementation requests that the 'step' method be called once every second."

	^ 1000! !

!Morph methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
privateOwner: aMorph
	"Private!! Should only be used by methods that maintain the ower/submorph invariant."

	owner _ aMorph.! !

!Morph methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
privateSubmorphs
	"Private!! Use 'submorphs' instead."

	^ submorphs! !


!WorldMorph methodsFor: 'classification' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isWorldMorph

	^ true! !

!WorldMorph methodsFor: 'structure' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
world

	^ self! !

