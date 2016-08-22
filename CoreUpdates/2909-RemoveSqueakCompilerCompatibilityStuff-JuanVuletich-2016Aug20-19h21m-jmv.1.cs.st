'From Cuis 4.2 of 25 July 2013 [latest update: #2907] on 20 August 2016 at 7:42:07 pm'!

!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 1/26/2016 11:00'!
defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame oldFilledWidth oldFilledWidth2 |
	f _ AbstractFont default.
	h _ f height * 3//2.
	textWidth _ (f widthOfString: progressTitle) + h.
	width _ 150 max: textWidth.

	textForm _ Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).

	barFrame _ Rectangle center: aPoint extent: width@h.
	textFrame _ 0@0 extent: width@h.
	textFrame _ textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + (0@2).
	outerFrame _ barFrame merge: textFrame.
	delta _ outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame _ barFrame translatedBy: delta.
	textFrame _ textFrame translatedBy: delta.
	outerFrame _ outerFrame translatedBy: delta.
	range _ maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame _ barFrame insetBy: 2.
	textForm displayAt: textFrame topLeft.
	Display fillBlack: barFrame.
	Display fillWhite: innerBarFrame.
	oldFilledWidth _ 0.
	oldFilledWidth2 _ 0.
	result _ workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ currentVal _ barVal ]
			ifNil: [
				currentVal _ currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal _ minVal ]].
		filledWidth _ ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		filledWidth  > oldFilledWidth ifTrue: [
			textForm displayAt: textFrame topLeft.
			Display fillBlack: barFrame.
			Display fillWhite: innerBarFrame.
			Display fillGray: (barFrame topLeft + (2@2) extent: filledWidth@17).
			filledWidth + 20 > oldFilledWidth2
				ifFalse: [ DisplayScreen screenUpdateRequired: outerFrame ]
				ifTrue: [ Display forceToScreen: outerFrame. oldFilledWidth2 _ filledWidth ].
			oldFilledWidth _ filledWidth ]].
	"Not pretty at all!!"
	WorldState addDeferredUIMessage: [
		self runningWorld ifNotNil: [ :w |
			w invalidateDisplayRect: outerFrame from: nil ]].
	self resume: result! !

!methodRemoval: PseudoClass #literalScannedAs:environment:notifying:!
PseudoClass removeSelector: #literalScannedAs:environment:notifying:!
!methodRemoval: PseudoClass #newCompiler!
PseudoClass removeSelector: #newCompiler!
!methodRemoval: PseudoClass #newParser!
PseudoClass removeSelector: #newParser!
!methodRemoval: MethodReference #sourceString!
MethodReference removeSelector: #sourceString!
!methodRemoval: Transcript class #cr!
Transcript class removeSelector: #cr!
!methodRemoval: DummyStream #cr!
DummyStream removeSelector: #cr!
!methodRemoval: FileStream class #newFileNamed:!
FileStream class removeSelector: #newFileNamed:!
!methodRemoval: WriteStream #cr!
WriteStream removeSelector: #cr!
!methodRemoval: WriteStream #crtab!
WriteStream removeSelector: #crtab!
!methodRemoval: WriteStream #crtab:!
WriteStream removeSelector: #crtab:!
!methodRemoval: Stream #isStream!
Stream removeSelector: #isStream!
!methodRemoval: SystemDictionary #allClassesAndTraits!
SystemDictionary removeSelector: #allClassesAndTraits!
!methodRemoval: SystemDictionary #undeclared!
SystemDictionary removeSelector: #undeclared!
!methodRemoval: SystemDictionary #valueOf:ifAbsent:!
SystemDictionary removeSelector: #valueOf:ifAbsent:!
!methodRemoval: Text #isText!
Text removeSelector: #isText!
!methodRemoval: CompiledMethod #isCompiledMethod!
CompiledMethod removeSelector: #isCompiledMethod!
!methodRemoval: Symbol #indexOfAnyOf:startingAt:!
Symbol removeSelector: #indexOfAnyOf:startingAt:!
!methodRemoval: String #asOctetString!
String removeSelector: #asOctetString!
!methodRemoval: String #convertFromEncoding:!
String removeSelector: #convertFromEncoding:!
!methodRemoval: String #convertToEncoding:!
String removeSelector: #convertToEncoding:!
!methodRemoval: String #indexOfAnyOf:startingAt:!
String removeSelector: #indexOfAnyOf:startingAt:!
!methodRemoval: String #withCRs!
String removeSelector: #withCRs!
!methodRemoval: SequenceableCollection #indexOfAnyOf:startingAt:!
SequenceableCollection removeSelector: #indexOfAnyOf:startingAt:!
!methodRemoval: CharacterSet #findFirstInByteString:startingAt:!
CharacterSet removeSelector: #findFirstInByteString:startingAt:!
!methodRemoval: Collection #findFirstInByteString:startingAt:!
Collection removeSelector: #findFirstInByteString:startingAt:!
!methodRemoval: BlockContext #cull:cull:!
BlockContext removeSelector: #cull:cull:!
!methodRemoval: BlockClosure #cull:cull:!
BlockClosure removeSelector: #cull:cull:!
!methodRemoval: BlockClosure #displayProgressAt:from:to:during:!
BlockClosure removeSelector: #displayProgressAt:from:to:during:!
!methodRemoval: BlockClosure #printSourceOn:!
BlockClosure removeSelector: #printSourceOn:!
!methodRemoval: BlockClosure #sourceString!
BlockClosure removeSelector: #sourceString!
!methodRemoval: Character #charCode!
Character removeSelector: #charCode!
!methodRemoval: Character #isCharacter!
Character removeSelector: #isCharacter!
!methodRemoval: SmallInteger #isLarge!
SmallInteger removeSelector: #isLarge!
!methodRemoval: LargePositiveInteger #isLarge!
LargePositiveInteger removeSelector: #isLarge!
!methodRemoval: Integer #radix:!
Integer removeSelector: #radix:!
!methodRemoval: Number #asScaledDecimal:!
Number removeSelector: #asScaledDecimal:!
!methodRemoval: Behavior #allowUnderscoreAssignments!
Behavior removeSelector: #allowUnderscoreAssignments!
!methodRemoval: Behavior #bindingOf:environment:!
Behavior removeSelector: #bindingOf:environment:!
!methodRemoval: Behavior #environment!
Behavior removeSelector: #environment!
!methodRemoval: Behavior #literalScannedAs:environment:notifying:!
Behavior removeSelector: #literalScannedAs:environment:notifying:!
!methodRemoval: Behavior #newCompiler!
Behavior removeSelector: #newCompiler!
!methodRemoval: Behavior #newParser!
Behavior removeSelector: #newParser!
!methodRemoval: UndefinedObject #literalScannedAs:environment:notifying:!
UndefinedObject removeSelector: #literalScannedAs:environment:notifying:!
!methodRemoval: MessageSend #cull:cull:!
MessageSend removeSelector: #cull:cull:!
!methodRemoval: Object #deprecated:!
Object removeSelector: #deprecated:!
!methodRemoval: Object #isCharacter!
Object removeSelector: #isCharacter!
!methodRemoval: Object #isCompiledMethod!
Object removeSelector: #isCompiledMethod!
!methodRemoval: Object #isStream!
Object removeSelector: #isStream!
!methodRemoval: Object #isText!
Object removeSelector: #isText!
!classRemoval: #NumberParser!
Smalltalk removeClassNamed: #NumberParser!
!classRemoval: #NumberParserError!
Smalltalk removeClassNamed: #NumberParserError!
!classRemoval: #PluggableDictionary!
Smalltalk removeClassNamed: #PluggableDictionary!
!classRemoval: #ScaledDecimal!
Smalltalk removeClassNamed: #ScaledDecimal!
!classRemoval: #SqNumberParser!
Smalltalk removeClassNamed: #SqNumberParser!
!classRemoval: #SyntaxMorph!
Smalltalk removeClassNamed: #SyntaxMorph!
!classRemoval: #SystemNavigation!
Smalltalk removeClassNamed: #SystemNavigation!
!classRemoval: #TextConstants!
Smalltalk removeClassNamed: #TextConstants!
!classRemoval: #TextStyle!
Smalltalk removeClassNamed: #TextStyle!
!classRemoval: #ToolSet!
Smalltalk removeClassNamed: #ToolSet!
!classRemoval: #UIManager!
Smalltalk removeClassNamed: #UIManager!
!classRemoval: #ZipReadStream!
Smalltalk removeClassNamed: #ZipReadStream!
!classRemoval: #ZipWriteStream!
Smalltalk removeClassNamed: #ZipWriteStream!
