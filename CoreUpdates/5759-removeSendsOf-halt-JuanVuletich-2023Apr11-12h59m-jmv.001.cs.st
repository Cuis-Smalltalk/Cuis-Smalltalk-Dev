'From Cuis 6.0 [latest update: #5758] on 11 April 2023 at 1:02:31 pm'!

!FileList methodsFor: 'private' stamp: 'jmv 4/11/2023 12:19:41'!
acceptedStringOrText
	"Answer the contents of the file, reading it first if needed."
	"Possible brevityState values:
		FileList,
		fullFile, briefFile, needToGetFull, needToGetBrief,
		fullHex, briefHex, needToGetFullHex, needToGetBriefHex"

	(listIndex = 0) | (brevityState == #FileList) ifTrue: [ ^ self defaultContents ].  "no file selected"
	brevityState == #fullFile ifTrue: [ ^ acceptedContentsCache ].
	brevityState == #fullHex ifTrue: [ ^ acceptedContentsCache ].
	brevityState == #briefFile ifTrue: [ ^ acceptedContentsCache ].
	brevityState == #briefHex ifTrue: [ ^ acceptedContentsCache ].

	brevityState == #needToGetFullHex ifTrue: [^ self readContentsHex: false ].
	brevityState == #needToGetBriefHex ifTrue: [^ self readContentsHex: true ].

	brevityState == #needToGetFull ifTrue:  [^ self readContentsBrief: false ].
	brevityState == #needToGetBrief ifTrue: [ ^ self readContentsBrief: true ].  "default"

	^''.! !

!FileList methodsFor: 'private' stamp: 'jmv 4/11/2023 12:27:05'!
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first part characters. Don't create a file if it doesn't already exist."
	| fileSize firstPart firstPartSize |
firstPartSize := 1e6.
directory // fileName readStreamDo: [ :f |
	f ifNil: [^ 'For some reason, this file cannot be read'].
	(brevityFlag not or: [(fileSize := f size) <= (1.2*firstPartSize) ]) ifTrue: [
		acceptedContentsCache := f contentsOfEntireFile.
		brevityState := #fullFile.   "don't change till actually read"
		^ acceptedContentsCache ].

	"if brevityFlag is true, don't display long files when first selected"
	firstPart := f next: 1e6.
].
	acceptedContentsCache :=
'File ''{1}'' is {2} characters long.
You may use the ''get'' command to read the entire file.

Here are the first {3} characters...
------------------------------------------
{4}
------------------------------------------
... end of the first {3} characters.' format: {fileName. fileSize. firstPartSize. firstPart}.
	brevityState := #briefFile.   "don't change till actually read"
	^ acceptedContentsCache! !


!Fraction methodsFor: 'printing' stamp: 'jmv 4/11/2023 12:26:06'!
printAsLiteralOn: aStream
	| n d base powers pow2 maximumBase |
	n := numerator.
	d := denominator.
	powers := Dictionary new.
	(pow2 := d lowBit - 1) = 0
		ifTrue: [
			base := 1]
		ifFalse: [
			base := 2.
			powers at: 2 put: pow2.
			d := d >> pow2].
	maximumBase := 36.
	Integer largePrimesUpTo: (d sqrtFloor max: 2) do: [ :p |
		| pow |
		(d rem: p) = 0
			ifTrue: [
				pow := 1.
				[((d := d quo: p) rem: p) = 0] whileTrue: [pow := pow + 1].
				base := base * p.
				base > maximumBase ifTrue: [
					"self halt: 'cannot print in base > ' , maximumBase printString."
					^self printOn: aStream.
					].
				powers at: p put: pow].
		p squared > d
			ifTrue: [ "Decomposition done, we can now print"
				| maximumPower b |
				d = 1 ifFalse: [base := base * d. powers at: d put: 1].
				base > maximumBase ifTrue: [
					"self halt: 'cannot print in base > ' , maximumBase printString."
					^self printOn: aStream.
					].
				base = 2 ifTrue: [base := 10. powers at: 5 put: 0].
				maximumPower := powers detectMax: [:each | each].
				powers keysAndValuesDo: [:k :v | n := n * (k raisedTo: maximumPower - v)].
				b := base.
				[maximumPower > 1 and: [b * base <= maximumBase]]
					whileTrue:
						[b := b * base.
						maximumPower := maximumPower - 1].
				n storeOn: aStream base: b.
				aStream nextPutAll: 'e-'.
				maximumPower storeOn: aStream.
				^self ]].! !


!Class methodsFor: 'private' stamp: 'jmv 4/11/2023 12:21:22'!
becomeCompact
	"Here are the restrictions on compact classes in order for export segments to work:  A compact class index may not be reused.  If a class was compact in a release of Squeak, no other class may use that index.  The class might not be compact later, and there should be nil in its place in the array."
	| cct index |

	Smalltalk isSpur ifTrue: [^ self error: 'No Compact Classes support in Spur'].
	self isWeak ifTrue:[^ self error: 'You must not make a weak class compact'].
	cct := Smalltalk compactClassesArray.
	(self indexIfCompact > 0 or: [cct includes: self])
		ifTrue: [^ self error: self name , 'is already compact'].
	index := cct indexOf: nil
		ifAbsent: [^ self error: 'compact class table is full'].
	"Install this class in the compact class table"
	cct at: index put: self.
	"Update instspec so future instances will be compact"
	format := format + (index bitShift: 11).
	"Make up new instances and become old ones into them"
	self updateInstancesFrom: self.
	"Purge any old instances"
	Smalltalk garbageCollect.! !

!Class methodsFor: 'private' stamp: 'jmv 4/11/2023 12:21:31'!
becomeCompactSimplyAt: index
	"Make me compact, but don't update the instances.  For importing segments."
"Here are the restrictions on compact classes in order for export segments to work:  A compact class index may not be reused.  If a class was compact in a release of Squeak, no other class may use that index.  The class might not be compact later, and there should be nil in its place in the array."
	| cct |

	Smalltalk isSpur ifTrue: [^ self error: 'No Compact Classes support in Spur'].
	self isWeak ifTrue:[^ self error: 'You must not make a weak class compact'].
	cct := Smalltalk compactClassesArray.
	(self indexIfCompact > 0 or: [cct includes: self])
		ifTrue: [^ self error: self name , 'is already compact'].
	(cct at: index) ifNotNil: [^ self error: 'compact table slot already in use'].
	"Install this class in the compact class table"
	cct at: index put: self.
	"Update instspec so future instances will be compact"
	format := format + (index bitShift: 11).
	"Caller must convert the instances"
! !

!Class methodsFor: 'private' stamp: 'jmv 4/11/2023 12:27:42'!
becomeUncompact
	| cct index |
	cct := Smalltalk compactClassesArray.
	(index := self indexIfCompact) = 0
		ifTrue: [^ self].
	(cct includes: self)
		ifFalse: [^ self error:  'inconsistent state of compact classes'].
	"Update instspec so future instances will not be compact"
	format := format - (index bitShift: 11).
	"Make up new instances and become old ones into them"
	self updateInstancesFrom: self.
	"Make sure there are no compact ones left around"
	Smalltalk garbageCollect.
	"Remove this class from the compact class table"
	cct at: index put: nil.
! !


!ClassBuilder methodsFor: 'public' stamp: 'jmv 4/11/2023 12:21:36'!
superclass: aClass
	ephemeronSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have ephemeron semantics, i.e. where the object will be queued for
	finalization when the key (first) inst var is not reachable other than through
	the other fields of ephemerons with unreachable keys."

	"Pre Spur does not support Ephemerons."
	Smalltalk isSpur ifFalse: [
		self error: 'Attempt to create a new Ephemeron Class in a PreSpur image!!'.
		^ self superclass: aClass
			subclass: t instanceVariableNames: f 
			classVariableNames: d poolDictionaries: s category: cat ].

	aClass isPointers ifFalse:
		[^self error: 'cannot make a pointer subclass of a class with non-pointer fields'].
	aClass instSize + f substrings size < 2 ifTrue:
		[^self error: 'cannot make an ephemeron class with less than two named instance varaibles'].
	^self 
		name: t
		subclassOf: aClass
		type: #ephemeron
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat! !


!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'jmv 4/11/2023 12:34:44'!
initialize
	"https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	http://www.unicode.org/L2/L1999/UnicodeData.html
	UnicodeCodePoint initialize
	"

	| fields codePoint codePointName generalCategory canonicalCombiningClass characterDecompositionMapping decomposition composition |
	UppercaseLettersTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	LowercaseLettersTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	SymbolsLikeTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	DiacriticalMarksTable := ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	CanonicalCombiningClasses := Dictionary new.
	DecompositionMapping := Dictionary new.
	CompositionMapping := Dictionary new.
	(DirectoryEntry smalltalkImageDirectory // 'UnicodeData.txt') readStreamDo: [ :strm |
		[strm atEnd] whileFalse: [
			fields := strm nextLine substringsSeparatedBy: $;.
			(#(4 5 6) includes: fields first size) ifFalse: [ fields first print. self error: 'Bad UnicodeData.txt file' ].
			codePoint := ('16r', fields first) asNumber.
			codePointName := fields at: 2.
			generalCategory := fields at: 3.
			codePoint <= 16r1FFFF ifTrue: [
				({ 'Lu'. 'Lt' } includes: generalCategory) ifTrue: [ UppercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Ll'. 'Lo' } includes: generalCategory) ifTrue: [ LowercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Pc'. 'Po'. 'Sm'. 'Sc'. 'Sk'. 'So' } includes: generalCategory) ifTrue: [ SymbolsLikeTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Mn'. 'Mc'. 'Me' } includes: generalCategory) ifTrue: [ DiacriticalMarksTable bitBooleanAt: codePoint+1 put: true ]].
			canonicalCombiningClass := (fields at: 4) asNumber.
			canonicalCombiningClass = 0 ifFalse: [
				CanonicalCombiningClasses at: codePoint put: canonicalCombiningClass ].
			characterDecompositionMapping := fields at: 6.
			(characterDecompositionMapping size > 0 and: [ characterDecompositionMapping first ~= $< ]) ifTrue: [
				decomposition := characterDecompositionMapping substrings collect: [ :s | ('16r', s) asNumber].
				DecompositionMapping at: codePoint put: decomposition.
				decomposition size > 1 ifTrue: [
					composition := CompositionMapping at: decomposition first ifAbsentPut: [Dictionary new].
					composition at: decomposition second put: codePoint.
					decomposition size > 2 ifTrue: [self error: decomposition printString print].
					]].
			"
			uppercaseMapping _ fields at: 13. lowercaseMapping _ fields at: 14. titlecaseMapping _ fields at: 15.
			Random next > 10.999 | (generalCategory = 'So') ifTrue: [
				{codePoint hex. codePointName. generalCategory} print ].
			decimalDigitValue _ fields at: 7. digitValue _ fields at: 8. numericValue _ fields at: 9.
			decimalDigitValue isEmpty & digitValue isEmpty & numericValue isEmpty ifFalse: [
				{'numeric'. codePoint. codePointName. generalCategory. decimalDigitValue. digitValue. numericValue} print].
			"
			]].! !


!BlockClosure methodsFor: 'lambda calculus' stamp: 'jmv 4/11/2023 12:30:06'!
curried
	"
	https://en.wikipedia.org/wiki/Currying
	[ :a :b | a + b ] value: 1 value: 2
	[ :a :b | a + b ] curried value: 1 :: value: 2
	"
	^self argumentCount caseOf: {
		[ 1] -> [[ :arg1 | [ self value: arg1 ]]].
		[ 2] -> [[ :arg1 | [ :arg2 | self value: arg1 value: arg2 ]]].
		[ 3] -> [[ :arg1 | [ :arg2 :arg3 | self value: arg1 value: arg2 value: arg3 ]]].
		[ 4] -> [[ :arg1 | [ :arg2 :arg3 :arg4 | self value: arg1 value: arg2 value: arg3 value: arg4 ]]] }
	otherwise: [ self error: 'can not curry' ]! !

!BlockClosure methodsFor: 'lambda calculus' stamp: 'jmv 4/11/2023 12:30:27'!
withFirstArg: arg1
	"
	https://en.wikipedia.org/wiki/Partial_application

	[ :a :b | a + b ] value: 1 value: 2
	[ :a :b | a + b ] withFirstArg: 1
	([ :a :b | a + b ] withFirstArg: 1) value: 2
	([ :a :b | a + b ] withFirstArg: 1) withFirstArg: 2
	(([ :a :b | a + b ] withFirstArg: 1) withFirstArg: 2) value

	([ :a :b | a - b ] withFirstArg: 1) value: 2
	"
	^self argumentCount caseOf: {
		[ 1] -> [[ self value: arg1 ]].
		[ 2] -> [[ :arg2 | self value: arg1 value: arg2 ]].
		[ 3] -> [[ :arg2 :arg3 | self value: arg1 value: arg2 value: arg3 ]].
		[ 4] -> [[ :arg2 :arg3 :arg4 | self value: arg1 value: arg2 value: arg3 value: arg4 ]] }
	otherwise: [ self error: 'can not un-curry' ]! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 4/11/2023 12:21:03'!
internalizeChangeLog    
		"
		Smalltalk internalizeChangeLog
		"
	"Bring the changes file into a memory-resident filestream, for faster access and freedom from external file system.  1/31/96 sw"

	| reply aFile |
	reply := self confirm:  'CAUTION -- do not undertake this lightly!!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse: [
		^ self inform: 'Okay - abandoned'].

	(aFile := SourceFiles second) ifNil: [
		^ self error: 'Cannot locate Changes file so cannot proceed.'].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, changes file internalized'! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 4/11/2023 12:20:59'!
internalizeSources
		"
		Smalltalk internalizeSources
		"
	"Bring the sources and changes files into memory-resident filestreams, for faster access and freedom from file-system interface.  1/29/96 sw"

	| reply aFile |
	reply := self confirm:  'CAUTION -- do not undertake this lightly!!
If you have backed up your system and
are prepared to face the consequences of
the requested internalization of sources,
hit Yes.  If you have any doubts, hit No
to back out with no harm done.'.

	(reply ==  true) ifFalse: [
		^ self inform: 'Okay - abandoned'].

	(aFile := SourceFiles first) ifNil: [
		^ self error: 'Cannot locate Sources file so cannot proceed.'].
	SourceFiles at: 1 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	(aFile := SourceFiles last) ifNil: [
		^ self error: 'Cannot locate Changes so cannot proceed.'].
	SourceFiles at: 2 put: (ReadWriteStream with: aFile contentsOfEntireFile).

	self inform: 'Okay, sources internalized'! !


!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 4/11/2023 12:32:40'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection := (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = 1 ifTrue: [
		^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue: [
		newName := self request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FileIOAccessor default privateWriteableFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !


!BitBlt methodsFor: 'private' stamp: 'jmv 4/11/2023 12:20:43'!
oldPaintBits
	"Perform the old paint operation, which requires two calls to BitBlt."
	| requestedFill oldMap requestedRule |
	sourceForm depth = 1 ifFalse: [
		^ self error: 'paint operation is only defined for 1-bit deep sourceForms'].

	requestedRule := combinationRule.
	requestedFill := halftoneForm.
	oldMap := colorMap.

	halftoneForm := nil.
	colorMap := Bitmap with: 0 with: 16rFFFFFFFF.	"Map 1's to ALL ones, not just one"
	combinationRule := Form erase.
	self copyBits. 		"Erase the dest wherever the source is 1"

	halftoneForm := requestedFill.
	combinationRule := Form under.
	self copyBits.	"then OR, with whatever color, into the hole"
	colorMap := oldMap.
	combinationRule := requestedRule

" | dot |
dot _ Form dotOfSize: 32.
((BitBlt destForm: Display
		sourceForm: dot
		fillColor: Color lightGray
		combinationRule: Form paint
		destOrigin: Sensor mousePoint
		sourceOrigin: 0@0
		extent: dot extent
		clipRect: Display boundingBox)
		colorMap: (Bitmap with: 0 with: 16rFFFFFFFF)) copyBits"! !


!Morph methodsFor: 'copying' stamp: 'jmv 4/11/2023 12:33:19'!
copyForClipboard
	"The answer is used both for internal (Smalltalk) and external (Host OS) clipboard.
	Some subclasses might need specific behavior."

	self okayToDuplicate ifFalse: [ ^ nil ].
	^self copy
		removeProperty: #contour;
		removeProperty: #contourY0;
		removeProperty: #contourY1;
		yourself! !


!TTFontReader methodsFor: 'processing' stamp: 'jmv 4/11/2023 12:34:00'!
processCompositeGlyphFrom: entry
	"Read a composite glyph from the font data."
	| glyph flags glyphIndex hasInstructions ofsX ofsY iLen geometryTransformation bb bl br bt |
	glyph := TTCompositeGlyph new.
	bl := entry nextShort.
	bt := entry nextShort.
	br := entry nextShort.
	bb := entry nextShort.
	glyph boundsLeft: bl boundsRight: br boundsTop: bt boundsBottom: bb.
	hasInstructions := false.
	[
		flags := entry nextUShort.
		glyphIndex := entry nextUShort + 1.

		(flags allMask: 1 "ARG_1_AND_2_ARE_WORDS") ifTrue: [
			"'Hay Offset short x@y' print."
			ofsX := entry nextShort.
			ofsY := entry nextShort.
		] ifFalse: [
			"'Hay Offset byte x@y' print."
			(ofsX := entry nextByte) > 127 ifTrue: [ofsX := ofsX - 256].
			(ofsY := entry nextByte) > 127 ifTrue: [ofsY := ofsY - 256]].

		(flags allMask: 2 "ARGS_ARE_XY_VALUES") ifFalse: [self error: 'Unsupported TrueType format']. "Unsupported"
		"ROUND_XY_TO_GRID = 4. Ignored"
		(flags anyMask: 8+64+128) ifFalse: [
			geometryTransformation := MorphicTranslation withTranslation: ofsX@ofsY ].
		(flags allMask: 8 "WE_HAVE_A_SCALE") ifTrue: [
			geometryTransformation := AffineTransformation withScale: entry nextShort asFloat / 16r4000 ].
		(flags allMask: 64 "WE_HAVE_AN_X_AND_Y_SCALE") ifTrue: [
			geometryTransformation := AffineTransformation
				withPointScale: (entry nextShort asFloat / 16r4000)@(entry nextShort asFloat / 16r4000)
				position: ofsX@ofsY ].
		(flags allMask: 128 "WE_HAVE_A_TWO_BY_TWO") ifTrue: [
			"2x2 transformation"
			geometryTransformation := AffineTransformation new
				a11: entry nextShort asFloat / 16r4000;
				a21: entry nextShort asFloat / 16r4000;
				a12: entry nextShort asFloat / 16r4000;
				a22: entry nextShort asFloat / 16r4000;
				a13: ofsX;
				a23: ofsY ].

		glyph addGlyph: glyphIndex transformation: geometryTransformation.
		hasInstructions := hasInstructions or: [ flags allMask: 256 "WE_HAVE_INSTRUCTIONS"].
		"Continue as long as the MORE_COMPONENTS bit is set"
		flags allMask: 32 "MORE_COMPONENTS"
	] whileTrue.
	hasInstructions ifTrue: [
		"Ignore them"
		iLen := entry nextUShort.
		entry skip: iLen].
	^glyph! !


!Object reorganize!
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize customizeExplorerContents rawBasicAt: rawBasicAt:put: size yourself)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = ~= hash literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('events-old protocol' addDependent: breakDependents changed: removeDependent:)
('error handling' assert: assert:description: caseError deny: doesNotUnderstand: error: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: someObject)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isByteString isCharacter isClosure isCollection isComplex isContext isFloat isFloatOrFloatComplex isFraction isInteger isInterval isMethodProperties isNumber isObject isPoint isPseudoContext isString isSymbol isUnicodeCodePoint isUnicodeString isVariableBinding name renameTo:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: explore hasContentsInExplorer inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel:)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('updating' changed update:)
('debugging-halt' clearHaltOnce halt halt: haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
!

