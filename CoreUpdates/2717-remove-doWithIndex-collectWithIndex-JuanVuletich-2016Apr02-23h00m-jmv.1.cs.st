'From Cuis 4.2 of 25 July 2013 [latest update: #2716] on 2 April 2016 at 11:08:54.336293 pm'!

!Object methodsFor: 'copying' stamp: 'jmv 4/2/2016 23:06'!
copySameFrom: otherObject
	"Copy to myself all instance variables named the same in otherObject.
	This ignores otherObject's control over its own inst vars."

	| myInstVars otherInstVars |
	myInstVars := self class allInstVarNames.
	otherInstVars := otherObject class allInstVarNames.
	myInstVars withIndexDo: [:each :index | | match |
		(match := otherInstVars indexOf: each) > 0 ifTrue:
			[self instVarAt: index put: (otherObject instVarAt: match)]].
	1 to: (self basicSize min: otherObject basicSize) do: [:i |
		self basicAt: i put: (otherObject basicAt: i)].
! !

!Object methodsFor: 'printing' stamp: 'jmv 4/2/2016 23:06'!
longPrintOn: aStream
	"Append to the argument, aStream, the names and values of all 
	of the receiver's instance variables."

	self class allInstVarNames withIndexDo: [ :title :index |
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 print: (self instVarAt: index);
		 newLine]! !

!Object methodsFor: 'printing' stamp: 'jmv 4/2/2016 23:06'!
longPrintOn: aStream limitedTo: sizeLimit indent: indent
	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  Limit is the length limit for each inst var."

	self class allInstVarNames withIndexDo: [ :title :index |
		indent timesRepeat: [aStream tab].
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 nextPutAll: 
			((self instVarAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 1));
		 newLine ]! !


!Behavior methodsFor: 'private' stamp: 'jmv 4/2/2016 23:08'!
indexIfCompact
	"If these 5 bits are non-zero, then instances of this class
	will be compact.  It is crucial that there be an entry in
	Smalltalk compactClassesArray for any class so optimized.
	See the msgs becomeCompact and becomeUncompact."
	^ (format bitShift: -11) bitAnd: 16r1F
"
Smalltalk compactClassesArray withIndexDo: 
	[:c :i | c == nil ifFalse:
		[c indexIfCompact = i ifFalse: [self halt]]]
"! !


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 4/2/2016 23:04'!
forceNewFrom: anArray
    "Create a new instance of the class and fill
    its instance variables up with the array."
    | object max |

    object _ self new.
    max _ self instSize.
    anArray withIndexDo: [:each :index |
        index > max ifFalse:
            [object instVarAt: index put: each]].
    ^ object! !


!String methodsFor: '*fileman-core-accessing' stamp: 'jmv 4/2/2016 23:07'!
indexOfFirstPathSeparator
	self
		withIndexDo: [:each :idx | each isPathSeparator
				ifTrue: [^ idx]].
	^ 0! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 4/2/2016 23:05'!
colorFrom: parm
	"Return an instantiated color from parm.  If parm is already a color, return it, else return the result of my performing it if it's a symbol or, if it is a list, it can either be an array of three numbers, which will be interpreted as RGB values, or a list of symbols, the first of which is sent to me and then the others of which are in turn sent to the prior result, thus allowing entries of the form #(blue darker).  Else just return the thing"

	| aColor firstParm |
	(parm is: #Color) ifTrue: [ ^ parm ].
	parm isSymbol ifTrue: [ ^ self perform: parm ].
	(parm isSequenceableCollection and: [ parm size > 0 ])
		ifTrue: [
			firstParm _ parm first.
			firstParm isNumber ifTrue: [
				^ self fromRgbTriplet: parm].
			aColor _ self colorFrom: firstParm.
			parm withIndexDo: [ :sym :ind |
				ind > 1 ifTrue: [
					aColor _ aColor perform: sym ]].
			^ aColor].
	^ parm

"
Color colorFrom: #(blue darker)
Color colorFrom: Color blue darker
Color colorFrom: #blue
Color colorFrom: #(0.0 0.0 1.0)
"! !


!DamageRecorder methodsFor: 'recording' stamp: 'jmv 4/2/2016 23:05'!
recordInvalidRect: requestedRect
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle.
	Rectangles are specified in world coordinates (might be not the same as Display coordinates if there is some origin set on some canvas. Usually there will be none, but we can't be sure)
	"
	"Details: Damaged rectangles are often identical or overlap significantly. In these cases, we merge them to reduce the number of damage rectangles that must be processed when the display is updated. Moreover, above a certain threshold, we ignore the individual rectangles completely, and simply do a complete repaint on the next cycle.
	jmv: Important. There should be no overlapping rectangles in the list. If there are, translucent morphs might be drawn several times, with visible defects.
	WRT performance, the different paths for various intersections seem a bit complicated. I could not find strong evidence of the best way.
	Leave it as it is right now."

	| newRect mergeRect indexToReuse |
	totalRepaint ifTrue: [^ self].  "planning full repaint; don't bother collecting damage"

	indexToReuse _ nil.
	newRect _ requestedRect truncated.
	invalidRects withIndexDo: [ :oldRect :index |
		oldRect ifNil: [
			indexToReuse ifNil: [ indexToReuse _ index ]]
		ifNotNil: [
			"No two rectangles should intersect"
			(oldRect intersects: newRect) ifTrue: [

				"newRect already in. Nothing to do then."
				(oldRect containsRect: newRect) ifTrue: [ 
					^self ].

				"Some oldRect included in newRect. Remove it and continue, as newRect could still intersect others."
				(newRect containsRect: oldRect) ifTrue: [
					invalidRects at: index put: nil.	"Effectively like 'invalidRects remove: rect', but without performance penalty."
					indexToReuse ifNil: [ indexToReuse _ index ]].

				"Merge rectangles if they overlap significantly, i.e. if the merge is not much larger than separated rectangles."
				mergeRect _ (oldRect origin min: newRect origin) corner: (oldRect corner max: newRect corner).
				mergeRect area > (newRect area + oldRect area * 2)
					ifTrue: [
						"Avoid intersections!!"
						newRect areasOutside: oldRect do: [ :nonIntersection |
							"We have brand new newRects. Start all over with each of them."
							self recordInvalidRect: nonIntersection ].
						"newRect no longer needed, then."
						^self ]
					ifFalse: [
						"Merge into newRect, as any overlap with forecoming rectangles in the iteration is detected and treated."
						newRect _ mergeRect.
						invalidRects at: index put: nil.	"Effectively like 'invalidRects remove: rect', but without performance penalty."
						indexToReuse ifNil: [ indexToReuse _ index ]]]]].

	invalidRects size >= 10 ifTrue: [
		"if there are too many separate areas, merge them all"
		mergeRect _ Rectangle merging: invalidRects.
		self reset.
		mergeRect ifNotNil: [
			invalidRects addLast: mergeRect ].
		indexToReuse _ nil ].

	"Add the given rectangle to the damage list"
	indexToReuse
		ifNil: [
			invalidRects addLast: newRect ]
		ifNotNil: [
			invalidRects at: indexToReuse put: newRect ]! !


!DebuggerMethodMap methodsFor: 'accessing' stamp: 'jmv 4/2/2016 23:05'!
tempsAndValuesForContext: aContext
	"Return a string of the temporary variabls and their current values"
	| aStream |
	aStream := WriteStream on: (String new: 100).
	(self tempNamesForContext: aContext) withIndexDo: [ :title :index |
		aStream nextPutAll: title; nextPut: $:; space; tab.
		aContext print: (self namedTempAt: index in: aContext) on: aStream.
		aStream newLine].
	^aStream contents! !


!TextEditor methodsFor: 'attributes' stamp: 'jmv 4/2/2016 23:00'!
changeEmphasisOrAlignment
	"This is a user command, and generates undo"

	| menuStrings aList reply code align menuList startIndex attribute |
	startIndex _ self startIndex.
	aList _ #(normal bold italic underlined struckThrough leftFlush centered rightFlush justified).
	align _ model actualContents alignmentAt: startIndex.
	code _ model actualContents emphasisAt: startIndex.
	menuList _ WriteStream on: Array new.
	menuList nextPut: (code isZero ifTrue:['<on>'] ifFalse:['<off>']), 'normal'.
	menuList nextPutAll: (#(bold italic underlined struckThrough superscript subscript withST80Glyphs) collect: [ :emph |
		(code anyMask: (TextEmphasis perform: emph) emphasisCode)
			ifTrue: [ '<on>', emph asString ]
			ifFalse: [ '<off>', emph asString ]]).
	menuList nextPutAll: (#(leftFlush centered rightFlush justified) withIndexCollect: [ :type :i |
		align = (i-1)
			ifTrue: [ '<on>', type asString ]
			ifFalse: [ '<off>', type asString ]]).
	menuStrings _ menuList contents.
	aList _ #(normal bold italic underlined struckThrough superscript subscript withST80Glyphs leftFlush centered rightFlush justified).
	reply _ (SelectionMenu labelList: menuStrings lines: #(1 8) selections: aList) startUpWithoutKeyboard.
	reply ifNotNil: [
		(#(leftFlush centered rightFlush justified) includes: reply)
			ifTrue: [
				attribute _ TextAlignment perform: reply]
			ifFalse: [
				attribute _ TextEmphasis perform: reply].
		((menuStrings at: (aList indexOf: reply)) beginsWith: '<on>')
			ifTrue: [ self unapplyAttribute: attribute ]
			ifFalse: [ self applyAttribute:  attribute ]].
	^ true! !

!TextEditor methodsFor: 'undo & redo' stamp: 'jmv 4/2/2016 23:00'!
offerUndoHistory
	| index labels current |
	current _ model undoRedoCommandsPosition.
	labels _ model undoRedoCommands withIndexCollect: [ :each :i | 
		(i = current ifTrue: [ '<on>' ] ifFalse: [ '<off>' ]), each printString ].
	labels isEmpty ifFalse: [
		index _ (PopUpMenu
			labelArray: labels
			lines: #()) startUpMenu.
		index = current ifTrue: [ ^self ].
		index = 0 ifTrue: [ ^self ].
		index < current
			ifTrue: [ current - index timesRepeat: [ self undo ]]
			ifFalse: [ index - current timesRepeat: [ self redo ]]]! !


!Form methodsFor: 'analyzing' stamp: 'jmv 4/2/2016 23:06'!
cgForPixelValue: pv orNot: not
	"Return the center of gravity for all pixels of value pv.
	Note:  If orNot is true, then produce the center of gravity for all pixels
	that are DIFFERENT from the supplied (background) value"
	| pixCount weighted xAndY |
	xAndY _ (Array with: (self xTallyPixelValue: pv orNot: not)
					with: (self yTallyPixelValue: pv orNot: not)) collect: [ :profile |	"For both x and y profiles..."
		pixCount _ 0.  weighted _ 0.
		profile withIndexDo: [ :t :i |
			pixCount _ pixCount + t.
			weighted _ weighted + (t*i)].
		pixCount = 0  "Produce average of nPixels weighted by coordinate"
			ifTrue: [0.0]
			ifFalse: [weighted asFloat / pixCount asFloat - 1.0]].

	^ xAndY first @ xAndY last
"
| r f cg |
[Sensor isAnyButtonPressed] whileFalse: [
	r _ Sensor mousePoint extent: 50@50.
	f _ Form extent: r extent depth: 16.
	f fromDisplay: r.
	cg _ f cgForPixelValue: (Color black pixelValueForDepth: f depth) orNot: false.
	f displayAt: 0@0.
	Display fill: (cg extent: 4) fillColor: Color red ]
"! !


!ContextPart methodsFor: 'debugger access' stamp: 'jmv 4/2/2016 23:05'!
tempsAndValuesLimitedTo: sizeLimit indent: indent
	"Return a string of the temporary variabls and their current values"

	| aStream |
	aStream _ WriteStream on: (String new: 100).
	self tempNames
		withIndexDo: [:title :index |
			indent timesRepeat: [aStream tab].
			aStream nextPutAll: title; nextPut: $:; space; tab.
			aStream nextPutAll: 
				((self tempAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 1)).
			aStream newLine].
	^aStream contents! !


!ReferencesExplorerWrapper methodsFor: 'reference chain' stamp: 'jmv 4/2/2016 23:06'!
contents

	| answer |
	answer _ Array new: model paths size.
	model paths withIndexDo:
		[:each :eachIndex |
			| name newWrapper |
			name _ self pathStringFrom: model object to: each object count: eachIndex.
			newWrapper _ self class with: each object name: name model: each parent: self.
			newWrapper finderSkipsClasses: self finderSkipsClasses.
			answer at: eachIndex put: newWrapper.
		].
	^answer! !


!WeightExplorerWrapper methodsFor: 'reference chain' stamp: 'jmv 4/2/2016 23:07'!
contents

	| answer |
	answer _ Array new: model paths size.
	model paths withIndexDo:
		[:each :eachIndex |
			| name newWrapper |
			name _ self weightStringFrom: model object to: each object count: eachIndex.
			newWrapper _ self class with: each object name: name model: each parent: self.
			newWrapper finderSkipsClasses: self finderSkipsClasses.
			answer at: eachIndex put: newWrapper.
		].
	^answer! !


!Message methodsFor: 'stub creation' stamp: 'jmv 4/2/2016 23:06'!
createStubMethod
	| argNames aOrAn argName arg argClassName |
	argNames _ Set new.
	^ String streamContents: [ :s |
		self selector keywords withIndexDo: [ :key :i |
			s nextPutAll: key.
			((key last = $:) or: [self selector isInfix]) ifTrue: [
				arg _ self arguments at: i.
				argClassName _ (arg isKindOf: Class) ifTrue: ['Class'] ifFalse: [arg class name].
				aOrAn _ argClassName first isVowel ifTrue: ['an'] ifFalse: ['a'].
				argName _ aOrAn, argClassName.
				[argNames includes: argName] whileTrue: [argName _ argName, i asString].
				argNames add: argName.
				s nextPutAll: ' '; nextPutAll: argName; space
			].
		].
		s newLine; tab.
		s nextPutAll: 'self shouldBeImplemented'
	]! !


!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'jmv 4/2/2016 23:04'!
initializeTables

	FromCharTable _ Array new: 256.	"nils"
	ToCharTable _ Array new: 64.
	($A asciiValue to: $Z asciiValue) withIndexDo: [:val :ind | 
		FromCharTable at: val+1 put: ind-1.
		ToCharTable at: ind put: val asCharacter].
	($a asciiValue to: $z asciiValue) withIndexDo: [:val :ind | 
		FromCharTable at: val+1 put: ind+25.
		ToCharTable at: ind+26 put: val asCharacter].
	($0 asciiValue to: $9 asciiValue) withIndexDo: [:val :ind | 
		FromCharTable at: val+1 put: ind+25+26.
		ToCharTable at: ind+26+26 put: val asCharacter].
	FromCharTable at: $+ asciiValue + 1 put: 62.
	ToCharTable at: 63 put: $+.
	FromCharTable at: $/ asciiValue + 1 put: 63.
	ToCharTable at: 64 put: $/.
	! !


!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 4/2/2016 23:07'!
storeInstVarsIn: anObject from: dict
	"For instance variables with the same names, store them in the new instance.  Values in variable-length part also.  This is NOT the normal inst var transfer!!  See Object.readDataFrom:size:.  This is for when inst var names have changed and some additional conversion is needed.  Here we handle the unchanged vars.  "

	(anObject class allInstVarNames) withIndexDo: [:varName :index |
		(dict includesKey: varName) ifTrue: [
			anObject instVarAt: index put: (dict at: varName)]].
	"variable part"
	(dict includesKey: #SizeOfVariablePart) ifFalse: [^ anObject].
	1 to: (dict at: #SizeOfVariablePart) do: [:index | 
		anObject basicAt: index put: (dict at: index)].
	^ anObject! !

!methodRemoval: Set #doWithIndex:!
Set removeSelector: #doWithIndex:!
!methodRemoval: SequenceableCollection #collectWithIndex:!
SequenceableCollection removeSelector: #collectWithIndex:!
!methodRemoval: SequenceableCollection #doWithIndex:!
SequenceableCollection removeSelector: #doWithIndex:!
