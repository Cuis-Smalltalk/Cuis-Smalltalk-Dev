'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 6 November 2008 at 6:31:19 pm'!!Object methodsFor: 'user interface' stamp: 'di 5/11/1999 22:01'!modelWakeUp	"A window with me as model is being entered or expanded.  Default response is no-op" ! !!Object methodsFor: 'user interface' stamp: 'sw 10/16/1999 22:45'!modelWakeUpIn: aWindow	"A window with me as model is being entered or expanded.  Default response is no-op" 	self modelWakeUp! !!CodeHolder methodsFor: 'misc' stamp: 'jmv 11/6/2008 14:43'!modelWakeUp	"A window with me as model is being entered or expanded.  Default response is no-op" ! !!CodeHolder methodsFor: 'misc' stamp: 'sw 9/27/2001 01:26'!modelWakeUpIn: aWindow	"The window has been activated.  Respond to possible changes that may have taken place while it was inactive"	self updateListsAndCodeIn: aWindow.	self decorateButtons.	self refreshAnnotation.	super modelWakeUpIn: aWindow! !!Browser methodsFor: 'initialize-release' stamp: 'jmv 11/6/2008 14:30'!buildMorphicSwitches
	| instanceSwitch divider1 divider2 commentSwitch classSwitch row aColor |
	instanceSwitch := OldPluggableButtonMorph 
				on: self
				getState: #instanceMessagesIndicated
				action: #indicateInstanceMessages.
	instanceSwitch
		label: 'instance';
		askBeforeChanging: true;
		borderWidth: 0.
	commentSwitch := OldPluggableButtonMorph 
				on: self
				getState: #classCommentIndicated
				action: #plusButtonHit.
	commentSwitch
		label: '?' asText allBold;
		askBeforeChanging: true;
		setBalloonText: 'class comment';
		borderWidth: 0.
	classSwitch := OldPluggableButtonMorph 
				on: self
				getState: #classMessagesIndicated
				action: #indicateClassMessages.
	classSwitch
		label: 'class';
		askBeforeChanging: true;
		borderWidth: 0.
	divider1 := OldBorderedSubpaneDividerMorph vertical.
	divider2 := OldBorderedSubpaneDividerMorph vertical.
	Preferences alternativeWindowLook 
		ifTrue: 
			[divider1
				extent: 4 @ 4;
				borderWidth: 2;
				borderRaised;
				color: Color transparent.
			divider2
				extent: 4 @ 4;
				borderWidth: 2;
				borderRaised;
				color: Color transparent].
	row := (OldAlignmentMorph newRow)
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				layoutInset: 0;
				borderWidth: 0;
				addMorphBack: instanceSwitch;
				addMorphBack: divider1;
				addMorphBack: commentSwitch;
				addMorphBack: divider2;
				addMorphBack: classSwitch.
	aColor := Color colorFrom: self class windowColor.
	row color: aColor duller.	"ensure matching button divider color. (see #paneColor)"
	Preferences alternativeWindowLook ifTrue: [aColor := aColor muchLighter].
	{ 
		instanceSwitch.
		commentSwitch.
		classSwitch} do: 
				[:m | 
				m
					color: aColor;
					onColor: aColor twiceDarker offColor: aColor;
					hResizing: #spaceFill;
					vResizing: #spaceFill].
	^row! !!ChangeSorter methodsFor: 'changeSet menu' stamp: 'tk 6/10/1999 12:44'!fileOut	"File out the current change set."	myChangeSet fileOut.	parent modelWakeUp.	"notice object conversion methods created"! !!LightWidget methodsFor: 'halos and balloon help' stamp: 'jmv 3/24/2008 16:14'!transferHalo: event from: formerHaloOwner	"Progressively transfer the halo to the next likely recipient"	| localEvt w |	"Never transfer halo to top-most world"	(self isWorldMorph and:[owner isNil]) ifFalse:[		(self wantsHaloFromClick and:[formerHaloOwner ~~ self]) 			ifTrue:[^self addHalo: event from: formerHaloOwner]].	event shiftPressed ifTrue:[		"Pass it outwards"		owner ifNotNil:[^owner transferHalo: event from: formerHaloOwner].		"We're at the top level; throw the event back in to find recipient"		formerHaloOwner removeHalo.		^self processEvent: event copy resetHandlerFields.	].	"We're at the bottom most level; throw the event back up to the root to find recipient"	formerHaloOwner removeHalo.	(w _ self world) ifNil: [ ^self ].	localEvt _ event transformedBy: (self transformedFrom: w) inverseTransformation.	^w processEvent: localEvt resetHandlerFields.! !!MethodFinder methodsFor: 'initialize' stamp: 'jmv 11/6/2008 14:54'!initialize
	"The methods we are allowed to use.  (MethodFinder new initialize) "

	Approved _ Set new.
	AddAndRemove _ Set new.
	Blocks _ Set new.
	"These modify an argument and are not used by the MethodFinder: longPrintOn: printOn: storeOn: sentTo: storeOn:base: printOn:base: absPrintExactlyOn:base: absPrintOn:base: absPrintOn:base:digitCount: writeOn: writeScanOn: possibleVariablesFor:continuedFrom: printOn:format:"

"Object"  
	#("in class, instance creation" categoryForUniclasses chooseUniqueClassName initialInstance  newFrom: readCarefullyFrom:
"accessing" at: basicAt: basicSize bindWithTemp: in: size yourself 
"testing" basicType ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isColor isFloat isFraction isInMemory isInteger isMorph isNil isNumber isPoint isPseudoContext isText isTransparent isWebBrowser knownName notNil pointsTo: wantsSteps 
"comparing" = == closeTo: hash hashMappedBy: identityHash identityHashMappedBy: identityHashPrintString ~= ~~ 
"copying" clone copy shallowCopy 
"dependents access" canDiscardEdits dependents hasUnacceptedEdits 
"updating" changed changed: okToChange update: windowIsClosing 
"printing" fullPrintString isLiteral longPrintString printString storeString stringForReadout stringRepresentation 
"class membership" class isKindOf: isKindOf:orOf: isMemberOf: respondsTo: xxxClass 
"error handling" 
"user interface" defaultLabelForInspector initialExtent modelWakeUp 
"system primitives" asOop instVarAt: instVarNamed: 
"private" 
"associating" -> 
"converting" as: asOrderedCollection asString 
"casing" caseOf: caseOf:otherwise: 
"binding" bindingOf: 
"macpal" contentsChanged currentEvent currentHand currentWorld flash ifKindOf:thenDo: instanceVariableValues 
"flagging" flag: 
"translation support" "objects from disk" "finalization" ) do: [:sel | Approved add: sel].
	#(at:add: at:modify: at:put: basicAt:put: "NOT instVar:at:"
"message handling" perform: perform:orSendTo: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: 
) do: [:sel | AddAndRemove add: sel].

"Boolean, True, False, UndefinedObject"  
	#("logical operations" & eqv: not xor: |
"controlling" and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or:
"copying" 
"testing" isEmptyOrNil) do: [:sel | Approved add: sel].

"Behavior" 
	#("initialize-release"
"accessing" compilerClass decompilerClass evaluatorClass format methodDict parserClass sourceCodeTemplate subclassDefinerClass
"testing" instSize instSpec isBits isBytes isFixed isPointers isVariable isWeak isWords
"copying"
"printing" printHierarchy
"creating class hierarchy"
"creating method dictionary"
"instance creation" basicNew basicNew: new new:
"accessing class hierarchy" allSubclasses allSubclassesWithLevelDo:startingLevel: allSuperclasses subclasses superclass withAllSubclasses withAllSuperclasses
"accessing method dictionary" allSelectors changeRecordsAt: compiledMethodAt: compiledMethodAt:ifAbsent: firstCommentAt: lookupSelector: selectors selectorsDo: selectorsWithArgs: "slow but useful ->" sourceCodeAt: sourceCodeAt:ifAbsent: sourceMethodAt: sourceMethodAt:ifAbsent:
"accessing instances and variables" allClassVarNames allInstVarNames allSharedPools classVarNames instVarNames instanceCount sharedPools someInstance subclassInstVarNames
"testing class hierarchy" inheritsFrom: kindOfSubclass
"testing method dictionary" canUnderstand: classThatUnderstands: hasMethods includesSelector: scopeHas:ifTrue: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:
"enumerating"
"user interface"
"private" indexIfCompact) do: [:sel | Approved add: sel].

"ClassDescription"
	#("initialize-release" 
"accessing" classVersion isMeta name theNonMetaClass
"copying" 
"printing" classVariablesString instanceVariablesString sharedPoolsString
"instance variables" checkForInstVarsOK: 
"method dictionary" 
"organization" category organization whichCategoryIncludesSelector:
"compiling" acceptsLoggingOfCompilation wantsChangeSetLogging
"fileIn/Out" definition
"private" ) do: [:sel | Approved add: sel].

"Class"
	#("initialize-release" 
"accessing" classPool
"testing"
"copying" 
"class name" 
"instance variables" 
"class variables" classVarAt: classVariableAssociationAt:
"pool variables" 
"compiling" 
"subclass creation" 
"fileIn/Out" ) do: [:sel | Approved add: sel]. 

"Metaclass"
	#("initialize-release" 
"accessing" soleInstance
"copying" "instance creation" "instance variables"  "pool variables" "class hierarchy"  "compiling"
"fileIn/Out"  nonTrivial ) do: [:sel | Approved add: sel].

"Context, BlockContext"
	#(receiver client method receiver tempAt: 
"debugger access" mclass pc selector sender shortStack sourceCode tempNames tempsAndValues
"controlling"  "printing" "system simulation" 
"initialize-release" 
"accessing" hasMethodReturn home numArgs
"evaluating" value value:ifError: value:value: value:value:value: value:value:value:value: valueWithArguments:
"controlling"  "scheduling"  "instruction decoding"  "printing" "private"  "system simulation" ) do: [:sel | Approved add: sel].
	#(value: "<- Association has it as a store" ) do: [:sel | AddAndRemove add: sel].

"Message"
	#("inclass, instance creation" selector: selector:argument: selector:arguments:
"accessing" argument argument: arguments sends:
"printing" "sending" ) do: [:sel | Approved add: sel].
	#("private" setSelector:arguments:) do: [:sel | AddAndRemove add: sel].

"Magnitude"
	#("comparing" < <= > >= between:and:
"testing" max: min: min:max: ) do: [:sel | Approved add: sel].

"Date, Time"
	#("in class, instance creation" fromDays: fromSeconds: fromString: newDay:month:year: newDay:year: today
	"in class, general inquiries" dateAndTimeNow dayOfWeek: daysInMonth:forYear: daysInYear: firstWeekdayOfMonth:year: indexOfMonth: leapYear: nameOfDay: nameOfMonth:
"accessing" day leap monthIndex monthName weekday year
"arithmetic" addDays: subtractDate: subtractDays:
"comparing"
"inquiries" dayOfMonth daysInMonth daysInYear daysLeftInYear firstDayOfMonth previous:
"converting" asSeconds
"printing" mmddyy mmddyyyy printFormat: 
"private" firstDayOfMonthIndex: weekdayIndex 
	"in class, instance creation" fromSeconds: now 
	"in class, general inquiries" dateAndTimeFromSeconds: dateAndTimeNow millisecondClockValue millisecondsToRun: totalSeconds
"accessing" hours minutes seconds
"arithmetic" addTime: subtractTime:
"comparing"
"printing" intervalString print24 
"converting") do: [:sel | Approved add: sel].
	#("private" hours: hours:minutes:seconds: day:year: 
		 ) do: [:sel | AddAndRemove add: sel].

"Number"
	#("in class" readFrom:base: 
"arithmetic" * + - / // \\ abs negated quo: reciprocal rem:
"mathematical functions" arcCos arcSin arcTan arcTan: cos exp floorLog: ln log log: raisedTo: raisedToInteger: sin sqrt squared tan
"truncation and round off" ceiling detentBy:atMultiplesOf:snap: floor roundTo: roundUpTo: rounded truncateTo: truncated
"comparing"
"testing" even isDivisibleBy: isInf isInfinite isNaN isZero negative odd positive sign strictlyPositive
"converting" @ asInteger asNumber asPoint asSmallAngleDegrees degreesToRadians radiansToDegrees
"intervals" to: to:by: 
"printing" printStringBase: storeStringBase: ) do: [:sel | Approved add: sel].

"Integer"
	#("in class" primesUpTo:
"testing" isPowerOfTwo
"arithmetic" alignedTo:
"comparing"
"truncation and round off" atRandom normalize
"enumerating" timesRepeat:
"mathematical functions" degreeCos degreeSin factorial gcd: lcm: take:
"bit manipulation" << >> allMask: anyMask: bitAnd: bitClear: bitInvert bitInvert32 bitOr: bitShift: bitXor: lowBit noMask:
"converting" asCharacter asColorOfDepth: asFloat asFraction asHexDigit
"printing" asStringWithCommas hex hex8 radix:
"system primitives" lastDigit replaceFrom:to:with:startingAt:
"private" "benchmarks" ) do: [:sel | Approved add: sel].

"SmallInteger, LargeNegativeInteger, LargePositiveInteger"
	#("arithmetic" "bit manipulation" highBit "testing" "comparing" "copying" "converting" "printing" 
"system primitives" digitAt: digitLength 
"private" fromString:radix: ) do: [:sel | Approved add: sel].
	#(digitAt:put: ) do: [:sel | AddAndRemove add: sel].

"Float"
	#("arithmetic"
"mathematical functions" reciprocalFloorLog: reciprocalLogBase2 timesTwoPower:
"comparing" "testing"
"truncation and round off" exponent fractionPart integerPart significand significandAsInteger
"converting" asApproximateFraction asIEEE32BitWord asTrueFraction
"copying") do: [:sel | Approved add: sel].

"Fraction, Random"
	#(denominator numerator reduced next nextValue) do: [:sel | Approved add: sel].
	#(setNumerator:denominator:) do: [:sel | AddAndRemove add: sel].

"Collection"
	#("accessing" anyOne
"testing" includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: isEmpty isSequenceable occurrencesOf:
"enumerating" collect: collect:thenSelect: count: detect: detect:ifNone: detectMax: detectMin: detectSum: inject:into: reject: select: select:thenCollect:
"converting" asBag asCharacterSet asSet asSortedArray asSortedCollection asSortedCollection:
"printing"
"private" maxSize
"arithmetic"
"math functions" average max median min range sum) do: [:sel | Approved add: sel].
	#("adding" add: addAll: addIfNotPresent:
"removing" remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat: remove:ifAbsent:) do: [:sel | AddAndRemove add: sel].

"SequenceableCollection"
	#("comparing" hasEqualElements:
"accessing" allButFirst allButLast at:ifAbsent: atAll: atPin: atRandom: atWrap: fifth first fourth identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt:ifAbsent: indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last second sixth third
"removing"
"copying" , copyAfterLast: copyAt:put: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpTo: copyUpToLast: copyWith: copyWithout: copyWithoutAll: forceTo:paddingWith: shuffled sortBy:
"enumerating" collectWithIndex: findFirst: findLast: pairsCollect: with:collect: withIndexCollect: polynomialEval:
"converting" asArray asDictionary asFloatArray asIntegerArray asStringWithCr asWordArray reversed
"private" copyReplaceAll:with:asTokens: ) do: [:sel | Approved add: sel].
	#( swap:with:) do: [:sel | AddAndRemove add: sel].

"ArrayedCollection, Bag"
	#("private" defaultElement 
"sorting" isSorted
"accessing" cumulativeCounts sortedCounts sortedElements "testing" "adding" add:withOccurrences: "removing" "enumerating" 
	) do: [:sel | Approved add: sel].
	#( mergeSortFrom:to:by: sort sort: add: add:withOccurrences:
"private" setDictionary ) do: [:sel | AddAndRemove add: sel].

"Other messages that modify the receiver"
	#(atAll:put: atAll:putAll: atAllPut: atWrap:put: replaceAll:with: replaceFrom:to:with:  removeFirst removeLast) do: [:sel | AddAndRemove add: sel].

	self initialize2.

"
MethodFinder new initialize.
MethodFinder new organizationFiltered: Set
"

! !!OldMorph methodsFor: 'e-toy support' stamp: 'jmv 11/6/2008 14:31'!embeddedInMorphicWindowLabeled: labelString 
	| window |
	window := (OldSystemWindow labelled: labelString) model: nil.
	window setStripeColorsFrom: nil class windowColor.
	window addMorph: self frame: (0 @ 0 extent: 1 @ 1).
	^window! !!OldMorph methodsFor: 'halos and balloon help' stamp: 'jmv 11/12/2006 22:06'!transferHalo: event from: formerHaloOwner	"Progressively transfer the halo to the next likely recipient"	| localEvt w |	"Never transfer halo to top-most world"	(self isWorldMorph and:[owner isNil]) ifFalse:[		(self wantsHaloFromClick and:[formerHaloOwner ~~ self]) 			ifTrue:[^self addHalo: event from: formerHaloOwner]].	event shiftPressed ifTrue:[		"Pass it outwards"		owner ifNotNil:[^owner transferHalo: event from: formerHaloOwner].		"We're at the top level; throw the event back in to find recipient"		formerHaloOwner removeHalo.		^self processEvent: event copy resetHandlerFields.	].	self submorphsDo:[:m|		localEvt _ event transformedBy: (m transformedFrom: self).		(m fullContainsPoint: localEvt position) 			ifTrue:[^m transferHalo: event from: formerHaloOwner].	].	"We're at the bottom most level; throw the event back up to the root to find recipient"	formerHaloOwner removeHalo.	(w _ self world) ifNil: [ ^self ].	localEvt _ event transformedBy: (self transformedFrom: w) inverseTransformation.	^w processEvent: localEvt resetHandlerFields.! !!OldMorph methodsFor: 'macpal' stamp: 'sw 10/10/1999 10:23'!flash	| c w |	c _ self color.	self color: Color black.	(w _ self world) ifNotNil: [w displayWorldSafely].	self color: c! !!OldHaloMorph methodsFor: 'events' stamp: 'ar 10/10/2000 19:09'!transferHalo: event	"Transfer the halo to the next likely recipient"	target ifNil:[^self delete].	target transferHalo: (event transformedBy: (target transformedFrom: self)) from: target.! !!OldHaloMorph methodsFor: 'meta-actions' stamp: 'jmv 11/6/2008 13:59'!blueButtonDown: event	"Transfer the halo to the next likely recipient"	target ifNil:[^self delete].	event hand obtainHalo: self.	positionOffset _ event position - (target point: target position in: owner).	"wait for drags or transfer"	event hand 		waitForClicksOrDrag: self 		event: event		selectors: { #transferHalo:. nil. nil. #dragTarget:. }		threshold: 5.! !!OldHandMorph methodsFor: 'events-processing' stamp: 'jmv 11/6/2008 15:37'!handleEvent: anEvent	| evt ofs k |	owner ifNil:[^self].	evt _ anEvent.	EventStats ifNil:[EventStats _ IdentityDictionary new].	EventStats at: #count put: (EventStats at: #count ifAbsent:[0]) + 1.	EventStats at: evt type put: (EventStats at: evt type ifAbsent:[0]) + 1.	evt isMouseOver ifTrue:[^self sendMouseEvent: evt].ShowEvents == true ifTrue:[	Display fill: (0@0 extent: 450@120) rule: Form over fillColor: Color white.	ofs _ (owner hands indexOf: self) - 1 * 60.	evt printString displayAt: (0@ofs) + (evt isKeyboard ifTrue:[0@30] ifFalse:[0@0]).	(self keyboardFocus printString,		'    ', self navigationFocus printString) displayAt: (0@ofs)+(0@45).].	"Notify listeners"	self sendListenEvent: evt to: self eventListeners.	evt isKeyboard ifTrue:[		(evt commandKeyPressed and: [evt isKeystroke]) ifTrue: ["			(navigationFocus notNil and: [ navigationFocus seizesNavigationFocus ]) ifFalse: ["				k _ evt keyValue.				k = 28 ifTrue: [					self navigationFocusOut.					^self mouseOverHandler processMouseOver: lastMouseEvent].				k = 29 ifTrue: [					self navigationFocusIn.					^self mouseOverHandler processMouseOver: lastMouseEvent].				k = 30 ifTrue: [					self navigationFocusPrevious.					^self mouseOverHandler processMouseOver: lastMouseEvent].				k = 31 ifTrue: [					self navigationFocusNext.					^self mouseOverHandler processMouseOver: lastMouseEvent]."			]."		].		self sendListenEvent: evt to: self keyboardListeners.		self sendKeyboardEvent: evt.		^self mouseOverHandler processMouseOver: lastMouseEvent].	evt isDropEvent ifTrue:[		self sendEvent: evt.		^self mouseOverHandler processMouseOver: lastMouseEvent].	evt isMouse ifTrue:[		self sendListenEvent: evt to: self mouseListeners.		lastMouseEvent _ evt].	"Check for pending drag or double click operations."	mouseClickState ifNotNil:[		(mouseClickState handleEvent: evt from: self) ifFalse:[			"Possibly dispatched #click: or something and will not re-establish otherwise"			^self mouseOverHandler processMouseOver: lastMouseEvent]].	evt isMove ifTrue:[		self position: evt position.		self sendMouseEvent: evt.	] ifFalse:[		"Issue a synthetic move event if we're not at the position of the event"		(evt position = self position) ifFalse:[self moveToEvent: evt].		"Drop submorphs on button events"		(self hasSubmorphs) 			ifTrue:[self dropMorphs: evt]			ifFalse:[self sendMouseEvent: evt].	].	ShowEvents == true ifTrue:[self mouseFocus printString displayAt: (0@ofs) + (0@15)].	self mouseOverHandler processMouseOver: lastMouseEvent.! !!OldHandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 11/6/2008 15:37'!dropMorph: aMorph event: anEvent	"Drop the given morph which was carried by the hand"	| event |	(anEvent isMouseUp and:[aMorph shouldDropOnMouseUp not]) ifTrue:[^self].	"Note: For robustness in drag and drop handling we remove the morph BEFORE we drop him, but we keep his owner set to the hand. This prevents system lockups when there is a problem in drop handling (for example if there's an error in #wantsToBeDroppedInto:). THIS TECHNIQUE IS NOT RECOMMENDED FOR CASUAL USE."	self privateRemove: aMorph.	aMorph privateOwner: self.	event _ DropEvent new setPosition: self position contents: aMorph hand: self.	self sendEvent: event.	event wasHandled ifFalse:[aMorph rejectDropMorphEvent: event].	aMorph owner == self ifTrue:[aMorph delete].	self mouseOverHandler processMouseOver: anEvent.! !!OldHandMorph methodsFor: 'private events' stamp: 'jmv 11/6/2008 15:35'!sendEvent: anEvent	"Send the event to the morph currently holding the focus, or if none to the owner of the hand."		| result |	ActiveEvent _ anEvent.	result _ owner processEvent: anEvent.	ActiveEvent _ nil.	^result! !!OldHandMorph methodsFor: 'private events' stamp: 'jmv 11/6/2008 15:40'!sendFocusEvent: anEvent to: focusHolder in: world	"Send the event to focusHolder, the morph currently holding the focus"	| result |	world becomeActiveDuring: [		ActiveHand _ self.		ActiveEvent _ anEvent.		result _ focusHolder handleFocusEvent: 			(anEvent transformedBy: (focusHolder transformedFrom: self)).	].	^result! !!OldHandMorph methodsFor: 'private events' stamp: 'jmv 11/6/2008 15:42'!sendKeyboardEvent: anEvent	"Send the event to the morph currently holding the focus, or if none to the owner of the hand."		| w |	keyboardFocus ifNotNil: [		(w _ keyboardFocus world) ifNil: [			keyboardFocus _ nil.			^self ].		^self sendFocusEvent: anEvent to: keyboardFocus in: w].	^self sendEvent: anEvent! !!OldHandMorph methodsFor: 'private events' stamp: 'jmv 11/6/2008 15:42'!sendMouseEvent: anEvent	"Send the event to the morph currently holding the focus, or if none to the owner of the hand."	| w |	mouseFocus ifNotNil: [		(w _ mouseFocus world) ifNil: [			mouseFocus _ nil.			^self ].		^self sendFocusEvent: anEvent to: mouseFocus in: w].	^self sendEvent: anEvent! !!OldMorphWithModel methodsFor: 'menu' stamp: 'jmv 11/6/2008 14:29'!addCustomMenuItems: aCustomMenu hand: aHandMorph	super addCustomMenuItems: aCustomMenu hand: aHandMorph.	self isOpen		ifTrue: [aCustomMenu add: 'close editing' translated action: #closeToEdits]		ifFalse: [aCustomMenu add: 'open editing' translated action: #openToEdits].! !!OldStringMorph methodsFor: 'editing' stamp: 'jmv 11/6/2008 17:38'!launchMiniEditor: evt 
	| textMorph |
	hasFocus := true.	"Really only means edit in progress for this morph"
	textMorph := OldStringMorphEditor new contentsAsIs: contents.
	textMorph beAllFont: self fontToUse.
	textMorph bounds: (self bounds expandBy: 0 @ 2).
	self addMorphFront: textMorph.
	evt hand newMouseFocus: textMorph.
	self flag: #arNote.	"Why???""
	evt hand newKeyboardFocus: textMorph."
	textMorph editor selectFrom: 1 to: textMorph paragraph text string size! !!OldSystemWindow methodsFor: 'events' stamp: 'jmv 11/6/2008 14:27'!mouseDown: evt	self setProperty: #clickPoint toValue: evt cursorPoint.	TopWindow == self ifFalse: [		evt hand releaseKeyboardFocus.		self activate].	self activeOnFirstClick ifTrue: [		"Normally window keeps control of first click.		Need explicit transmission for first-click activity."		submorphs do: [:m | (m containsPoint: evt cursorPoint) ifTrue: [m mouseDown: evt]]]! !!OldSystemWindow methodsFor: 'events' stamp: 'jmv 11/6/2008 14:27'!mouseMove: evt	"Handle a mouse-move event"	| cp |	cp _ evt cursorPoint.	self valueOfProperty: #clickPoint ifPresentDo: [:firstClick |		((self labelRect containsPoint: firstClick) and: [(cp dist: firstClick) > 3]) ifTrue: [			"If this is a drag that started in the title bar, then pick me up"			^ self isSticky ifFalse: [				self fastFramingOn 					ifTrue: [self doFastFrameDrag: firstClick]					ifFalse: [evt hand grabMorph: self]]]].	self activeOnFirstClick ifTrue: [		"Normally window takes control on first click.		Need explicit transmission for first-click activity."		submorphs do: [:m | (m containsPoint: cp) ifTrue: [m mouseMove: evt]]]! !!OldSystemWindow methodsFor: 'events' stamp: 'jmv 11/6/2008 14:28'!mouseUp: evt	| cp |	self activeOnFirstClick ifTrue: [		"Normally window takes control on first click.		Need explicit transmission for first-click activity."		cp _ evt cursorPoint.		submorphs do: [:m | (m containsPoint: cp) ifTrue: [m mouseUp: evt]]]! !!OldSystemWindow methodsFor: 'label' stamp: 'jmv 11/6/2008 14:54'!relabel	| newLabel |	newLabel _ FillInTheBlank 		request: 'New title for this window'		initialAnswer: labelString.	newLabel isEmpty ifTrue: [^self].	self setLabel: newLabel! !!OldSystemWindow methodsFor: 'panes' stamp: 'jmv 11/6/2008 14:31'!paneColor	| cc |	(cc := self valueOfProperty: #paneColor) ifNotNil: [^cc].	Display depth > 2 		ifTrue: 			[model ifNotNil: 					[model isInMemory 						ifTrue: 							[cc := Color colorFrom: model class windowColor.							Preferences alternativeWindowLook 								ifTrue: 									[cc := (cc = Color lightYellow or: [cc = Color white]) 										ifTrue: [Color gray: 0.67]										ifFalse: [cc duller]]]].			cc 				ifNil: [cc := paneMorphs isEmptyOrNil ifFalse: [paneMorphs first color]]].	cc ifNil: [cc := Color white].	self paneColor: cc.	^cc! !!OldSystemWindow methodsFor: 'testing' stamp: 'jmv 11/6/2008 14:26'!activeOnFirstClick	"Return true if should be active on first click.)"	^ false! !!OldThreePhaseButtonMorph methodsFor: 'event handling' stamp: 'jmv 11/6/2008 14:53'!mouseUp: evt	"Allow on:send:to: to set the response to events other than actWhen"	actWhen == #buttonUp ifFalse: [^super mouseUp: evt].	(self containsPoint: evt cursorPoint) ifTrue: [		self state: #on.		self doButtonAction: evt	] ifFalse: [		self state: #off.		"target ifNotNil: [target mouseUpBalk: evt]"	].	"Allow owner to keep it selected for radio buttons"! !!Preference methodsFor: 'button' stamp: 'jmv 11/6/2008 14:31'!tearOffButton	"Hand the user a button the can control this"	| aButton |	aButton _ self representativeButtonWithColor: self class windowColor inPanel: nil.	aButton borderWidth: 1; borderColor:  Color black.	aButton openInHand"(Preferences preferenceAt: #balloonHelpEnabled) tearOffButton"! !!SelectorBrowser methodsFor: 'as yet unclassified' stamp: 'jmv 11/6/2008 14:31'!morphicWindow
	"Create a Browser that lets you type part of a selector, shows a list of selectors, shows the classes of the one you chose, and spawns a full browser on it.  Answer the window
	SelectorBrowser new open "

	| window typeInView selectorListView classListView |
	window := (OldSystemWindow labelled: 'later') model: self.
	window setStripeColorsFrom: self class windowColor.
	selectorIndex := classListIndex := 0.
	typeInView := OldPluggableTextMorph 
				on: self
				text: #contents
				accept: #contents:notifying:
				readSelection: #contentsSelection
				menu: #codePaneMenu:shifted:.
	typeInView acceptOnCR: true.
	typeInView hideScrollBarsIndefinitely.
	window addMorph: typeInView frame: (0 @ 0 corner: 0.5 @ 0.14).
	selectorListView := OldPluggableListMorph 
				on: self
				list: #messageList
				selected: #messageListIndex
				changeSelected: #messageListIndex:
				menu: #selectorMenu:
				keystroke: #messageListKey:from:.
	selectorListView menuTitleSelector: #selectorMenuTitle.
	window addMorph: selectorListView frame: (0 @ 0.14 corner: 0.5 @ 0.6).
	classListView := OldPluggableListMorph 
				on: self
				list: #classList
				selected: #classListIndex
				changeSelected: #classListIndex:
				menu: nil
				keystroke: #arrowKey:from:.
	classListView menuTitleSelector: #classListSelectorTitle.
	window addMorph: classListView frame: (0.5 @ 0 corner: 1 @ 0.6).
	window 
		addMorph: ((OldPluggableTextMorph 
				on: self
				text: #byExample
				accept: #byExample:
				readSelection: #contentsSelection
				menu: #codePaneMenu:shifted:) askBeforeDiscardingEdits: false)
		frame: (0 @ 0.6 corner: 1 @ 1).
	window setLabel: 'Method Finder'.
	^window! !!TestRunner methodsFor: 'updating' stamp: 'jmv 11/6/2008 14:31'!refreshWindow	| pc |	pc _ self class windowColor.	passFailText isMorph		ifTrue: [passFailText color: pc.			detailsText color: pc]		ifFalse: [passFailText insideColor: pc.			detailsText insideColor: pc].	self updateErrors: TestResult new.	self updateFailures: TestResult new.	self displayPassFail: 'N/A'.	self displayDetails: '...'! !!TestRunner methodsFor: 'updating' stamp: 'jmv 11/6/2008 14:31'!runWindow	| pc |	pc _ self class windowColor.	passFailText isMorph		ifTrue: [passFailText color: pc.			detailsText color: pc]		ifFalse: [passFailText insideColor: pc.			detailsText insideColor: pc].	self updateErrors: TestResult new.	self updateFailures: TestResult new.	self displayPassFail: 'Running...'.	self displayDetails: '...'! !!TextEditor methodsFor: 'new selection' stamp: 'jmv 11/6/2008 15:43'!nextTokenFrom: start direction: dir	"simple token-finder for compiler automated corrections"	| loc str |	loc _ start + dir.	str _ paragraph text string.	[(loc between: 1 and: str size) and: [(str at: loc) isSeparator]]		whileTrue: [loc _ loc + dir].	^ loc! !!TextEditor methodsFor: 'new selection' stamp: 'jmv 11/6/2008 15:44'!notify: aString at: anInteger in: aStream 	"The compilation of text failed. The syntax error is noted as the argument, 	aString. Insert it in the text at starting character position anInteger."	| pos |	pos _ self selectionInterval notEmpty		ifTrue: [			self startIndex + anInteger - 1 ]		ifFalse: [anInteger].	self insertAndSelect: aString at: (pos max: 1)! !OldHandMorph removeSelector: #sendEvent:focus:!OldHandMorph removeSelector: #sendEvent:focus:clear:!OldHandMorph removeSelector: #sendFocusEvent:to:clear:!OldMorph removeSelector: #beep:!OldMorph removeSelector: #coordinatesFromWorld:!OldMorph removeSelector: #externalize:to:!!OldMorph reorganize!('*geniestubs-stubs' handleMouseDown: mouseStillDownStepRate)('WiW support' addMorphInFrontOfLayer: addMorphInLayer: morphicLayerNumber morphicLayerNumberWithin: shouldGetStepsFrom:)('accessing' adoptPaneColor: balloonText beSticky borderColor borderColor: borderStyle borderStyle: borderStyleForSymbol: borderWidth borderWidth: color color: colorForInsets doesBevels eventHandler eventHandler: forwardDirection highlight highlightColor insetColor isLocked isSticky lock lock: raisedColor rememberedColor rememberedColor: resistsRemoval scaleFactor setBorderStyle: sticky: toggleResistsRemoval toggleStickiness unlock unlockContents userString)('accessing - extension' assureExtension extension hasExtension initializeExtension privateExtension: resetExtension)('accessing - properties' hasProperty: otherProperties removeProperty: setProperty:toValue: valueOfProperty: valueOfProperty:ifAbsent: valueOfProperty:ifAbsentPut: valueOfProperty:ifPresentDo:)('as yet unclassified' rotationDegrees:)('button' doButtonAction)('caching' fullReleaseCachedState releaseCachedState)('change reporting' addedMorph: invalidRect: invalidRect:from: ownerChanged privateInvalidateMorph:)('classification' isAlignmentMorph isHandMorph isPlayfieldLike isTextMorph isWorldMorph isWorldOrHandMorph)('copying' copy deepCopy duplicate duplicateMorphCollection: veryDeepCopyWith: veryDeepFixupWith: veryDeepInner:)('creation' asMorph)('debug and other' addDebuggingItemsTo:hand: allStringsAfter: altSpecialCursor0 altSpecialCursor1 altSpecialCursor2 altSpecialCursor3 altSpecialCursor3: buildDebugMenu: inspectOwnerChain installModelIn: ownerChain resumeAfterDrawError resumeAfterStepError)('dispatching' disableSubmorphFocusForHand:)('drawing' areasRemainingToFill: changeClipSubmorphs clipLayoutCells clipLayoutCells: clipSubmorphs clipSubmorphs: clippingBounds drawDropHighlightOn: drawErrorOn: drawHighlightOn: drawMouseDownHighlightOn: drawOn: drawSubmorphsOn: drawingFails drawingFailsNot fullDrawOn: hasClipSubmorphsString hide highlightForMouseDown highlightForMouseDown: highlightedForMouseDown imageForm imageForm:forRectangle: imageFormForRectangle: isKnownFailing refreshWorld shadowForm show visible visible:)('drop shadows' shadowColor shadowColor:)('dropping/grabbing' aboutToBeGrabbedBy: dragEnabled dragEnabled: dragNDropEnabled dropEnabled dropEnabled: dropHighlightColor enableDrag: enableDragNDrop enableDragNDrop: enableDrop: formerOwner formerOwner: formerPosition formerPosition: highlightForDrop highlightForDrop: highlightedForDrop justDroppedInto:event: justGrabbedFrom: rejectDropMorphEvent: repelsMorph:event: resetHighlightForDrop separateDragAndDrop slideBackToFormerSituation: startDrag:with: vanishAfterSlidingTo:event: wantsDroppedMorph:event: wantsToBeDroppedInto: wantsToBeOpenedInWorld)('e-toy support' allMorphsAndBookPagesInto: changeAllBorderColorsFrom:to: containingWindow cursor defaultValueOrNil embeddedInMorphicWindowLabeled: rotationStyle rotationStyle: unlockOneSubpart wantsRecolorHandle wrappedInWindow: wrappedInWindowWithTitle:)('event handling' click click: cursorPoint doubleClick: doubleClickTimeout: dropFiles: handlesKeyboard: handlesMouseDown: handlesMouseOver: handlesMouseOverDragging: handlesMouseStillDown: hasFocus keyDown: keyStroke: keyUp: mouseDown: mouseEnter: mouseEnterDragging: mouseLeave: mouseLeaveDragging: mouseMove: mouseStillDown: mouseStillDownThreshold mouseUp: on:send:to: on:send:to:withValue: startDrag: suspendEventHandler transformFrom: transformFromOutermostWorld transformFromWorld wantsDropFiles: wantsKeyboardFocusFor: wouldAcceptKeyboardFocus wouldAcceptKeyboardFocusUponTab)('events-accessing' actionMap updateableActionMap)('events-alarms' addAlarm:after: addAlarm:at: addAlarm:with:after: addAlarm:with:at: addAlarm:with:with:after: addAlarm:with:with:at: addAlarm:withArguments:after: addAlarm:withArguments:at: alarmScheduler removeAlarm: removeAlarm:at:)('events-processing' containsPoint:event: defaultEventDispatcher handleDropFiles: handleDropMorph: handleEvent: handleFocusEvent: handleKeyDown: handleKeyUp: handleKeystroke: handleListenEvent: handleMouseEnter: handleMouseLeave: handleMouseMove: handleMouseOver: handleMouseStillDown: handleMouseUp: handleUnknownEvent: handlerForMouseDown: mouseDownPriority processEvent: processEvent:using: rejectDropEvent: rejectsEvent: transformedFrom:)('events-removing' releaseActionMap)('fileIn/out' prepareToBeSaved)('filter streaming' drawOnCanvas:)('focus handling' gotNavigationFocus gotNavigationFocus: keyboardFocusChange: lostNavigationFocus seizesNavigationFocus skipsNavigationFocus)('geometry' align:with: bottom bottom: bottomCenter bottomLeft bottomRight bounds bounds: bounds:in: boundsIn: boundsInWorld center center: extent extent: fullBoundsInWorld globalPointToLocal: height height: innerBounds left left: leftCenter localPointToGlobal: minimumExtent point:from: point:in: pointFromWorld: pointInWorld: position position: positionInWorld positionSubmorphs right right: rightCenter setConstrainedPosition:hangOut: top top: topCenter topLeft topRight width width: worldBounds worldBoundsForHalo)('geometry eToy' addTransparentSpacerOfSize: beTransparent forwardDirection: heading referencePosition referencePosition: rotationCenter rotationCenter: setDirectionFrom: transparentSpacerOfSize:)('geometry testing' containsPoint: fullContainsPoint:)('halos and balloon help' addHalo addHalo: addHalo:from: addHandlesTo:box: addOptionalHandlesTo:box: addSimpleHandlesTo:box: addWorldHandlesTo:box: balloonColor balloonColor: balloonFont balloonFont: balloonHelpAligner balloonHelpDelayTime balloonHelpTextForHandle: boundsForBalloon comeToFrontAndAddHalo defaultBalloonColor defaultBalloonFont deleteBalloon editBalloonHelpContent: editBalloonHelpText halo haloClass mouseDownOnHelpHandle: noHelpString okayToBrownDragEasily okayToResizeEasily okayToRotateEasily removeHalo setBalloonText: setBalloonText:maxLineLength: setCenteredBalloonText: showBalloon: showBalloon:hand: transferHalo:from: wantsBalloon wantsHaloFromClick wantsHaloHandleWithSelector:inHalo:)('initialization' defaultBounds defaultColor inATwoWayScrollPane initialize intoWorld: openCenteredInWorld openInHand openInWorld openInWorld:)('layout' acceptDroppingMorph:event: adjustLayoutBounds doLayoutIn: fullBounds layoutBounds layoutBounds: layoutChanged layoutInBounds: layoutProportionallyIn: minExtent minHeight minWidth privateFullBounds submorphBounds)('layout-menu' addCellLayoutMenuItems:hand: addLayoutMenuItems:hand: addTableLayoutMenuItems:hand: changeCellInset: changeClipLayoutCells changeDisableTableLayout changeLayoutInset: changeListDirection: changeMaxCellSize: changeMinCellSize: changeNoLayout changeProportionalLayout changeReverseCells changeRubberBandCells changeTableLayout hasClipLayoutCellsString hasDisableTableLayoutString hasNoLayoutString hasProportionalLayoutString hasReverseCellsString hasRubberBandCellsString hasTableLayoutString layoutMenuPropertyString:from:)('layout-properties' assureLayoutProperties assureTableProperties cellInset cellInset: cellPositioning cellPositioning: cellPositioningString: cellSpacing cellSpacing: cellSpacingString: disableTableLayout disableTableLayout: hResizing hResizing: hResizingString: layoutFrame layoutFrame: layoutInset layoutInset: layoutPolicy layoutPolicy: layoutProperties layoutProperties: listCentering listCentering: listCenteringString: listDirection listDirection: listDirectionString: listSpacing listSpacing: listSpacingString: maxCellSize maxCellSize: minCellSize minCellSize: reverseTableCells reverseTableCells: rubberBandCells rubberBandCells: spaceFillWeight vResizing vResizing: vResizingString: wrapCentering wrapCentering: wrapCenteringString: wrapDirection wrapDirection: wrapDirectionString:)('macpal' flash)('menu' addBorderStyleMenuItems:hand:)('menus' addAddHandMenuItemsForHalo:hand: addCopyItemsTo: addCustomHaloMenuItems:hand: addCustomMenuItems:hand: addExportMenuItems:hand: addFillStyleMenuItems:hand: addHaloActionsTo: addPaintingItemsTo:hand: addStandardHaloMenuItemsTo:hand: addTitleForHaloMenu: addToggleItemsToHaloMenu: adhereToEdge: adjustedCenter adjustedCenter: changeColor changeDragAndDrop chooseNewGraphic chooseNewGraphicCoexisting: chooseNewGraphicFromHalo collapse defaultArrowheadSize exportAsBMP exportAsGIF exportAsJPEG exportAsPNG hasDragAndDropEnabledString inspectInMorphic: lockUnlockMorph lockedString maybeAddCollapseItemTo: resetForwardDirection resistsRemovalString setToAdhereToEdge: snapToEdgeIfAppropriate stickinessString uncollapseSketch)('meta-actions' addEmbeddingMenuItemsTo:hand: blueButtonDown: blueButtonUp: buildHandleMenu: buildMetaMenu: changeColorTarget:selector:originalColor:hand: copyToPasteBuffer: dismissMorph: duplicateMorph: grabMorph: handlerForBlueButtonDown: handlerForMetaMenu: inspectAt:event: invokeMetaMenu: invokeMetaMenuAt:event: maybeDuplicateMorph maybeDuplicateMorph: potentialEmbeddingTargets resizeFromMenu resizeMorph: showActions)('miscellaneous' setExtentFromHalo:)('naming' name: nameForFindWindowFeature setNamePropertyTo: setNameTo:)('objects from disk' objectForDataStream: storeDataOn:)('other events' menuButtonMouseEnter: menuButtonMouseLeave:)('player' assureExternalName okayToDuplicate)('player commands' playSoundNamed:)('printing' clipText colorString: fullPrintOn: initString printConstructorOn:indent:nodeDict: printOn:)('rotate scale and flex' rotationDegrees)('stepping and presenter' arrangeToStartStepping arrangeToStartSteppingIn: start startStepping startStepping:at:arguments:stepTime: startSteppingSelector: step stepAt: stopStepping stopSteppingSelector:)('structure' activeHand allOwners allOwnersDo: firstOwnerSuchThat: hasOwner: isInWorld nearestOwnerThat: outermostMorphThat: outermostWorldMorph owner ownerThatIsA: pasteUpMorph pasteUpMorphHandlingTabAmongFields primaryHand root withAllOwnersDo: world)('submorphs-accessing' allMorphs allMorphsDo: allNonSubmorphMorphs findA: findDeepSubmorphThat:ifAbsent: findSubmorphBinary: firstSubmorph hasSubmorphs lastSubmorph morphsAt: morphsAt:behind:unlocked: morphsAt:unlocked: morphsAt:unlocked:do: morphsInFrontOf:overlapping:do: noteNewOwner: rootMorphsAt: submorphCount submorphNamed: submorphNamed:ifNone: submorphThat:ifNone: submorphs submorphsBehind:do: submorphsDo: submorphsInFrontOf:do: submorphsReverseDo: submorphsSatisfying:)('submorphs-add/remove' abandon actWhen actWhen: addAllMorphs: addAllMorphs:after: addMorph: addMorph:behind: addMorph:fullFrame: addMorph:inFrontOf: addMorphBack: addMorphCentered: addMorphFront: addMorphFront:fromWorldPosition: addMorphFrontFromWorldPosition: comeToFront copyWithoutSubmorph: delete dismissViaHalo goBehind privateDelete removeAllMorphs removeAllMorphsIn: removeMorph: removedMorph: replaceSubmorph:by:)('testing' canDrawAtHigherResolution canDrawBorder: isMorph knownName shouldDropOnMouseUp stepTime wantsSteps)('text-anchor' addTextAnchorMenuItems:hand: changeDocumentAnchor changeInlineAnchor changeParagraphAnchor hasDocumentAnchorString hasInlineAnchorString hasParagraphAnchorString relativeTextAnchorPosition relativeTextAnchorPosition: textAnchorType textAnchorType:)('updating' changed)('user interface' defaultLabelForInspector initialExtent)('visual properties' fillStyle fillStyle: useDefaultFill)('private' privateAddAllMorphs:atIndex: privateAddMorph:atIndex: privateBounds: privateColor: privateFullMoveBy: privateMoveBy: privateOwner: privateRemove: privateSubmorphs:)!Object removeSelector: #addModelMenuItemsTo:forMorph:hand:!Object removeSelector: #beep:!Object removeSelector: #beepPrimitive!Object removeSelector: #defaultBackgroundColor!Object removeSelector: #fullScreenSize!Object removeSelector: #mouseUpBalk:!Object removeSelector: #show!Object removeSelector: #windowActiveOnFirstClick!Object removeSelector: #windowReqNewLabel:!!Object reorganize!('*VMMaker-translation support' asIf:var: asIf:var:asValue: asIf:var:put: asOop: asSmallIntegerObj asValue: cCode: cCode:inSmalltalk: cCoerce:to: debugCode: export: primitive:parameters:receiver: remapOop:in: returnTypeC: sharedCodeNamed:inCase: stAt: stAt:put: stSize static: suppressFailureGuards: var:type: var:type:array:)('*sunit-preload' sunitAddDependent: sunitChanged: sunitRemoveDependent:)('*tools-browser' browse browseHierarchy)('Breakpoint' break)('accessing' addInstanceVarNamed:withValue: at: at:modify: at:put: basicAt: basicAt:put: basicSize bindWithTemp: doIfNotNil: ifNotNilDo: in: readFromString: size yourself)('as yet unclassified' revisar)('associating' ->)('binding' bindingOf:)('casing' caseOf: caseOf:otherwise:)('class membership' class inheritsFromAnyIn: isKindOf: isKindOf:orOf: isMemberOf: respondsTo: xxxClass)('comparing' = closeTo: hash hashMappedBy: identityHashMappedBy: identityHashPrintString literalEqual: ~=)('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asActionSequence asActionSequenceTrappingErrors asOrderedCollection asString asStringOrText complexContents mustBeBoolean mustBeBooleanIn: printDirectlyToDisplay withoutListWrapper)('copying' clone copy copyAddedStateFrom: copyFrom: copySameFrom: copyTwoLevel deepCopy initialDeepCopierSize postCopy shallowCopy veryDeepCopy veryDeepCopySibling veryDeepCopyUsing: veryDeepCopyWith: veryDeepFixupWith: veryDeepInner:)('creation' asMorph openAsMorph)('dependents access' addDependent: breakDependents canDiscardEdits dependents hasUnacceptedEdits myDependents myDependents: release removeDependent:)('drag and drop' acceptDroppingMorph:event:inMorph: dragAnimationFor:transferMorph: dragTransferType dragTransferTypeForMorph:)('error handling' assert: caseError confirm: confirm:orCancel: deprecated: deprecated:block: deprecated:explanation: deprecatedExplanation: doesNotUnderstand: error: halt halt: handles: notify: notify:at: notifyWithLabel: primitiveFailed shouldBeImplemented shouldNotImplement subclassResponsibility)('evaluating' value valueWithArguments:)('events-accessing' actionForEvent: actionForEvent:ifAbsent: actionMap actionSequenceForEvent: actionsDo: createActionMap hasActionForEvent: setActionSequence:forEvent: updateableActionMap)('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)('events-triggering' triggerEvent: triggerEvent:ifNotHandled: triggerEvent:with: triggerEvent:with:ifNotHandled: triggerEvent:withArguments: triggerEvent:withArguments:ifNotHandled:)('filter streaming' byteEncode: drawOnCanvas: elementSeparator flattenOnStream: printOnStream: putOn: storeOnStream: writeOnFilterStream:)('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)('flagging' isThisEverCalled isThisEverCalled: logEntry logExecution logExit)('inspecting' basicInspect inspect inspectorClass)('macpal' contentsChanged currentEvent currentHand currentWorld flash ifKindOf:thenDo: instanceVariableValues playSoundNamed: refusesToAcceptCode)('message handling' disableCode: perform: perform:orSendTo: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: withArgs:executeMethod:)('objects from disk' comeFullyUpOnReload: convertToCurrentVersion:refStream: indexIfCompact objectForDataStream: readDataFrom:size: storeDataOn:)('printing' fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString nominallyUnsent: print printOn: printString printStringLimitedTo: storeOn: storeString stringForReadout stringRepresentation)('scripting' defaultFloatPrecisionFor:)('system primitives' asOop becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarNamed: instVarNamed:put: oopString primitiveChangeClassTo: rootStubInImageSegment: someObject)('testing' basicType haltIfNil isBehavior isBlock isBlockClosure isColor isColorForm isCompiledMethod isFloat isForm isFraction isHeap isInteger isInterval isMessageSend isMorph isMorphicEvent isMorphicModel isNumber isPoint isPseudoContext isStream isString isSymbol isSystemWindow isText isTransparent isVariableBinding isWebBrowser knownName name notNil renameTo: showDiffs stepAt:in: stepIn: stepTime stepTimeIn: wantsDiffFeedback wantsSteps wantsStepsIn:)('translation support' inline: var:declareC:)('updating' changed changed: changed:with: noteSelectionIndex:for: okToChange update: update:with: updateListsAndCodeIn: windowIsClosing)('user interface' addModelItemsToWindowMenu: asExplorerString beep defaultLabelForInspector explore hasContentsInExplorer inform: initialExtent inspectWithLabel: modelSleep modelWakeUp modelWakeUpIn: notYetImplemented)('viewer' externalName)('private' errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: species storeAt:inTempFrame:)!