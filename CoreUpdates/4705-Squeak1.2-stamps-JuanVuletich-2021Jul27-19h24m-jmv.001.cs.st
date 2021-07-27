'From Cuis 5.0 [latest update: #4702] on 27 July 2021 at 7:25:19 pm'!

!Object methodsFor: 'comparing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
hash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	May be overridden, and should be overridden in any classes that define = "

	^ self identityHash! !

!Object methodsFor: 'translation support' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
inline: inlineFlag
	"For translation only; noop when running in Smalltalk."! !


!Browser methodsFor: 'metaclass' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
classOrMetaClassOrganizer
	"Answer the class organizer for the metaclass or class, depending on 
	which (instance or class) is indicated."

	self metaClassIndicated
		ifTrue: [^metaClassOrganizer]
		ifFalse: [^classOrganizer]! !


!HierarchyBrowser methodsFor: 'initialization' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
initAlphabeticListing
	| tab stab index |
	self systemOrganizer: SystemOrganization.
	metaClassIndicated _ false.
	classList _ Smalltalk classNames.! !


!Color methodsFor: 'conversions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
pixelWordForDepth: depth
	"Return to a 32-bit word that concatenates enough copies of the receiver's pixel value to fill the word (i.e., 32/depth copies). Depth should be one of 1, 2, 4, 8, 16, or 32. The pixel value should be an integer in 0..2^depth-1."

	| pixelValue |
	pixelValue _ self pixelValueForDepth: depth.
	^ self pixelWordFor: depth filledWith: pixelValue
! !

!Color methodsFor: 'queries' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isTransparent

	^ false
! !

!Color methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
attemptToMutateError
	"A color is immutable. Once a color's red, green, and blue have been initialized, you cannot change them. Instead, create a new Color and use it."

	self error: 'Color objects are immutable once created'
! !


!Color class methodsFor: 'examples' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
showColorCube
	"Show a 12x12x12 color cube."
	"Color showColorCube"

	0 to: 11 do: [:r |
		0 to: 11 do: [:g |
			0 to: 11 do: [:b |	
				Display fill: (((r*60) + (b*5)) @ (g*5) extent: 5@5)
					fillColor: (Color r: r g: g b: b range: 11)]]].
! !

!Color class methodsFor: 'examples' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
showHSVPalettes
	"Shows a palette of hues, varying the saturation and brightness for each one. Best results are with depths 16 and 32."
	"Color showHSVPalettes"

	| left top c |
	left _ top _ 0.
	0 to: 179 by: 15 do: [:h |
		0 to: 10 do: [:s |
			left _ (h * 4) + (s * 4).
			0 to: 10 do: [:v |
				c _ Color h: h s: s asFloat / 10.0 v: v asFloat / 10.0.
				top _ (v * 4).
				Display fill: (left@top extent: 4@4) fillColor: c.

				c _ Color h: h + 180 s: s asFloat / 10.0 v: v asFloat / 10.0.
				top _ (v * 4) + 50.
				Display fill: (left@top extent: 4@4) fillColor: c]]].
! !

!Color class methodsFor: 'examples' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
wheel: thisMany
	"Return a collection of thisMany colors evenly spaced around the color wheel."
	"Color showColors: (Color wheel: 12)"

	^ Color wheel: thisMany saturation: 0.9 brightness: 0.7
! !

!Color class methodsFor: 'examples' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
wheel: thisMany saturation: s brightness: v
	"Return a collection of thisMany colors evenly spaced around the color wheel, all of the given saturation and brightness."
	"Color showColors: (Color wheel: 12 saturation: 0.4 brightness: 1.0)"
	"Color showColors: (Color wheel: 12 saturation: 0.8 brightness: 0.5)"

	^ (Color h: 0.0 s: s v: v) wheel: thisMany
! !

!Color class methodsFor: 'colormaps' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
colorMapIfNeededFrom: sourceDepth to: destDepth
	"Return a colormap for mapping between the given depths, or nil if no colormap is needed."
	"Note: This method returns a shared, cached colormap to save time and space. Clients that need to modify a colormap returned by this method should make a copy and modify that!!"

	sourceDepth = destDepth ifTrue: [^ nil].  "not needed if depths are the same"

	(sourceDepth >= 16) & (destDepth >= 16) ifTrue: [
		"mapping is done in BitBlt by zero-filling or truncating each color component"
		^ nil].

	^ Color cachedColormapFrom: sourceDepth to: destDepth
! !


!UndefinedObject methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
ifNil: aBlock
	"A convenient test, in conjunction with Object ifNil:"

	^ aBlock value! !

!UndefinedObject methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
ifNil: nilBlock ifNotNil: ifNotNilBlock
	"Evaluate the block for nil because I'm == nil"

	^ nilBlock value! !

!UndefinedObject methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
ifNotNil: aBlock
	"A convenient test, in conjunction with Object ifNotNil:"

	^ self! !

!UndefinedObject methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
ifNotNil: ifNotNilBlock ifNil: nilBlock 
	"If I got here, I am nil, so evaluate the block nilBlock"

	^ nilBlock value! !


!Float methodsFor: 'mathematical functions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
cos
	"Answer the cosine of the receiver taken as an angle in radians."

	^ (self + Halfpi) sin! !

!Float methodsFor: 'mathematical functions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
log
	"Answer the base 10 logarithm of the receiver."

	^ self ln / Ln10! !

!Float methodsFor: 'mathematical functions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
tan
	"Answer the tangent of the receiver taken as an angle in radians."

	^ self sin / self cos! !

!Float methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isFloat
	^ true! !


!Fraction methodsFor: 'converting' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isFraction
	^ true! !


!Integer methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
digitRshift: anInteger bytes: b lookfirst: a 
	 "Shift right 8*b+anInteger bits, 0<=n<8.
	Discard all digits beyond a, and all zeroes at or below a."
	| n x r f m digit count i |
	n _ 0 - anInteger.
	x _ 0.
	f _ n + 8.
	i _ a.
	m _ 255 bitShift: 0 - f.
	digit _ self digitAt: i.
	[((digit bitShift: n) bitOr: x) = 0 and: [i ~= 1]] whileTrue:
		[x _ digit bitShift: f "Can't exceed 8 bits".
		i _ i - 1.
		digit _ self digitAt: i].
	i <= b ifTrue: [^Integer new: 0 neg: self negative].  "All bits lost"
	r _ Integer new: i - b neg: self negative.
	count _ i.
	x _ (self digitAt: b + 1) bitShift: n.
	b + 1 to: count do:
		[:j | digit _ self digitAt: j + 1.
		r digitAt: j - b put: (((digit bitAnd: m) bitShift: f) bitOr: x) 
			"Avoid values > 8 bits".
		x _ digit bitShift: n].
	^r! !


!SmallInteger methodsFor: 'comparing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
identityHash

	^self! !


!MessageTally methodsFor: 'tallying' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
bumpBy: count

	tally _ tally + count! !


!ContextPart class methodsFor: 'examples' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
tallyInstructions: aBlock
	"This method uses the simulator to count the number of occurrences of
	each of the Smalltalk instructions executed during evaluation of aBlock.
	Results appear in order of the byteCode set."
	| tallies |
	tallies _ Bag new.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep:
			[:current | tallies add: current nextByte].
	^tallies sortedElements

	"ContextPart tallyInstructions: [3.14159 printString]"! !


!InputSensor methodsFor: 'joystick' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
joystickButtons: index

	^ ((self primReadJoystick: index) bitShift: -22) bitAnd: 16r71F
	! !

!InputSensor methodsFor: 'joystick' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
joystickOn: index

	^ (((self primReadJoystick: index) bitShift: -27) bitAnd: 1) ~= 0
	! !

!InputSensor methodsFor: 'joystick' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
joystickXY: index

	| inputWord x y |
	inputWord _ self primReadJoystick: index.
	x _ (inputWord bitAnd: 16r7FF) - 16r400.
	y _ ((inputWord bitShift: -11) bitAnd: 16r7FF) - 16r400.
	^ x@y
	! !


!String methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
skipDelimiters: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that does NOT match one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1.  Assumes the delimiters to be a non-empty string."

	start to: self size do: [:i |
		delimiters detect: [:delim | delim = (self at: i)]
				ifNone: [^ i]].
	^ self size + 1! !


!ByteArray methodsFor: 'converting' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
asString
	"Convert to a String with Characters for each byte.
	Fast code uses primitive that avoids character conversion"

	^ (String new: self size) replaceFrom: 1 to: self size with: self! !


!CompiledMethod methodsFor: 'printing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
storeOn: aStream
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' newMethod: '.
	aStream store: self size - self initialPC + 1.
	aStream nextPutAll: ' header: '.
	aStream store: self header.
	aStream nextPut: $).
	noneYet _ self storeElementsFrom: self initialPC to: self endPC on: aStream.
	1 to: self numLiterals do:
		[:index |
		noneYet
			ifTrue: [noneYet _ false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' literalAt: '.
		aStream store: index.
		aStream nextPutAll: ' put: '.
		aStream store: (self literalAt: index)].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)! !

!CompiledMethod methodsFor: 'scanning' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
scanLongStore: extension 
	"Answer whether the receiver contains a long store whose extension is 
	the argument."
	| scanner |
	scanner _ InstructionStream on: self.
	^scanner scanFor: 
		[:instr |  (instr = 129 or: [instr = 130]) and: [scanner followingByte = extension]]! !

!CompiledMethod methodsFor: 'scanning' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
scanVeryLongLoad: extension offset: offset
	"Answer whether the receiver contains a long load whose extension is the 
	argument."
	| scanner |
	scanner _ InstructionStream on: self.
	^ scanner scanFor: [:instr | (instr = 132 and: [scanner followingByte = extension])
											and: [scanner thirdByte = offset]]! !

!CompiledMethod methodsFor: 'scanning' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
sendsToSuper
	"Answer whether the receiver sends any message to super."
	| scanner |
	scanner _ InstructionStream on: self.
	^ scanner scanFor: 
		[:instr |  instr = 16r85 or: [instr = 16r84
						and: [scanner followingByte between: 16r20 and: 16r3F]]]! !


!CompiledMethod class methodsFor: 'instance creation' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
new
	"This will not make a meaningful method, but it could be used
	to invoke some otherwise useful method in this class."
	^ self newMethod: 0 header: 0! !


!Bitmap methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
primFill: aPositiveInteger
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays."

	<primitive: 145>
	self errorImproperStore.! !


!OrderedCollection methodsFor: 'copying' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| newOrderedCollection delta startIndex stopIndex |
	"if start is less than 1, ignore stop and assume this is inserting at the front. 
	if start greater than self size, ignore stop and assume this is appending. 
	otherwise, it is replacing part of me and start and stop have to be within my 
	bounds. "
	delta _ 0.
	startIndex _ start.
	stopIndex _ stop.
	start < 1
		ifTrue: [startIndex _ stopIndex _ 0]
		ifFalse: [startIndex > self size
				ifTrue: [startIndex _ stopIndex _ self size + 1]
				ifFalse: 
					[(stopIndex < (startIndex - 1) or: [stopIndex > self size])
						ifTrue: [self errorOutOfBounds].
					delta _ stopIndex - startIndex + 1]].
	newOrderedCollection _ 
		self species new: self size + replacementCollection size - delta.
	1 to: startIndex - 1 do: [:index | newOrderedCollection add: (self at: index)].
	1 to: replacementCollection size do: 
		[:index | newOrderedCollection add: (replacementCollection at: index)].
	stopIndex + 1 to: self size do: [:index | newOrderedCollection add: (self at: index)].
	^newOrderedCollection! !


!RunArray methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
at: index

	self at: index setRunOffsetAndValue: [:run :offset :value | ^value]! !

!RunArray methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
runLengthAt: index 
	"Answer the length remaining in run beginning at index."

	self at: index 
		setRunOffsetAndValue: [:run :offset :value | ^(runs at: run) - offset]! !


!Set methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
findElementOrNil: anObject
	"Answer the index of a first slot containing either a nil (indicating an empty slot) or an element that matches the given object. Answer the index of that slot or zero. Fail if neither a match nor an empty slot is found."
	| index |
	index _ self scanFor: anObject.
	index > 0 ifTrue: [ ^ index ].

	"Bad scene.  Neither have we found a matching element
	nor even an empty slot.  No hashed set is ever supposed to get
	completely full."
	self error: 'There is no free space in this set!!'.! !

!Set methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start _ (anObject hash \\ array size) + 1.
	finish _ array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == nil or: [element = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == nil or: [element = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !


!Dictionary methodsFor: 'removing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
removeUnreferencedKeys   "Undeclared removeUnreferencedKeys"

	^ self unreferencedKeys do: [:key | self removeKey: key].! !

!Dictionary methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start _ (anObject hash \\ array size) + 1.
	finish _ array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !


!MethodDictionary methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start _ (anObject identityHash \\ array size) + 1.
	finish _ array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element _ self basicAt: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element _ self basicAt: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
cleanOutUndeclared 
	Undeclared removeUnreferencedKeys! !

!SystemDictionary methodsFor: 'memory space' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
okayToProceedEvenIfSpaceIsLow
	"Return true if either there is enough memory to do so safely or if the user gives permission after being given fair warning."

	self garbageCollectMost > self lowSpaceThreshold ifTrue: [^ true].  "quick"
	self garbageCollect > self lowSpaceThreshold ifTrue: [^ true].  "work harder"

	^ self confirm:
'WARNING: There is not enough space to start the low space watcher.
If you proceed, you will not be warned again, and the system may
run out of memory and crash. If you do proceed, you can start the
low space notifier when more space becomes available simply by
opening and then closing a debugger (e.g., by hitting Cmd-period.)
Do you want to proceed?'
! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
pointersTo: anObject
	"Find all occurrences in the system of pointers to the argument anObject."
	"(Smalltalk pointersTo: Browser) inspect."

	^ self pointersTo: anObject except: #()
! !

!SystemDictionary methodsFor: 'special objects' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
specialNargsAt: anInteger 
	"Answer the number of arguments for the special selector at: anInteger."

	^ (self specialObjectsArray at: 24) at: anInteger * 2! !

!SystemDictionary methodsFor: 'special objects' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
specialSelectorAt: anInteger 
	"Answer the special message selector from the interleaved specialSelectors array."

	^ (self specialObjectsArray at: 24) at: anInteger * 2 - 1! !

!SystemDictionary methodsFor: 'special objects' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
specialSelectorSize
	"Answer the number of special selectors in the system."

	^ (self specialObjectsArray at: 24) size // 2! !


!DataStream methodsFor: 'other' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
byteStream
	^ byteStream! !


!SmartRefStream methodsFor: 'read write' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
structures
	^ structures! !


!Parser methodsFor: 'temps' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
bindArg: name

	^ self bindTemp: name! !


!ParseNode methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isUndefTemp
	^ false! !

!ParseNode methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isUnusedTemp
	^ false! !

!ParseNode methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
nowHasDef  "Ignored in all but VariableNode"! !

!ParseNode methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
nowHasRef  "Ignored in all but VariableNode"! !


!Encoder methodsFor: 'temps' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
newTemp: name

	nTemps _ nTemps + 1.
	^ TempVariableNode new
		name: name
		index: nTemps - 1
		type: LdTempType
		scope: 0! !

!Encoder methodsFor: 'source mapping' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
sourceMap
	"Answer with a sorted set of associations (pc range)."

	^ (sourceRanges keys collect: 
		[:key |  Association key: key pc value: (sourceRanges at: key)])
			asSortedCollection! !

!Encoder methodsFor: 'initialization' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
fillDict: dict with: nodeClass mapping: keys to: codeArray
	| codeStream |
	codeStream _ ReadStream on: codeArray.
	keys do: 
		[:key | dict 
				at: key
				put:  (nodeClass new name: key key: key code: codeStream next)]! !


!TempVariableNode methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isTemp
	^ true! !

!TempVariableNode methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isUndefTemp
	^ hasDefs not! !

!TempVariableNode methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isUnusedTemp
	^ hasRefs not! !

!TempVariableNode methodsFor: 'initialization' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
nowHasDef
	hasDefs _ true! !

!TempVariableNode methodsFor: 'initialization' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
nowHasRef
	hasRefs _ true! !

!TempVariableNode methodsFor: 'initialization' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
scope: level
	"Note scope of temporary variables.
	Currently only the following distinctions are made:
		0	outer level: args and user-declared temps
		1	block args and doLimiT temps
		-1	a block temp that is no longer active
		-2	a block temp that held limit of to:do:"
	scope _ level! !


!DecompilerConstructor methodsFor: 'constructor' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
codeConstants
	"Answer with an array of the objects representing self, true, false, nil,
	-1, 0, 1, 2."

	^(Array with: NodeSelf with: NodeTrue with: NodeFalse with: NodeNil)
		, ((-1 to: 2) collect: [:i | LiteralNode new key: i code: LdMinus1 + i + 1])! !

!DecompilerConstructor methodsFor: 'constructor' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
codeTemp: index named: tempName

	^ TempVariableNode new
		name: tempName
		index: index
		type: LdTempType
		scope: 0! !


!TextAttribute methodsFor: 'as yet unclassified' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
actOnClickFor: model
	"Subclasses may override to provide, eg, hot-spot actions"
	^ false! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
dominates: another
	"Subclasses may override condense multiple attributes"
	^ false! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
mayActOnClick
	"Subclasses may override to provide, eg, hot-spot actions"
	^ false! !


!TextAction methodsFor: 'as yet unclassified' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
mayActOnClick

	^ true! !


!TextColor methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
color
	^ color! !

!TextColor methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
color: aColor
	color _ aColor! !

!TextColor methodsFor: 'printing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
printOn: strm
	super printOn: strm.
	strm nextPutAll: ' code: '; print: color! !


!TextColor class methodsFor: 'instance creation' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
color: aColor
	^ self new color: aColor! !


!TextEmphasis methodsFor: 'as yet unclassified' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
emphasisCode
	^ emphasisCode! !

!TextEmphasis methodsFor: 'printing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
printOn: strm
	super printOn: strm.
	strm nextPutAll: ' code: '; print: emphasisCode! !


!RemoteString methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
position 
	"Answer the location of the string on a file."

	^ filePositionHi! !

!RemoteString methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
fileNumber: fileNumber position: position 

	sourceFileNumber _ fileNumber.
	filePositionHi _ position! !


!Form methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
size
	"Should no longer be used -- use bitsSize instead.  length of variable part of instance."
	^ super size! !

!Form methodsFor: 'copying' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
copy: sourceRectangle from: sourceForm to: destPt rule: rule
	^ self copy: (destPt extent: sourceRectangle extent)
		from: sourceRectangle topLeft in: sourceForm rule: rule! !

!Form methodsFor: 'scaling, rotation' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
shrink: aRectangle by: scale 
	| scalePt |
	scalePt _ scale asPoint.
	^ self magnify: aRectangle by: (1.0 / scalePt x asFloat) @ (1.0 / scalePt y asFloat)! !


!Form class methodsFor: 'mode constants' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
erase1bitShape
	"Answer the integer denoting mode erase."

	^ 26! !

!Form class methodsFor: 'mode constants' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
oldErase1bitShape
	"Answer the integer denoting mode erase."

	^ 17! !

!Form class methodsFor: 'mode constants' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
oldPaint
	"Answer the integer denoting the 'paint' combination rule."

	^16! !

!Form class methodsFor: 'mode constants' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
paint
	"Answer the integer denoting the 'paint' combination rule."

	^25! !


!ImageReadWriter methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
nextImage
	"Dencoding an image on stream and answer the image."

	^self subclassResponsibility! !

!ImageReadWriter methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
nextPutImage: anImage
	"Encoding anImage on stream."

	^self subclassResponsibility! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
atEnd

	^stream atEnd! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
contents

	^stream contents! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
next

	^stream next! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
next: size

	^stream next: size! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
nextPut: aByte

	^stream nextPut: aByte! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
nextPutAll: aByteArray

	^stream nextPutAll: aByteArray! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
position

	^stream position! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
position: anInteger

	^stream position: anInteger! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
size

	^stream size! !

!ImageReadWriter methodsFor: 'stream access' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
skip: anInteger

	^stream skip: anInteger! !

!ImageReadWriter methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
changePadOfBits: bits width: width height: height depth: depth from: oldPad
to: newPad
	"Change padding size of bits."

	| srcRowByteSize dstRowByteSize newBits srcRowBase rowEndOffset |
	(#(8 16 32) includes: oldPad)
		ifFalse: [^self error: 'Invalid pad: ', oldPad printString].
	(#(8 16 32) includes: newPad)
		ifFalse: [^self error: 'Invalid pad: ', newPad printString].
	srcRowByteSize _ width * depth + oldPad - 1 // oldPad * (oldPad / 8).
	srcRowByteSize * height = bits size
		ifFalse: [^self error: 'Incorrect bitmap array size.'].
	dstRowByteSize _ width * depth + newPad - 1 // newPad * (newPad / 8).
	newBits _ ByteArray new: dstRowByteSize * height.
	srcRowBase _ 1.
	rowEndOffset _ dstRowByteSize - 1.
	1 to: newBits size by: dstRowByteSize do:
		[:dstRowBase |
		newBits replaceFrom: dstRowBase
			to: dstRowBase + rowEndOffset
			with: bits
			startingAt: srcRowBase.
		srcRowBase _ srcRowBase + srcRowByteSize].
	^newBits! !

!ImageReadWriter methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
hasMagicNumber: aByteArray
	| position |
	position _ stream position.
	((stream size - position) >= aByteArray size and:
	[(stream next: aByteArray size)  = aByteArray])
		ifTrue: [^true].
	stream position: position.
	^false! !


!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
colorMap
	^ colorMap! !

!BitBlt methodsFor: 'accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
sourceForm

	^ sourceForm! !

!BitBlt methodsFor: 'line drawing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
drawFrom: startPoint to: stopPoint 
	
	 ^ self drawFrom: startPoint to: stopPoint withFirstPoint: true! !


!WarpBlt methodsFor: 'setup' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
cellSize
	^ cellSize! !

!WarpBlt methodsFor: 'primitives' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
copyQuad: pts toRect: destRect
	self sourceQuad: pts destRect: destRect.
	self warpBits! !

!WarpBlt methodsFor: 'primitives' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
deltaFrom: x1 to: x2 nSteps: n
	"Utility routine for computing Warp increments.
	x1 is starting pixel, x2 is ending pixel;  assumes n >= 1"
	| fixedPtOne |
	fixedPtOne _ 16384.  "1.0 in fixed-pt representation"
	x2 > x1
		ifTrue: [^ x2 - x1 + fixedPtOne // (n+1) + 1]
		ifFalse: [x2 = x1 ifTrue: [^ 0].
				^ 0 - (x1 - x2 + fixedPtOne // (n+1) + 1)]! !

!WarpBlt methodsFor: 'primitives' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
sourceQuad: pts destRect: aRectangle
	| fixedPt1 |
	sourceX _ sourceY _ 0.
	self destRect: aRectangle.
	fixedPt1 _ (pts at: 1) x isInteger ifTrue: [16384] ifFalse: [16384.0].
	p1x _ (pts at: 1) x * fixedPt1.
	p2x _ (pts at: 2) x * fixedPt1.
	p3x _ (pts at: 3) x * fixedPt1.
	p4x _ (pts at: 4) x * fixedPt1.
	p1y _ (pts at: 1) y * fixedPt1.
	p2y _ (pts at: 2) y * fixedPt1.
	p3y _ (pts at: 3) y * fixedPt1.
	p4y _ (pts at: 4) y * fixedPt1.
	p1z _ p2z _ p3z _ p4z _ 16384.  "z-warp ignored for now"
! !

!WarpBlt methodsFor: 'primitives' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
startFrom: x1 to: x2 offset: sumOfDeltas
	"Utility routine for computing Warp increments."
	x2 >= x1
		ifTrue: [^ x1]
		ifFalse: [^ x2 - sumOfDeltas]! !


!WarpBlt class methodsFor: 'initialization' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
toForm: destinationForm
	"Default cell size is 1 (no pixel smoothing)"
	^ (super toForm: destinationForm) cellSize: 1! !


!Point methodsFor: 'converting' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
asFloatPoint
	^ x asFloat @ y asFloat! !


!Rectangle methodsFor: 'rectangle functions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
withHeight: height 
	"Return a copy of me with a different height"
	^ origin corner: corner x @ (origin y + height)! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
withLeft: x 
	"Return a copy of me with a different left x"
	^ x @ origin y corner: corner x @ corner y! !

!Rectangle methodsFor: 'rectangle functions' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
withRight: x 
	"Return a copy of me with a different right x"
	^ origin x @ origin y corner: x @ corner y! !

!Rectangle methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
containsRect: aRect
	"Answer whether aRect is within the receiver (OK to coincide)."

	^ aRect origin >= origin and: [aRect corner <= corner]
! !

!Rectangle methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isTall
	^ self height > self width! !

!Rectangle methodsFor: 'testing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isWide
	^ self width > self height! !

!Rectangle methodsFor: 'private' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
setOrigin: topLeft corner: bottomRight
	origin _ topLeft.
	corner _ bottomRight! !


!Morph methodsFor: 'classification' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
isWorldMorph

	^ false! !

!Morph methodsFor: 'structure' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
owner
	"Returns the owner of this morph, which may be nil."

	^ owner! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
submorphCount

	^ submorphs size! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
submorphs

	^ submorphs copy! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
submorphsDo: aBlock

	submorphs do: aBlock.! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
submorphsReverseDo: aBlock

	submorphs reverseDo: aBlock.! !


!ImageMorph methodsFor: 'menu commands' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
grabFromScreen
	self image: Form fromUser! !


!ScrollBar methodsFor: 'access' stamp: 'Squeak1.2 6/29/1997 10:40:55'!
scrollDelta: d1 pageDelta: d2
	"Supply optional increments for better scrolling of, eg, text"
	scrollDelta _ d1.
	pageDelta _ d2.! !

