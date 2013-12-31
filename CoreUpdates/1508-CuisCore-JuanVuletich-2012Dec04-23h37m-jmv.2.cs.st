'From Cuis 4.0 of 21 April 2012 [latest update: #1507] on 4 December 2012 at 11:42:47 pm'!

!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 12/4/2012 23:38'!
handleKeystrokeBefore: kbEvent
	"I return a boolean. true when I have handled the event and no futher processing is needed by the caller."
	| currentPos currentCharIsAlphaNumeric keyValue ctrl cmd tab colon alphanum backspace esc space return keyChar  |
	currentPos _ textMorph editor startIndex-1.
	currentCharIsAlphaNumeric _ currentPos > 0 and: [ model textSize >= currentPos and: [
			(model actualContents at: currentPos) isAlphaNumeric ]].
	keyValue _ kbEvent keyValue.
	keyChar _ kbEvent keyCharacter.
	ctrl _ kbEvent controlKeyPressed.
	cmd _ kbEvent commandAltKeyPressed.
	tab _ keyChar = Character tab.
	colon _ keyChar = $:.
	alphanum _ kbEvent keyCharacter isAlphaNumeric.
	backspace _ keyValue = 8.
	esc _ keyValue = 27.
	space _ #(0 32 160) includes: keyValue.
	return _ kbEvent isReturnKey.

	"Stuff to do if the menu is not open"
	menuMorph ifNil: [
		"Ctrl-Space or Tab for open"
		"Mac specific note: Using option-space (actually option+160) effectively disables the non-breaking space character 160"
		(space & (ctrl | kbEvent rawMacOptionKeyPressed) or: [
			(self opensWithTab and: [tab]) and: [ currentCharIsAlphaNumeric ]])
				ifTrue: [ self openCompletionMenu. ^ true].
		"Auto-open - currently deactivated"
"		(ctrl not & cmd not & alphanum) 
			ifTrue: [ self openCompletionMenu ]."
		^ false].

	"Starting here, stuff to do if the menu is open"
	menuMorph stillActive.
	"Escape"
	esc ifTrue: [ self closeMenu. ^ true].
	"Backspace"
	backspace ifTrue: [
		currentCharIsAlphaNumeric ifFalse: [ self closeMenu ].
		^ false].
	"Home"
	keyValue = 1 ifTrue: [ menuMorph home. ^ true ].
	"End"
	keyValue = 4 ifTrue: [ menuMorph end. ^ true].
	"?"
	keyChar = $? ifTrue: [ menuMorph help. ^true].
	"Arrow up"
	keyValue = 30 ifTrue: [ menuMorph moveUp. ^ true].
	"Arrow down"
	keyValue = 31 ifTrue: [ menuMorph moveDown. ^ true].
	"Page up"
	keyValue = 11 ifTrue: [ menuMorph pageUp. ^ true].
	"Page down"
	keyValue = 12 ifTrue: [ menuMorph pageDown. ^ true].
	"Return, Tab or Ctrl-Space"
	(return or: [ space & (ctrl | kbEvent rawMacOptionKeyPressed) or: [ tab]]) ifTrue: [
		self insertSelected
			ifTrue: [^ true]].
	"All keys but the alphanumeric chars (without command and control ) 
	and the backspace key do close the menu"
	(ctrl not & cmd not and: [ alphanum | colon])
		ifFalse: [ self closeMenu ].
	^false! !


!BitBlt class methodsFor: 'private' stamp: 'jmv 12/4/2012 23:40'!
exampleAt: originPoint rule: rule fillColor: mask 
	"This builds a source and destination form and copies the source to the
	destination using the specifed rule and mask. It is called from the method
	named exampleOne. Only works with Display depth of 1"

	| s d border aBitBlt | 
	border _ Form extent: 32@32.
	border fillBlack.
	border fill: (1@1 extent: 30@30) fillColor: Color white.
	s _ Form extent: 32@32.
	s fillWhite.
	s fillBlack: (7@7 corner: 25@25).
	d _ Form extent: 32@32.
	d fillWhite.
	d fillBlack: (0@0 corner: 32@16).

	s displayOn: Display at: originPoint.
	border displayOn: Display at: originPoint rule: Form under.
	d displayOn: Display at: originPoint + (s width @0).
	border displayOn: Display at: originPoint + (s width @0) rule: Form under.

	d displayOn: Display at: originPoint + (s extent // (2 @ 1)). 
	aBitBlt _ BitBlt
		destForm: Display
		sourceForm: s
		fillColor: mask
		combinationRule: rule
		destOrigin: originPoint + (s extent // (2 @ 1))
		sourceOrigin: 0 @ 0
		extent: s extent
		clipRect: Display computeBoundingBox.
	aBitBlt copyBits.
	border 
		displayOn: Display at: originPoint + (s extent // (2 @ 1))
		rule: Form under.

	"BitBlt exampleAt: 100@100 rule: 0 fillColor: nil"  ! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 12/4/2012 23:40'!
scanCategory
	"Scan anything that involves more than one chunk; method name is historical only"

	| itemPosition item item2 tokens firstToken secondToken stamp isComment anIndex def isMeta name record methodReference doItOnlyIfInBaseSystem |
	itemPosition _ file position.
	item _ file nextChunk.
	isComment _ (item includesSubString: 'commentStamp:').
	((isComment
	or: [item includesSubString: 'methodsFor:']
	or: [item includesSubString: 'classDefinition:']
	or: [item includesSubString: 'classRemoval:']
	or: [item includesSubString: 'methodRemoval:'])
	or: [item includesSubString: 'methodMoveToSomePackage:']
	or: [item includesSubString: 'classMoveToSomePackage:'])
		ifFalse: [
			"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
			^ self addItem: (ChangeRecord new file: file position: itemPosition type: #preamble)
				 text: ('preamble: ' , item contractTo: 160)].

	tokens _ Smalltalk actualScannerClass new scanTokens: item.
	tokens size >= 2 ifTrue: [
		stamp _ ''.
		anIndex _ tokens indexOf: #stamp: ifAbsent: nil.
		anIndex ifNotNil: [stamp _ tokens at: (anIndex + 1)].
		firstToken _ tokens first.
		secondToken _ tokens second.

		firstToken == #classDefinition: ifTrue: [
			itemPosition _ file position.
			isMeta _ secondToken includesSubString: ' class'.
			name _ isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
			def _ file nextChunk.
			record _ ChangeRecord new file: file position: itemPosition type: #classDefinition
				class: name asSymbol category: tokens last meta: isMeta stamp: nil.
			self addItem: record text: 'classDefinition: ', def.
			^ self ].

		(firstToken == #classRemoval: or: [ firstToken == #classMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #classMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				self 
					addItem: (ClassDeletionChangeRecord new
						clsName: secondToken;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['clase move to some package: '] ifFalse: ['class removal: ']), secondToken ].
			^ self ].

		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #methodMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				isMeta _ tokens third == #class.
				isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
				methodReference _ (MethodReference new
					setClassSymbol: secondToken
					classIsMeta: isMeta
					methodSymbol: tokens last
					stringVersion: secondToken, ' ', (isMeta ifTrue: ['class '] ifFalse: ['']), tokens last).
				self
					addItem: (MethodDeletionChangeRecord new
						methodReference: methodReference;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['method move to some package: '] ifFalse: ['method removal: ']), 
							methodReference asStringOrText ].
			^ self ].
		
		secondToken == #methodsFor: ifTrue: [
			^ self scanCategory: tokens third class: firstToken meta: false stamp: stamp].

		tokens third == #methodsFor: ifTrue: [
			^ self scanCategory: tokens fourth class: firstToken meta: true stamp: stamp]].

	secondToken == #commentStamp:
		ifTrue: [
			stamp _ tokens third.
			self addItem:
					(ChangeRecord new file: file position: file position type: #classComment
									class: firstToken category: nil meta: false stamp: stamp)
					text: 'class comment for ' , firstToken, 
						  (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).
			file nextChunk.
			^ self]! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 12/4/2012 23:40'!
install: aFileStream
	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."
	| localName newCodePackage |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	classesToRemove notEmpty ifTrue: [
		('classesToRemove: ', classesToRemove printString) print.
		'=============' print ].
	methodsToRemove notEmpty ifTrue: [
		'methodsToRemove: ' print.
		methodsToRemove do: [ :methodReference | methodReference print ].
		'=============' print ].
	
	"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"

	"Crear, instalar y devolver una instancia de PackageInfo"
	newCodePackage _ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: true.
	newCodePackage
		fullFileName: fullName;
		sourceSystem: sourceSystem;
		description: description.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: newCodePackage packageName do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ]].
	newCodePackage hasUnsavedChanges: false.
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSorter removeChangeSet: (ChangeSet changeSetForPackage: newCodePackage).
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !


!Collection methodsFor: 'enumerating' stamp: 'jmv 12/4/2012 23:39'!
collect: aBlock andFold: binaryBlock
	"Evaluate the block with the first two elements of the receiver,
	 then with the result of the first evaluation and the next element,
	 and so on.  Answer the result of the final evaluation. If the receiver
	 is empty, raise an error. If the receiver has a single element, answer
	 that element."
	"
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each ] andFold: [:a :b | a, ' ', b]
	#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') collect: [ :each | each  size] andFold: [:a :b | a + b]
	"

	| first eachValue nextValue |
	first _ true.
	self do: [ :each |
		eachValue _  aBlock value: each.
		first
			ifTrue: [
				first _ false.
				nextValue _ eachValue ]
			ifFalse: [
				nextValue _ binaryBlock
					value: nextValue
					value: eachValue ]].
	first ifTrue: [ self errorEmptyCollection ].
	^ nextValue! !


!DisplayObject methodsFor: 'displaying-Display' stamp: 'jmv 12/4/2012 23:41'!
slideFrom: startPoint to: stopPoint nSteps: nSteps 
	"does not display at the first point, but does at the last"
	| i p delta |
	i _ 0.
	p _ startPoint.
	delta _ (stopPoint-startPoint) // nSteps.
	^ self follow: [p _ p+delta]
		while: [(i _ i+1) < nSteps]! !


!FFT methodsFor: 'initialization' stamp: 'jmv 12/4/2012 23:41'!
nu: order
	"Initialize variables and tables for transforming 2^nu points"
	|  j perms k |
	nu _ order.
	n _ 2 bitShift: nu-1.

	"Initialize permutation table (bit-reversed indices)"
	j _ 0.
	perms _ WriteStream on: (Array new: n).
	0 to: n-2 do:
		[:i |
		i < j ifTrue: [perms nextPut: i+1; nextPut: j+1].
		k _ n // 2.
		[k <= j] whileTrue: [j _ j-k.  k _ k//2].
		j _ j + k].
	permTable _ perms contents asWordArray.

	"Initialize sin table 0..pi/2 in n/4 steps."
	sinTable _ (0 to: n/4) collect: [:i | (i asFloat / (n//4) * Float pi / 2.0) sin].
	
	sinTable _ sinTable asFloatArray.
	realData _ FloatArray new: n.
	imagData _ FloatArray new: n.

	self initializeHammingWindow: 0.54.  "0.54 for Hamming, 0.5 for hanning"! !


!RunArray methodsFor: 'copying' stamp: 'jmv 12/4/2012 23:41'!
copyFrom: start to: stop
	| newRuns run1 run2 offset1 offset2 answer | 
	stop < start ifTrue: [
		answer _ RunArray new.
		answer canJoinMessage: canJoinMessage.
		^answer ].
	self at: start setRunOffsetAndValue: [ :r :o :value1 |
		run1 _ r.
		offset1 _ o. 
		value1 ].
	self at: stop setRunOffsetAndValue: [ :r :o :value2 |
		run2 _ r.
		offset2 _ o.
		value2].
	run1 = run2
		ifTrue: [
			newRuns _ Array with: offset2 - offset1 + 1]
		ifFalse: [
			newRuns _ runs copyFrom: run1 to: run2.
			newRuns at: 1 put: (newRuns at: 1) - offset1.
			newRuns at: newRuns size put: offset2 + 1 ].
	answer _ RunArray runs: newRuns values: (values copyFrom: run1 to: run2).
	answer canJoinMessage: canJoinMessage.
	^answer! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 12/4/2012 23:42'!
privateStyle: anInterval
	| ranges end start |
	start _ anInterval first.
	end _ anInterval last.
	ranges _ self rangesIn: (formattedText copyFrom: start to: end) setWorkspace: true.
	ranges ifNotNil: [
		self setAttributesIn: formattedText fromRanges: ranges in: anInterval ]! !


!SHTextStylerST80 class methodsFor: 'style table' stamp: 'jmv 12/4/2012 23:42'!
attributeArrayForColor: aColorOrNil emphasis: anEmphasisSymbolOrArrayorNil
	"Answer a new Array containing any non nil TextAttributes specified"
	| answer emphArray |

	answer _ #().
	aColorOrNil ifNotNil: [ answer _ answer, {TextColor color: aColorOrNil} ].
	anEmphasisSymbolOrArrayorNil ifNotNil: [
		emphArray _ anEmphasisSymbolOrArrayorNil isSymbol 
			ifTrue: [ {anEmphasisSymbolOrArrayorNil} ] 
			ifFalse: [ anEmphasisSymbolOrArrayorNil ].
		emphArray do: [ :each |
			each ~= #normal
				ifTrue: [
					answer _ answer, {TextEmphasis perform: each}]]].
	^answer! !


!SampledSound methodsFor: 'sound tracks' stamp: 'jmv 12/4/2012 23:42'!
volumeForm: height from: start to: stop nSamplesPerPixel: nPerPixel
	"Note: nPerPixel can be Integer or Float for pixel-perfect alignment."
	"In an inspector of a samplesSound...
		self currentWorld addMorph: (ImageMorph new image:
			(self volumeForm: 32 from: 1 to: samples size nSamplesPerPixel: 225))
	"
	| volPlot width sample min max vol |
	width _ stop-start//nPerPixel.
	volPlot _ Form extent: width@height.
	(start max: 1) to: (stop min: samples size)-nPerPixel by: nPerPixel do:
		[:i | min _ max _ 0.
		i asInteger to: (i+nPerPixel-1) asInteger by: 4 do:  "by: 4 makes it faster yet looks the same"
			[:j | sample _ samples at: j.
			sample < min ifTrue: [min _ sample].
			sample > max ifTrue: [max _ sample]].
		vol _ (max - min) * height // 65536.
		volPlot fillBlack: ((i-start//nPerPixel) @ (height-vol//2) extent: 1@(vol+1))].
	^ volPlot
	
! !

