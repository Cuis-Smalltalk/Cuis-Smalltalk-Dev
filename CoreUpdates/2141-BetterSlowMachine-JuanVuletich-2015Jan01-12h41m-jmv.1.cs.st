'From Cuis 4.2 of 25 July 2013 [latest update: #2140] on 1 January 2015 at 9:26:45.04955 pm'!

!MatrixTransform2x3 methodsFor: 'testing' stamp: 'jmv 1/1/2015 20:43'!
isTranslation: aPoint
	"The position of 0@0 in the external coordinate system.
	It is the translation we apply when transforming points."
	^self a13 = aPoint x and: [ self a23 = aPoint y ]! !


!Morph methodsFor: 'layout' stamp: 'jmv 12/30/2014 19:08'!
minItemWidth
	^self morphWidth! !


!MessageTally methodsFor: 'reporting' stamp: 'jmv 1/1/2015 12:58'!
reportGCStatsOn: str
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows |
	upTime _ time.
	oldSpaceEnd		_ gcStats at: 1.
	youngSpaceEnd		_ gcStats at: 2.
	memoryEnd			_ gcStats at: 3.
	fullGCs				_ gcStats at: 7.
	fullGCTime			_ gcStats at: 8.
	incrGCs				_ gcStats at: 9.
	incrGCTime			_ gcStats at: 10.
	tenureCount			_ gcStats at: 11.
	rootOverflows		_ gcStats at: 22.

	str newLine.
	str	nextPutAll: '**Memory**'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) printStringWithCommas; nextPutAll: ' bytes'; newLine.

	str newLine.
	str	nextPutAll: '**GCs**'; newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: (fullGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: (fullGCTime / fullGCs) rounded; nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr		';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: (incrGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	incrGCs = 0 ifFalse:
		[str nextPutAll:', avg '; print: (incrGCTime / incrGCs) rounded; nextPutAll: 'ms'].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
rootOverflows ifNotNil: [
	str	nextPutAll: '	root table	';
		nextPutAll: rootOverflows printStringWithCommas; nextPutAll:' overflows'.
	str newLine ]! !


!Morph methodsFor: 'geometry' stamp: 'jmv 1/1/2015 20:46'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	(location isTranslation: aPoint) ifTrue: [
		^ self ].		"Null change"

	self redrawNeeded.
	location setTranslation: aPoint.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 1/1/2015 20:46'!
morphPositionInWorld: newPositionInWorld
	"Change the position of this morph."
	"El tema es, que tipo de coordenadas tenemos?
	En un mundo relativista, no hay un marco de referencia absoluto.
	No tiene sentido hablar de coordenadas del mundo... El mundo podria estar escalado... 
		Que tienen de especial las coordenadas del mundo?
	Coordenadas 'del hardware'? No deberia saber mucho sobre el... Puede haber multiples displays, hands de diverso tipo, remotas, virtuales...
	
	En ppio, un par de coordenadas pueden ser relativas a cualquier morph. Pareciera que necesito metodos de conversion de cualquier morph hacia mi, y de mi hacia cualquier morph... Como encontrar un marco de referencia comun????
	Dejar esto para despues. En realidad, para empezar, preciso menos: Solo preciso saber si las coordenadas estan en el morph o en su owner. Nada mas. Los eventos se iran transformando apropiadamente al moverse por el arbol, o al menos, llevaran consigo una transformacion (MatrixTransform2x3) que se ira actualizando"

	| newPositionInOwner |
	self flag: #jmvVer2.
	"This method MUST die"

	newPositionInOwner _ owner
		ifNotNil: [ owner internalizeFromWorld: newPositionInWorld ]
		ifNil: [ newPositionInWorld ].

	(location isTranslation: newPositionInOwner) ifTrue: [
		^ self ].		"Null change".

	self redrawNeeded.
	location setTranslation: newPositionInOwner.
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'private' stamp: 'jmv 1/1/2015 20:46'!
privatePosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	(location isTranslation: aPoint) ifTrue: [
		^ self ].		"Null change"

	location setTranslation: aPoint! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 1/1/2015 20:46'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| oldBoundsInWorld someChange |

	"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
	oldBoundsInWorld _ self morphBoundsInWorld.
	someChange _ false.
	(location isTranslation: newPos) ifFalse: [
		location setTranslation: newPos.
		someChange _ true ].

	extent = newExtent ifFalse: [
		(self privateExtent: newExtent) ifTrue: [
			someChange _ true ]].

	someChange ifTrue: [
		"Ask for the old bounds before updating them, but ask for repair only if extent was really changed."
		oldBoundsInWorld ifNotNil: [
			self invalidateDisplayRect: oldBoundsInWorld from: nil ].
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 1/1/2015 20:55'!
addStayUpIcons
	| closeBox pinBox w |
	Preferences optionalButtons ifFalse: [ ^self ].
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	closeBox _ PluggableButtonMorph model: self action: #delete.
	closeBox icon: Theme current closeIcon.
	pinBox _ PluggableButtonMorph model: self action: #stayUp.
	pinBox icon: Theme current pushPinIcon.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 60.
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: 19);
			color: Color transparent;
			addMorph: closeBox fixedWidth: 20;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (RectangleLikeMorph new color: Color transparent) fixedWidth: 4;
			addMorph: pinBox fixedWidth: 20).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'private' stamp: 'jmv 12/30/2014 19:09'!
adjustSubmorphsLayout
	"Enlarge the width of submorphs as needed
	so all of them are have the same width, and no less than #minWidth.
	Also adjust their vertical position.
	Finally, set our own extent."
	
	| w p h |
	
	submorphs isEmpty ifTrue: [ ^self ].
	w _ submorphs inject: 0 into: [ :prev :each |
		prev max: each minItemWidth].

	w _ w + 1.
	p _ 5 @ 5.
	submorphs do: [ :m |
		h _ m morphHeight.
		m morphPosition: p extent: w@h.
		p _ p + (0@(h + 1)) ].

	self morphExtent: w+4 @ p y + 5! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 1/1/2015 21:25'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences optionalButtons ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 1/1/2015 21:02'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations comment separator |
	comment _ self buildMorphicCommentPane.
	separator _ LayoutAdjustingMorph new.
	comment separator: separator.
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 2.0;
		addMorph: separator fixedHeight: 4;
		addMorph: comment proportionalHeight: 2.0.
	Preferences optionalButtons ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!MessageSetWindow methodsFor: 'GUI building' stamp: 'jmv 1/1/2015 21:05'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations |
	codeAndButtons _ LayoutMorph newColumn.
	Preferences optionalButtons ifTrue: [
		codeAndButtons
			addMorph: self optionalButtonRow fixedHeight: self defaultButtonPaneHeight;
			addAdjusterMorph ].
	codeAndButtons
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences optionalButtons ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'jmv 1/1/2015 21:08'!
buildLowerPanes
	| codeAndButtons codeButtonsAndAnnotations twoRowsOfButtons h |
	twoRowsOfButtons _ LayoutMorph newColumn.
	h _ self defaultButtonPaneHeight.
	Preferences optionalButtons ifTrue: [
		h _ self defaultButtonPaneHeight * 2.
		twoRowsOfButtons
			addMorph: self optionalButtonRow proportionalHeight: 1.0;
			addAdjusterMorph ].
	twoRowsOfButtons
		addMorph: self customButtonRow proportionalHeight: 1.0.
	codeAndButtons _ LayoutMorph newColumn.
	codeAndButtons
		addMorph: twoRowsOfButtons fixedHeight: h;
		addAdjusterMorph;
		addMorph: self buildMorphicCodePane proportionalHeight: 1.0.
	Preferences optionalButtons ifFalse: [
		^codeAndButtons ].
	codeButtonsAndAnnotations _ LayoutMorph newColumn.
	codeButtonsAndAnnotations
		addMorph: self buildMorphicAnnotationsPane fixedHeight: self defaultAnnotationPaneHeight;
		addAdjusterMorph;
		addMorph: codeAndButtons proportionalHeight: 1.0.
	^codeButtonsAndAnnotations! !


!MinimalStringMorph methodsFor: 'accessing' stamp: 'jmv 1/1/2015 21:16'!
fitContents
	"Measures contents later at #minimumExtent"
	self morphExtent: 0@0! !


!OneLineEditorMorph methodsFor: 'accessing' stamp: 'jmv 1/1/2015 21:16'!
fitContents
	"Measures contents later at #minimumExtent"
	self morphExtent: 0@0! !


!StringMorph methodsFor: 'accessing' stamp: 'jmv 1/1/2015 21:16'!
fitContents
	"Measures contents later at #minimumExtent"
	self morphExtent: 0@0! !

!StringMorph methodsFor: 'initialization' stamp: 'jmv 12/30/2014 18:55'!
initialize
	super initialize.
	font _ nil.
	emphasis _ 0.
	contents _ 'String Morph'
! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 1/1/2015 21:12'!
cuisDefaults
	"
	Preferences cuisDefaults
	"
	self setPreferencesFrom:

	#(
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds false)
		(checkForSlips true)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl true)
		(optionalButtons true)
		(extraDebuggerButtons true)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(allowUnderscoreAssignments true)
		(allowUnderscoreSelectors true)
		(syntaxHighlightingAsYouTypeAnsiAssignment false)
		(syntaxHighlightingAsYouTypeLeftArrowAssignment false)
		(cheapWindowReframe false)
	)! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 1/1/2015 21:23'!
slowMachine
	"
	Preferences slowMachine
	"
	self setPreferencesFrom: #(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe true)
		(tapAndHoldEmulatesButton2 false)
		(syntaxHighlightingAsYouType false)
	).
	self useNoIcons.
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hide! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 1/1/2015 21:23'!
smalltalk80
	"A traditional monochrome Smalltalk-80 look and feel, clean and austere, and lacking many features added to Squeak and Cuis in recent years. Caution: turns off the 'smartUpdating' feature that keeps multiple browsers in synch, and much more.

	Preferences smalltalk80
	"

	self setPreferencesFrom:

	#(
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList false)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(smartUpdating false)
		(subPixelRenderFonts true)
		(thoroughSenders false)
		(cheapWindowReframe true)
		(tapAndHoldEmulatesButton2 false)
		(syntaxHighlightingAsYouType false)
	).
	self useNoIcons.
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	Taskbar hide! !

!methodRemoval: MatrixTransform2x3 #isPosition:!
MatrixTransform2x3 removeSelector: #isPosition:!
