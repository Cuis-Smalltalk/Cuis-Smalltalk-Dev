'From Cuis7.1 [latest update: #6461] on 11 June 2024 at 10:31:04 am'!
!classDefinition: 'Editor class' category: #'System-Text'!
Editor class
	instanceVariableNames: 'shortcuts cmdShortcuts keyboardActions '!

!Editor methodsFor: 'typing support' stamp: 'jmv 6/10/2024 19:40:43'!
keyboardActions
	"Same for all instances.
	A subclass could handle specific keyboard actions for each instance, though."
	^self class keyboardActions! !


!Editor class methodsFor: 'class initialization' stamp: 'jmv 6/10/2024 19:39:12'!
initializeKeyboardActions
	"Initialize the table for regular (i.e. non-command) keystroke dispatch"
	"
	self initializeKeyboardActions
	"
	| actions |
	actions := Array new: 256 withAll: #normalCharacter:.
	0 to: 31 do: [ :i | actions at: i+1 put: #noop: ].
	actions at: 1 + 1 put: #cursorHome:.				"home key"
	actions at: 3 + 1 put: #enter:.						"enter / return key"
	actions at: 4 + 1 put: #cursorEnd:.				"end key"
	actions at: 5 + 1 put: #noop:.						"insert key"
	actions at: 8 + 1 put: #backspace:.				"macDelete winBackspace key"
	actions at: 9 + 1 put: #normalCharacter:.		"tab"
	actions at: 11 + 1 put: #cursorPageUp:.			"page up key"
	actions at: 12 + 1 put: #cursorPageDown:.		"page down key"
	actions
		at:  InputSensor returnKey + 1
		put: #returnKey:.									"return (sometimes labelled enter) key"
	actions at: 27 + 1 put: #offerMenuFromEsc:.	"escape key"
	actions at: 28 + 1 put: #cursorLeft:.				"left arrow key"
	actions at: 29 + 1 put: #cursorRight:.				"right arrow key"
	actions at: 30 + 1 put: #cursorUp:.				"up arrow key"
	actions at: 31 + 1 put: #cursorDown:.			"down arrow key"
	actions at: 127 + 1 put: #forwardDelete:.		"winDelete key"
	keyboardActions := actions! !

!Editor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/11/2024 10:28:08'!
keyboardActions
	"Same for all instances.
	A subclass could handle specific keyboard actions for each instance, though."
	
	keyboardActions ifNil: [ self initializeKeyboardActions ].
	^keyboardActions! !


!TextEditor class methodsFor: 'class initialization' stamp: 'jmv 6/10/2024 19:39:06'!
initializeKeyboardActions
	
	super initializeKeyboardActions.
	keyboardActions at: 9 + 1 put: #tabKey:.! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/11/2024 10:24:41'!
presumedSentMessages
	| sent |
	"
	In addition to those here, if it is desired to preserve some methods from deletion, see #nominallyUnsent:
	Smalltalk presumedSentMessages
	"

	"The following should be preserved for doIts, etc"
	sent := IdentitySet new.
	#( rehashWithoutBecome
		browseAllSelect:  lastRemoval
		vScrollBarValue: hScrollBarValue: 
		to: removeClassNamed:
		dragon: hilberts: mandala: web test3 factorial tinyBenchmarks benchFib
		newDepth: restoreAfter: zapAllMethods obsoleteClasses
		removeAllUnSentMessages abandonSources removeUnreferencedKeys
		zapOrganization condenseSources condenseChanges browseObsoleteReferences
		subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:
		startTimerEventLoop unusedClasses allClasses
		unimplemented
		reduceCuis
		variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		printSpaceAnalysis:on:) do: [ :sel |
			sent add: sel].
	"The following may be sent by perform: in dispatchOnChar..."
	Editor withAllSubclassesDo: [ :c |
		c keyboardActions asSet do: [ :sel | sent add: sel ].
		c new defaultMenuSpec do: [ :spec |
			spec size >= 5 ifTrue: [
				spec fifth isSymbol ifTrue: [ sent add: spec fifth ]]]].
	#(beReadOnlyBinding beReadWriteBinding) do: [ :sel |
		sent add: sel].
	AppLauncher appSelector ifNotNil: [ :sel |
		sent add: sel ].
	^ sent! !


!Editor class methodsFor: 'class initialization' stamp: 'jmv 6/11/2024 10:26:28'!
initialize
	"
	Editor initialize
	"
	self withAllSubclassesDo: [ :c | c initializeKeyboardActions ].! !

!Editor class methodsFor: 'cached state access' stamp: 'jmv 6/10/2024 19:41:53'!
releaseClassCachedState

	keyboardActions := nil.! !


!TextEditor methodsFor: 'typing support' stamp: 'jmv 6/10/2024 19:41:16'!
dispatchOn: aKeyboardEvent
	"Carry out the action associated with this character, if any."

	| codePoint asciiValue c |
	self clearParens.
  	codePoint := aKeyboardEvent keyValue.
	
	codePoint > 255 ifTrue: [
		^self normalCharacter: aKeyboardEvent ].

	c := aKeyboardEvent keyCharacter.
	(')]}' includes: c)
		ifTrue: [ self blinkPrevParen: c ].

	asciiValue := codePoint.
	^ self perform: (self keyboardActions at: asciiValue + 1) with: aKeyboardEvent.! !


!TextEditor class methodsFor: 'cached state access' stamp: 'jmv 6/10/2024 19:42:01'!
releaseClassCachedState

	FindText := nil.
	ChangeText := nil.
	"We are not calling super to avoid cleansing class vars many times.
	So, repeat inherited class instVars!!"
	keyboardActions := nil.! !


!SmalltalkEditor class methodsFor: 'cached state access' stamp: 'jmv 6/10/2024 19:41:57'!
releaseClassCachedState

	"We are not calling super to avoid cleansing class vars many times.
	So, repeat inherited class instVars!!"
	keyboardActions := nil.! !

!methodRemoval: SmalltalkEditor #createMenuCollectingOptionsWith: stamp: 'jmv 6/11/2024 10:25:48'!
SmalltalkEditor removeSelector: #createMenuCollectingOptionsWith:!
!methodRemoval: TextEditor class #initializeShortcuts stamp: 'jmv 6/11/2024 10:25:09'!
TextEditor class removeSelector: #initializeShortcuts!
!methodRemoval: Editor class #shortcuts stamp: 'jmv 6/11/2024 10:25:01'!
Editor class removeSelector: #shortcuts!
!methodRemoval: Editor class #initializeShortcuts stamp: 'jmv 6/11/2024 10:25:11'!
Editor class removeSelector: #initializeShortcuts!
!methodRemoval: Editor #shortcuts stamp: 'jmv 6/11/2024 10:24:52'!
Editor removeSelector: #shortcuts!
Editor initialize!
!classDefinition: 'Editor class' category: #'System-Text'!
Editor class
	instanceVariableNames: 'keyboardActions'!
