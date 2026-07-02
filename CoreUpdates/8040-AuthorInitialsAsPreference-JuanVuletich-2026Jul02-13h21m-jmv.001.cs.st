'From Cuis7.9 [latest update: #8037] on 2 July 2026 at 1:46:08 pm'!

!PreferenceSet methodsFor: 'accessing' stamp: 'jmv 2/Jul/2026, 5:15:29 pm (UTC)'!
at: symbolName ifPresent: aBlock ifAbsent: absentBlock
	(contents includesKey: symbolName)
		ifFalse: [ ^absentBlock value].
	^ aBlock value: (self instanceAt: symbolName) value! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 2/Jul/2026, 5:30:47 pm (UTC)'!
setAuthorInitials: aStringInitials name: aStringName
	"Set author name and initials programatically."

	Preferences name: #authorInitials category: #programming value: aStringName.
	Preferences saveToDisk: #authorInitials.
	Preferences name: #authorName category: #programming value: aStringInitials.
	Preferences saveToDisk: #authorName.! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 2/Jul/2026, 5:08:57 pm (UTC)'!
preambleTemplate
	"Answer a string that will form the default contents for a change set's preamble.
	Just a first stab at what the content should be."

	^ String streamContents: [:strm |
		strm nextPutAll: '"Change Set:'.  "NOTE: fileIn recognizes preambles by this string."
		strm tab;tab; nextPutAll: self name.
		strm newLine; nextPutAll: 'Date:'; tab; tab; tab; nextPutAll: Date today printString.
		strm newLine; nextPutAll: 'Author:'; tab; tab; tab; nextPutAll: Utilities authorName.
		strm newLine; newLine; nextPutAll: '<your descriptive text goes here>"']
"
ChangeSet changeSetForBaseSystem preambleTemplate
"! !


!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 2/Jul/2026, 5:08:31 pm (UTC)'!
installMiscPreferences
" Preferences with value defined as a closure. We could insert in a dynamic array...
  Preferences with an event handler.
  Preferences with default value of anything but simple literals."
	self sysPreferences

		name: #haloHandleSize 
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 5 // 6 * 2 max: 16];

		name: #halosMorphBoundsFrameColor
		description: 'When set, use this color for a rectangle on halos morph bounds. Useful for debugging morph sizes.' 
		category: #gui 
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;

		name: #halosMorphBoundsFillColor
		description: 'When set, use this color for filling a rectangle on halos morph bounds. Useful for debugging morph sizes.'
		category: #gui
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;

		name: #roundedButtonRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 8 // 14];

		name: #roundedWindowRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize] ;

		name: #scrollbarThickness 
		category: #gui 		
		value: [(Preferences at: #windowTitleFont) pointSize + 2];

		name: #backgroundEffect
		description: 'determines how backgroundImage is applied'
		category: #gui
		type: Symbol
		value: #stretch;

		name: #classFinder 
		category: #programming 
		value: [ Smalltalk findAndBrowseClass ];

		name: #worldMenu
		category: #system
		value: TheWorldMenu;

		name: #browserWindowClass
		category: #programming
		value: BrowserWindow.

	Preferences
		name: #haltThrowsUnhandledError
		description: 'If set, the Halt exception will be catched by an UnhandledError exception handler.'
		category: #system type: Boolean value: false.! !


!Utilities class methodsFor: 'cached state access' stamp: 'jmv 2/Jul/2026, 5:23:37 pm (UTC)'!
releaseClassState
	"
	Utilities releaseClassState
	"! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 2/Jul/2026, 5:27:59 pm (UTC)'!
authorInitials
	"Answer the initials to be used to identify the current code author.  "
	"
	Utilities clearAuthor; authorInitials
	"
	ChangeSet notInstallOrTestRun ifFalse: [
		^ ChangeSet installingString ].

	[(Preferences includesKey: #authorInitials) not or: [(Preferences at: #authorInitials) isEmpty]]
		whileTrue: [self setAuthor].
	^ Preferences at: #authorInitials! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 2/Jul/2026, 5:25:28 pm (UTC)'!
authorInitialsPerSe
	"Answer the currently-prevailing author initials, such as they, empty or not"

	(Preferences includesKey: #authorInitials)
		ifFalse: [ ^nil ].
	^ Preferences at: #authorInitials! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 2/Jul/2026, 5:28:24 pm (UTC)'!
authorName
	"
	Utilities clearAuthor; authorName
	"
	[(Preferences includesKey: #authorName) not or: [(Preferences at: #authorName) isEmpty]]
		whileTrue: [self setAuthor].
	^ Preferences at: #authorName! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 2/Jul/2026, 5:27:24 pm (UTC)'!
clearAuthor
	(Preferences includesKey: #authorInitials) ifTrue: [
		Preferences removeKey: #authorInitials ].
	(Preferences includesKey: #authorName) ifTrue: [
		Preferences removeKey: #authorName ].! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 2/Jul/2026, 5:45:20 pm (UTC)'!
setAuthor
	"Put up a dialog allowing the user to specify the author's initials.
	Utilities setAuthor
	"
	| authorInitials authorName |
	
	authorInitials := (self
		request: 'Please type your initials: '
		initialAnswer: (self authorInitialsPerSe ifNil: [''])) withBlanksTrimmed.
		
	Smalltalk knownInitialsAndNames
		detect: [ :pair |
			pair first = authorInitials ]
		ifFound: [ :pair |
			authorName := pair second withBlanksTrimmed ]
		ifNone: [
			authorName := (self
				request: 'Please type your name:'
				initialAnswer: 'Your Name') withBlanksTrimmed.
			self setAuthorInitials: authorInitials name: authorName.
			^ self ].
		
	(self confirm: 'Are you ' , authorName , '?')
		ifTrue: [
			self setAuthorInitials: authorInitials name: authorName ]
		ifFalse: [
			self inform: 'Please enter different initials, then'.
			self setAuthor ].! !

!methodRemoval: Utilities class #setAuthorName:initials: stamp: 'jmv 2/Jul/2026, 5:31:05 pm (UTC)'!
Utilities class removeSelector: #setAuthorName:initials:!
!classDefinition: #Utilities category: #'System-Support'!
Object subclass: #Utilities
	instanceVariableNames: ''
	classVariableNames: 'LastStats LogsUserChanges'
	poolDictionaries: ''
	category: 'System-Support'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Preferences name: #authorInitials category: #programming value: ''.
	Preferences saveToDisk: #authorInitials.
	Preferences name: #authorName category: #programming value: ''.
	Preferences saveToDisk: #authorName.!

