'From Cuis 4.0 of 21 April 2012 [latest update: #1309] on 17 June 2012 at 12:57:14 pm'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 6/17/2012 12:49'!
selectAll
	listIndex _ 0.
	listSelections atAllPut: true.
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 6/17/2012 12:49'!
selectEquivalentMethods
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	Cursor wait showWhile: [
		1 to: changeList size do: [ :i | 
			| change class |
			change _ changeList at: i.
			listSelections at: i put:
				((change type == #method and: [
					(class _ change methodClass) notNil]) and: [
						(class includesSelector: change methodSelector) and: [
							| cmWithNode |
							cmWithNode _ [class basicCompile: change string notifying: nil trailer: class defaultMethodTrailer ifFail: nil] 
								on: SyntaxErrorNotification do: [ :ex | ex return ].
							(cmWithNode notNil and: [
								| current inChange |
								current _ (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
								inChange _cmWithNode method copyWithTrailerBytes: #(0).
								current = inChange or: [
									| currentCmWithNode |
									currentCmWithNode _ [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
											notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ].
									(currentCmWithNode notNil and: [
										current _ currentCmWithNode method copyWithTrailerBytes: #(0).
										current = inChange])
								]
							])
						]]
				)]].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 6/17/2012 12:50'!
selectMethodsForAbsentClasses
	"Selects all method definitions for which there is no counterpart method in the current image"

	Cursor read showWhile: [
		| change |
		1 to: changeList size do: [ :i | 
			change _ changeList at: i.
			listSelections at: i put:
				((change type == #method and:
					[change methodClass isNil]))]].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 6/17/2012 12:50'!
selectNewMethods
	"Selects all method definitions for which there is no counterpart method in the current image"

	Cursor read showWhile: [
		| change class |
		1 to: changeList size do: [ :i | 
			change _ changeList at: i.
			listSelections at: i put:
				((change type == #method and:
					[((class _ change methodClass) isNil) or:
						[(class includesSelector: change methodSelector) not]]))]].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 6/17/2012 12:50'!
selectRemovalsOfSent
	"Selects all method removal for sent methods"

	Cursor read showWhile: [
		1 to: changeList size do: [ :i | | change |
			change _ changeList at: i.
			listSelections at: i put:
				(change type = #doIt and: [
					change string includesSubString: 'removeSelector: #' ] and: [
						Smalltalk isThereAReferenceTo: (change string copyAfterLast: $#) asSymbol ]) ]].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 6/17/2012 12:50'!
selectSuchThat: aBlock
	"select all changes for which block returns true"
	listSelections _ changeList collect: aBlock.
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 6/17/2012 12:50'!
selectUnchangedMethods
	"Selects all method definitions for which there is already a method in the current image, whose source is exactly the same.  9/18/96 sw"

	Cursor read showWhile: [
		| change class |
		1 to: changeList size do: [ :i | 
			change _ changeList at: i.
			listSelections at: i put:
				((change type == #method and:
					[(class _ change methodClass) notNil]) and:
						[(class includesSelector: change methodSelector) and:
							[change string = (class sourceCodeAt: change methodSelector) asString ]])]].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 6/17/2012 12:42'!
annotation
	"Answer the string to be shown in an annotation pane.  Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact."

	| annot change count selectedCount ann1 ann2 aClass |
	change _ self currentChange.
	
	change isNil ifTrue: [
		count _ listSelections size.
		selectedCount _ listSelections count: [ :flag | flag ].
		^ 'Total items: ', count printString, ' - Selected items: ', selectedCount printString ].

	change type == #classDefinition ifTrue: [
		ann1 _ change isMetaClassChange ifTrue: [ 'Metaclass' ] ifFalse: [ 'Class' ].
		ann2 _ (Smalltalk includesKey: change methodClassName) ifTrue: [ ' already exists' ] ifFalse: [ ' not in system' ].
		^ann1, ann2 ].
	
	annot _ super annotation.
	annot asString = '------' ifTrue: [^ annot].

	^ change methodSelector notNil
		ifFalse: [ annot]
		ifTrue: [
			((aClass _ change methodClass) isNil or: [(aClass includesSelector: change methodSelector) not])
				ifTrue: [
					change methodClassName, ' >> ', change methodSelector, ' is not present in the system.']
				ifFalse: [
					'current version: ', annot]]! !

!ChangeList methodsFor: 'accessing' stamp: 'jmv 6/17/2012 11:57'!
currentChange
	"return the current change being viewed, or nil if none"

	^ listIndex = 0
		ifFalse: [ changeList at: listIndex ]! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 6/17/2012 12:14'!
helpMenu
        "Build the help menu for the world."
        |  menu |

  	menu := self menu: 'Help...'.

        self fillIn: menu from:
        {
                {'About this System...'. {Smalltalk. #aboutThisSystem}. 'current version information.'}.
                {'Preferences...'. {Preferences. #openPreferencesInspector}. 'view and change various options.'}.
                nil.
               {'Editor keyboard shortcuts'. { SmalltalkEditor . #openHelp}. 'summary of keyboard shortcuts in editors for Smalltalk code.'}
	}.

	self addGestureHelpItemsTo: menu.

	self fillIn: menu from:
	{
                {'World menu Help'. { self . #worldMenuHelp}. 'helps find menu items buried in submenus.'}.
                {'Useful Expressions' . { Utilities . #openStandardWorkspace}. 'a window full of useful expressions.'}.
                nil.

                {'Set Code Author...' . { Utilities . #setAuthor }. 'supply initials to be used to identify the author of code and other content.'}.
                {'VM Statistics' . { self . #vmStatistics}.  'obtain some intriguing data about the vm.'}.
			nil.
                {'Space Left' . { self . #garbageCollect}. 'perform a full garbage-collection and report how many bytes of space remain in the image.'}.
        }.

	^menu

! !

