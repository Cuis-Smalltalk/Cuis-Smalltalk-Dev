'From Cuis 4.1 of 12 December 2012 [latest update: #1624] on 5 March 2013 at 11:42:32 am'!

!CodeProvider methodsFor: 'shout styling' stamp: 'jmv 3/5/2013 11:15'!
isModeStyleable
	"determine if Shout can style in the current mode"
	^ self showingSource or: [self showingPrettyPrint or: [self showingDecompile]]! !

!ChangeList methodsFor: 'accessing' stamp: 'jmv 3/5/2013 11:34'!
contentsSymbol

	self currentChange ifNotNil: [ :change |
		(self isNewMethod: change) ifTrue: [ ^#source ]].
	^super contentsSymbol! !

!ChangeList methodsFor: 'testing' stamp: 'jmv 3/5/2013 11:32'!
isNewMethod: change
	| class |
	^change changeType == #method and: [
		((class _ change changeClass) isNil) or: [
			(class includesSelector: change methodSelector) not]]! !


!CodeProvider methodsFor: 'diffs' stamp: 'jmv 3/5/2013 11:29'!
shouldDiffWords
	"Answer whether the receiver is currently set to use the word based differ"

	^ #(wordDiffs prettyWordDiffs) includes: self contentsSymbol! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 3/5/2013 11:28'!
showingAnyKindOfDiffs
	"Answer whether the receiver is currently set to show any kind of diffs"

	^ #(lineDiffs prettyLineDiffs wordDiffs prettyWordDiffs) includes: self contentsSymbol! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 3/5/2013 11:29'!
showingAnyKindOfPrettyDiffs
	"Answer whether the receiver is currently set to show any kind of pretty diffs"

	^ #(prettyLineDiffs prettyWordDiffs) includes: self contentsSymbol! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 3/5/2013 11:29'!
showingLineDiffs
	"Answer whether the receiver is showing regular diffs of source code"

	^ self contentsSymbol == #lineDiffs
! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 3/5/2013 11:28'!
showingPrettyLineDiffs
	"Answer whether the receiver is showing pretty diffs of source code"

	^ self contentsSymbol == #prettyLineDiffs! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 3/5/2013 11:28'!
showingPrettyWordDiffs
	"Answer whether the receiver is showing pretty diffs of source code"

	^ self contentsSymbol == #prettyWordDiffs
! !

!CodeProvider methodsFor: 'diffs' stamp: 'jmv 3/5/2013 11:29'!
showingWordDiffs
	"Answer whether the receiver is showing regular diffs (alternative algorithm) of source code"

	^ self contentsSymbol == #wordDiffs
! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 3/5/2013 11:28'!
prettyPrintString
	"Answer whether the receiver is showing pretty-print"

	^ ((self contentsSymbol == #prettyPrint)
		ifTrue:
			['<yes>']
		ifFalse:
			['<no>']), 'prettyPrint'! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 3/5/2013 11:28'!
showByteCodes: aBoolean
	"Get into or out of bytecode-showoing mode"

	aBoolean
		ifTrue: [
			self contentsSymbol: #byteCodes]
		ifFalse: [
			self contentsSymbol == #byteCodes ifTrue: [
				self contentsSymbol: #source]]! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 3/5/2013 11:28'!
showingByteCodes
	"Answer whether the receiver is showing bytecodes"

	^ self contentsSymbol == #byteCodes! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 3/5/2013 11:29'!
showingPlainSource
	"Answer whether the receiver is showing plain source"

	^ self contentsSymbol == #source! !

!CodeProvider methodsFor: 'what to show' stamp: 'jmv 3/5/2013 11:28'!
showingPrettyPrint
	"Answer whether the receiver is showing pretty-print"

	^ self contentsSymbol == #prettyPrint! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 3/5/2013 11:32'!
selectNewMethods
	"Selects all method definitions for which there is no counterpart method in the current image"

	Cursor read showWhile: [
		| change class |
		1 to: changeList size do: [ :i | 
			change _ changeList at: i.
			listSelections at: i put: (self isNewMethod: change)]].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 3/5/2013 11:33'!
annotation
	"Answer the string to be shown in an annotation pane.  Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact."

	| change count selectedCount ann1 ann2 |
	change _ self currentChange.
	
	change isNil ifTrue: [
		count _ listSelections size.
		selectedCount _ listSelections count: [ :flag | flag ].
		^ 'Total items: ', count printString, ' - Selected items: ', selectedCount printString ].

	change changeType == #classDefinition ifTrue: [
		ann1 _ change isMetaClassChange ifTrue: [ 'Metaclass' ] ifFalse: [ 'Class' ].
		ann2 _ (Smalltalk includesKey: change changeClassName) ifTrue: [ ' already exists' ] ifFalse: [ ' not in system' ].
		^ann1, ann2 ].
	
	(self selectedMessageName isNil or: [self selectedClassOrMetaClass isNil])
		ifTrue: [^ '------'].

	^ change methodSelector notNil
		ifFalse: [ super annotation]
		ifTrue: [
			(self isNewMethod: change)
				ifTrue: [
					change changeClassName, ' >> ', change methodSelector, ' is not present in the system.']
				ifFalse: [
					'current version: ', super annotation]]! !

!ChangeList methodsFor: 'shout styling' stamp: 'jmv 3/5/2013 11:25'!
shoutAboutToStyle: aSHTextStyler
	"This is a notification that aSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in aSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [^false].
	self selectedClassOrMetaClass ifNotNil: [ :cl |
		aSHTextStyler classOrMetaClass: cl.
		^true ].
	^false! !


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 3/5/2013 11:29'!
acceptedStringOrText
	"return the source code that shows in the bottom pane"

	| sel class strm changeType answer |
	self changed: #clearUserEdits.
	currentClassName ifNil: [^ myChangeSet preambleString ifNil: ['']].
	class _ self selectedClassOrMetaClass.
	(sel _ currentSelector)
		ifNotNil: [
			changeType _ (myChangeSet atSelector: (sel _ sel asSymbol) class: class).
			changeType == #remove
				ifTrue: [^'Method has been removed (see versions)'].
			changeType == #addedThenRemoved
				ifTrue: [^'Added then removed (see versions)'].
			changeType == #movedToOtherPackage
				ifTrue: [^'Method was moved to some other package'].
			class ifNil: [^'Method was added, but cannot be found!!'].
			(class includesSelector: sel)
				ifFalse: [^'Method was added, but cannot be found!!'].
			answer _  (class sourceCodeAt: sel).
			(#(prettyPrint prettyLineDiffs prettyWordDiffs) includes: self contentsSymbol) ifTrue: [
				answer _ (class compilerClass new
						format: answer
						in: class 
						notifying: nil)].
			self showingAnyKindOfDiffs
				ifTrue: [ answer _ (self diffFromPriorSourceFor: answer) ].
			^ answer asText makeSelectorBoldIn: class ]
		ifNil: [
			strm _ WriteStream on: (String new: 100).
			(myChangeSet classChangeAt: currentClassName) do: [ :each |
				each == #remove ifTrue: [strm nextPutAll: 'Entire class was removed.'; newLine].
				each == #addedThenRemoved ifTrue: [strm nextPutAll: 'Class was added then removed.'; newLine].
				each == #rename ifTrue: [strm nextPutAll: 'Class name was changed.'; newLine].
				each == #add ifTrue: [strm nextPutAll: 'Class definition was added.'; newLine].
				each == #change ifTrue: [strm nextPutAll: 'Class definition was changed.'; newLine].
				each == #reorganize ifTrue: [strm nextPutAll: 'Class organization was changed.'; newLine].
				each == #comment ifTrue: [strm nextPutAll: 'New class comment.'; newLine].
				each == #movedToOtherPackage ifTrue: [strm nextPutAll: 'Class was moved to some other package.'; newLine].
			].
			^ strm contents].! !

