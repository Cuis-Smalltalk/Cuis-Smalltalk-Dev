'From Cuis7.9 [latest update: #8296] on 28 October 2026 at 11:48:17 am'!

!TextModel methodsFor: 'accessing' stamp: 'jmv 28/Oct/2026, 2:17:57 pm (UTC)'!
actualContents: aTextOrString
	| actuallyChanged |
	actuallyChanged := self basicActualContents: aTextOrString.
	actuallyChanged
		ifTrue: [
			self changed: #actualContents ].
	^actuallyChanged! !

!TextModel methodsFor: 'accessing' stamp: 'jmv 28/Oct/2026, 2:27:16 pm (UTC)'!
basicActualContents: aTextOrString
	"Do not throw events.
	Answer whether actualContents changed."
	| prevContents |
	prevContents := actualContents.
	actualContents := aTextOrString asText asUnicodeStringOrText.

	actualContents = prevContents
		ifTrue: [ ^false ].

	undoRedoCommands resetToStart.
	lastEditTimeStamp := nil.
	^true! !

!TextModel methodsFor: 'misc' stamp: 'jmv 28/Oct/2026, 2:19:36 pm (UTC)'!
refetch
	"Nothing here. Answer true if actualContents was actually fetched."
	(self actualContents: self actualContents)
		ifTrue: [
			self changed: #refetched ].! !


!TextModel class methodsFor: 'instance creation' stamp: 'jmv 28/Oct/2026, 2:16:24 pm (UTC)'!
withText: aTextOrString

	^self new
		basicActualContents: aTextOrString;
		yourself! !


!PluggableTextModel methodsFor: 'misc' stamp: 'jmv 28/Oct/2026, 2:27:19 pm (UTC)'!
refetch
	"Answer true if actualContents was actually fetched."
	textGetter
		ifNil: [
			actualContents ifNil: [
				self actualContents: Text new ]]
		ifNotNil: [
			(self actualContents: (Text
				initialFont: (Preferences at: #standardCodeFont)
				stringOrText: (textProvider perform: textGetter)))
					ifTrue: [
						self changed: #refetched ]].! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 28/Oct/2026, 2:34:32 pm (UTC)'!
updateIfNeeded
	"Recompute all of my lists."
	| newList |
	myChangeSet ifNil: [^ self].  "Has been known to happen though shouldn't"
	myChangeSet isMoribund ifTrue: [
		self changed: #changeSetList.
		self showChangeSet: ChangeSet changeSetForBaseSystem.
		^self ].

	newList := self changeSetList.
	(priorChangeSetList == nil or: [priorChangeSetList ~= newList])
		ifTrue: [
			priorChangeSetList := newList.
			self changed: #changeSetList ].

	self acceptedContentsChanged! !


!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 28/Oct/2026, 2:32:00 pm (UTC)'!
updateIfNeeded
	| aMethod |
	aMethod := classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ self].
	aMethod == currentCompiledMethod
		ifFalse: [
			"Do not attempt to formulate if there is no source pointer.
			It probably means it has been recompiled, but the source hasn't been written
			(as during a display of the 'save text simply?' confirmation)."
			aMethod last ~= 0 ifTrue: [self reformulateList]].! !

