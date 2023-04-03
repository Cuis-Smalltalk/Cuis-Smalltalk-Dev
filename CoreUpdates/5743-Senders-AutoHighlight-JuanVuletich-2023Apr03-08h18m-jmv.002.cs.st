'From Cuis 6.0 [latest update: #5742] on 3 April 2023 at 9:35:11 am'!
!classDefinition: #MessageSet category: #'Tools-Browser'!
Browser subclass: #MessageSet
	instanceVariableNames: 'messageList autoSelectString shouldAutoHighlight '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!

!TextModel methodsFor: 'testing' stamp: 'jmv 4/3/2023 09:07:08'!
shouldAutoHighlight
	"Answer true if we do auto highlight, and it is appropriate to do it now."
	^false! !


!PluggableTextModel methodsFor: 'testing' stamp: 'jmv 4/3/2023 09:07:22'!
shouldAutoHighlight
	"Answer true if we do auto highlight, and it is appropriate to do it now."
	^textProvider shouldAutoHighlight! !

!PluggableTextModel methodsFor: 'user interface support' stamp: 'jmv 4/3/2023 09:21:39'!
justDidAutoHighlight
	textProvider justDidAutoHighlight! !


!TextProvider methodsFor: 'user interface support' stamp: 'jmv 4/3/2023 09:08:12'!
shouldAutoHighlight
	"Answer true if we do auto highlight, and it is appropriate to do it now."
	^false! !


!MessageSet methodsFor: 'shout styling' stamp: 'jmv 4/3/2023 09:22:51'!
justDidAutoHighlight
	"Don't do autoHighlight unless accepted contents are updated."
	shouldAutoHighlight := false! !

!MessageSet methodsFor: 'shout styling' stamp: 'jmv 4/3/2023 09:24:30'!
shouldAutoHighlight
	"Answer true if we do auto highlight, and it is appropriate to do it now."
	^shouldAutoHighlight! !


!ObjectExplorer methodsFor: 'user interface support' stamp: 'jmv 4/3/2023 09:22:16'!
shouldAutoHighlight
	"Answer true if we do auto highlight, and it is appropriate to do it now."
	^false! !


!TextAttribute methodsFor: 'testing' stamp: 'jmv 4/2/2023 15:16:58'!
isBackgroundColor
	^false! !


!TextBackgroundColor methodsFor: 'testing' stamp: 'jmv 4/2/2023 15:19:33'!
isBackgroundColor
	^true! !


!MessageSet methodsFor: 'message list' stamp: 'jmv 4/3/2023 09:15:33'!
messageListIndex: anInteger

	"Set the index of the selected item to be anInteger."

	| list |
	
	list := self messageList.
	selectedMessage := (list isInBounds: anInteger) ifTrue: [ list at: anInteger ].
	shouldAutoHighlight := true.
	self changed: #messageListIndex.	 "update my selection"
	self editSelection: #editMessage.
	self acceptedContentsChanged! !

!MessageSet methodsFor: 'shout styling' stamp: 'jmv 4/3/2023 08:42:07'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [ ^false ].
	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.
	^true! !


!Debugger methodsFor: 'shout styling' stamp: 'jmv 4/3/2023 08:41:55'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	anSHTextStyler 
		classOrMetaClass: self selectedClassOrMetaClass;
		workspace: self.
	^true! !


!Text class methodsFor: 'private' stamp: 'jmv 4/3/2023 08:28:40'!
addAttribute: newTextAttribute toArray: existingAttributes
	"Add a new text attribute to an existing set"
	"NOTE: The use of reset and isSet in this code is a specific
	hack for merging TextKerns.
	(It also means that TextEmphasis normal is not added. Same for TextColor black)"

	| includeNew |
	includeNew := true.
	newTextAttribute reset.
	^ Array streamContents: [ :strm |
		existingAttributes do: [ :existing |
			(newTextAttribute dominates: existing) ifFalse: [
				"Keep existing unless new dominates it."
				strm nextPut: existing.
				"If new doesn't dominate existing, perhaps existing dominates new."
				(existing dominates: newTextAttribute) ifTrue: [
					"Don't add new if an existing attribute dominates it."
					includeNew := false ]
				]].
		(includeNew and: [newTextAttribute isSet])
			 ifTrue: [ strm nextPut: newTextAttribute ]].! !


!TextBackgroundColor methodsFor: 'scanning' stamp: 'jmv 4/2/2023 15:19:11'!
dominates: other
	other isBackgroundColor ifFalse: [ ^false ].
	^ other isForShout or: [ self isForShout not ].! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 4/3/2023 09:29:13'!
privateFormatAndConvert

	textModel shouldAutoHighlight ifTrue: [
		textModel autoSelectString ifNotNil: [ :autoHighlight |
			(textModel textProvider messageSendsRangesOf: autoHighlight) do: [ :interval |
				formattedText 
					addAttribute: `TextBackgroundColor color: (Color red alpha: 0.2)` 
					from: ( interval first)
					to: ( interval last) ]].
		textModel justDidAutoHighlight ].

	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow"
			self replaceStringForRangesWithType: #ansiAssignment with: `Character smalltalkLeftArrow asString`.
			"Turn $_ into a left arrow"
			self replaceStringForRangesWithType: #assignment with: `Character smalltalkLeftArrow asString`.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: `Character smalltalkUpArrow asString` ].! !

!methodRemoval: ObjectExplorer #autoSelectString stamp: 'jmv 4/3/2023 09:32:50'!
ObjectExplorer removeSelector: #autoSelectString!
!methodRemoval: TextProvider #autoSelectString stamp: 'jmv 4/3/2023 09:32:39'!
TextProvider removeSelector: #autoSelectString!
!methodRemoval: TextModel #autoSelectString stamp: 'jmv 4/3/2023 09:32:35'!
TextModel removeSelector: #autoSelectString!
!classDefinition: #MessageSet category: #'Tools-Browser'!
Browser subclass: #MessageSet
	instanceVariableNames: 'messageList autoSelectString shouldAutoHighlight'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!
