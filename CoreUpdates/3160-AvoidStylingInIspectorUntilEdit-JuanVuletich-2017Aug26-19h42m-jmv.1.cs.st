'From Cuis 5.0 of 7 November 2016 [latest update: #3154] on 26 August 2017 at 7:48:09 pm'!

!TextModel methodsFor: 'user interface support' stamp: 'jmv 8/26/2017 19:19:29'!
formatAndStyleIfNeededWith: anSHTextStyler
	anSHTextStyler ifNotNil: [
		(self shouldStyle: self actualContents with: anSHTextStyler) ifTrue: [
			anSHTextStyler formatAndStyle: self actualContents allowBackgroundStyleProcess: true.
			self basicActualContents: anSHTextStyler formattedText ]]! !


!PluggableTextModel methodsFor: 'shout styling' stamp: 'jmv 8/26/2017 19:21:51'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	^textProvider shouldStyle: text with: anSHTextStyler! !


!Workspace methodsFor: 'shout styling' stamp: 'jmv 8/26/2017 19:21:53'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text."

	self shouldStyle ifFalse: [ ^false ].
	anSHTextStyler 
		classOrMetaClass: nil;
		workspace: self.
	^true! !


!CodeProvider methodsFor: 'shout styling' stamp: 'jmv 8/26/2017 19:21:32'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Answer false if showing difs, to veto the styling."
	
	^self showingAnyKindOfDiffs not! !


!Browser methodsFor: 'shout styling' stamp: 'jmv 8/26/2017 19:21:20'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	| type |
	
	self isModeStyleable ifFalse: [^false].
	type _ self editSelection.
	(#(newMessage editMessage editClass newClass) includes: type) ifFalse:[^false].
	anSHTextStyler classOrMetaClass: (type = #editClass ifFalse:[self selectedClassOrMetaClass]).
	^true! !


!MessageSet methodsFor: 'shout styling' stamp: 'jmv 8/26/2017 19:21:44'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [^false].
	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.
	^true! !


!CodeFileBrowser methodsFor: 'shout styling' stamp: 'jmv 8/26/2017 19:21:30'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.
	^true! !


!ChangeList methodsFor: 'shout styling' stamp: 'jmv 8/26/2017 19:21:23'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [^false].
	listIndex = 0 ifFalse: [
		(changeList at: listIndex) changeType = #method ifTrue: [
			self selectedClassOrMetaClass ifNotNil: [ :cl |
				anSHTextStyler classOrMetaClass: cl.
				^true ]]].
	^false! !


!ChangeSorter methodsFor: 'shout styling' stamp: 'jmv 8/26/2017 19:21:27'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	self currentSelector ifNil: [^false].
	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.
	^true! !


!Debugger methodsFor: 'shout styling' stamp: 'jmv 8/26/2017 19:21:36'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [^false].
	anSHTextStyler 
		classOrMetaClass: self selectedClassOrMetaClass;
		disableFormatAndConvert;
		workspace: self.
	^true! !


!Inspector methodsFor: 'shout styling' stamp: 'jmv 8/26/2017 19:42:36'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Answer true to allow styling to proceed, or false to veto the styling"

	anSHTextStyler workspace: self.
	^(text = self acceptedContents) not! !


!ObjectExplorer methodsFor: 'shout styling' stamp: 'jmv 8/26/2017 19:27:12'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Answer true to allow styling to proceed, or false to veto the styling"

	anSHTextStyler workspace: self.
	^true! !


!Inspector methodsFor: 'contents' stamp: 'jmv 8/26/2017 19:40:39'!
acceptedStringOrText
	"We need our cache not to be modified by user editions"
	^acceptedContentsCache copy! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 8/26/2017 19:07:20'!
hasUnacceptedEdits: aBoolean
	"Set the hasUnacceptedEdits flag to the given value. "
	aBoolean == hasUnacceptedEdits ifFalse: [
		hasUnacceptedEdits _ aBoolean.
		owner redrawNeeded].
	aBoolean ifFalse: [ hasEditingConflicts _ false].

	"shout:  re-style the text iff aBoolean is true
	Do not apply any formatting (i.e. changes to the characters in the text),
	just styling (i.e. TextAttributes)"
	aBoolean ifTrue: [
		self formatAndStyleIfNeeded ]! !

!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 8/26/2017 19:14:13'!
formatAndStyleIfNeeded
	"Apply both formatting (changes to the characters in the text, such as
	preferred assignment operators), and styling (TextAttributes to make
	Smalltalk code easier to understand)"

	model formatAndStyleIfNeededWith: styler! !

!methodRemoval: InnerTextMorph #okToStyle!
InnerTextMorph removeSelector: #okToStyle!
!methodRemoval: ObjectExplorer #shoutAboutToStyle:!
ObjectExplorer removeSelector: #shoutAboutToStyle:!
!methodRemoval: Inspector #shoutAboutToStyle:!
Inspector removeSelector: #shoutAboutToStyle:!
!methodRemoval: Debugger #shoutAboutToStyle:!
Debugger removeSelector: #shoutAboutToStyle:!
!methodRemoval: ChangeSorter #shoutAboutToStyle:!
ChangeSorter removeSelector: #shoutAboutToStyle:!
!methodRemoval: ChangeList #shoutAboutToStyle:!
ChangeList removeSelector: #shoutAboutToStyle:!
!methodRemoval: CodeFileBrowser #shoutAboutToStyle:!
CodeFileBrowser removeSelector: #shoutAboutToStyle:!
!methodRemoval: MessageSet #shoutAboutToStyle:!
MessageSet removeSelector: #shoutAboutToStyle:!
!methodRemoval: Browser #shoutAboutToStyle:!
Browser removeSelector: #shoutAboutToStyle:!
!methodRemoval: CodeProvider #shoutAboutToStyle:!
CodeProvider removeSelector: #shoutAboutToStyle:!
!methodRemoval: Workspace #shoutAboutToStyle:!
Workspace removeSelector: #shoutAboutToStyle:!
!methodRemoval: PluggableTextModel #shoutAboutToStyle:!
PluggableTextModel removeSelector: #shoutAboutToStyle:!
!methodRemoval: TextModel #formatAndStyleWith:!
TextModel removeSelector: #formatAndStyleWith:!
