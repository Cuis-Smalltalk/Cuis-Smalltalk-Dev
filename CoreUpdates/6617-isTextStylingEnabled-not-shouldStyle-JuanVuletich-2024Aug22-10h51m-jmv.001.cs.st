'From Cuis7.1 [latest update: #6615] on 22 August 2024 at 10:56:30 am'!

!Workspace methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 10:55:41'!
isTextStylingEnabled

	^shouldStyle ifNil: [ Preferences at: #shoutInWorkspaces]! !


!Workspace methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 10:55:51'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text."

	self isTextStylingEnabled ifFalse: [ ^false ].
	anSHTextStyler 
		classOrMetaClass: nil;
		workspace: self.
	^true! !

!Workspace methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 10:55:55'!
toggleStyling

	shouldStyle := self isTextStylingEnabled not.
	actualContents := actualContents asPlainString asText.
	self changed: #actualContents! !

!Workspace methodsFor: 'shout styling' stamp: 'jmv 8/22/2024 10:56:00'!
toggleStylingLabel

	^self isTextStylingEnabled 
		ifTrue: [ '<on> syntax highlighting' ]
		ifFalse: [ '<off> syntax highlighting' ]! !


!TranscriptMorph methodsFor: 'menu commands' stamp: 'jmv 8/22/2024 10:56:03'!
editContents
	workspace := Workspace new.
	workspace isTextStylingEnabled ifTrue: [ workspace toggleStyling ].
	workspace contents: Transcript contents.
	lastUnfinishedEntrySize := Transcript unfinishedEntrySize.
	workspaceWindow := workspace openLabel: 'Workspace - Transcript contents'.
	lastIncludedIndex := Transcript lastIndex.! !

!methodRemoval: Workspace #shouldStyle stamp: 'jmv 8/22/2024 10:56:10'!
Workspace removeSelector: #shouldStyle!
