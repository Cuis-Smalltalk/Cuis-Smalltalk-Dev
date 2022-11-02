'From Cuis 6.0 [latest update: #5546] on 2 November 2022 at 2:57:36 pm'!

!Debugger methodsFor: 'shout styling' stamp: 'jmv 11/2/2022 14:52:47'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [^false].
	anSHTextStyler 
		classOrMetaClass: self selectedClassOrMetaClass;
		"disableFormatAndConvert;"
		workspace: self.
	^true! !

