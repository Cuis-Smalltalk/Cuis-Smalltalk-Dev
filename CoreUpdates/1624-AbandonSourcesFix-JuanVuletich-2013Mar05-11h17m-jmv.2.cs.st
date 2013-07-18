'From Cuis 4.1 of 12 December 2012 [latest update: #1623] on 5 March 2013 at 11:22:34 am'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 3/5/2013 11:05'!
destroySourcePointer
	self setSourcePointer: 0! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 3/5/2013 11:03'!
setSourcePointer: srcPointer
	srcPointer = 0 ifTrue: [
		0 to: 3 do: [ :i |
			self at: self size-i put: 0].
		^self].
	(srcPointer between: 16r1000000 and: 16r4FFFFFF) ifFalse: [self error: 'Source pointer out of range'].
	self at: self size put: (srcPointer bitShift: -24) + 251.
	1 to: 3 do: [:i |
		self at: self size-i put: ((srcPointer bitShift: (i-3)*8) bitAnd: 16rFF)]! !


!RemoteString methodsFor: 'accessing' stamp: 'jmv 3/5/2013 09:07'!
string 
	"Answer the receiver's string if remote files are enabled."
	| theFile |
	(sourceFileNumber == nil or: [(SourceFiles at: sourceFileNumber) == nil]) ifTrue: [^nil].
	theFile _ SourceFiles at: sourceFileNumber.
	theFile position: filePositionHi.
	^ theFile nextChunk! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 3/5/2013 11:06'!
abandonSources
	"
	Smalltalk abandonSources
	"
	 | m bTotal bCount |
	(self confirm:
'This method will detach the image fom source code.
A fresh changes file will be created to record further changes.
-- CAUTION --
If you have backed up your system and
are prepared to face the consequences of
abandoning source code files, choose Yes.
If you have any doubts, you may choose No
to back out with no harm done.')
		==  true ifFalse: [^ self inform: 'Okay - no harm done'].
	bTotal _ 0.  bCount _ 0.
	Smalltalk allBehaviorsDo: [: b | bTotal _ bTotal + 1].
'Doing #destroySourcePointer ...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: bTotal
	during: [ :bar |
		Smalltalk allBehaviorsDo: [ :cl |
		"for testing"
		"{ EllipseMorph } do: [ :cl |"
			bar value: (bCount _ bCount + 1).
			cl selectors do: [:selector |
				m _ cl compiledMethodAt: selector.
				m destroySourcePointer ]]].
	Smalltalk allBehaviorsDo: [:b | b zapOrganization].
	Smalltalk closeSourceFiles.
	Preferences disable: #warnIfNoChangesFile.
	Preferences disable: #warnIfNoSourcesFile! !

!methodRemoval: SystemDictionary #abandonTempNames!
SystemDictionary removeSelector: #abandonTempNames!
!methodRemoval: CompiledMethod #setMySourcePointer:!
CompiledMethod removeSelector: #setMySourcePointer:!
