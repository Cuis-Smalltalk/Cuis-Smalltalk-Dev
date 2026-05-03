'From Cuis7.7 [latest update: #7901] on 3 May 2026 at 7:04:12 pm'!

!CodeFile methodsFor: 'private' stamp: 'jmv 3/May/2026 19:03:40'!
classRemoval: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	tokens size = 3 ifFalse:[^doIts add: chgRec].
	theClass := self getClass: (tokens at: 3).
	theClass definition: string.
	classOrder add: theClass.! !


!CodeFile methodsFor: 'change record types' stamp: 'jmv 3/May/2026 19:03:44'!
classRemoval: aClassDeletionChange
	^self classRemoval: aClassDeletionChange string with: aClassDeletionChange! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/May/2026 18:57:43'!
fileIn
	"FileIn the receiver"
	self hasDefinition ifTrue:[self fileInDefinition].
	self exists ifTrue: [
		self fileInMethods: self selectors.
		metaClass ifNotNil:[metaClass fileIn].
		self needsInitialize ifTrue:[
			self evaluate: self name,' initialize' ]].! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/May/2026 18:59:37'!
fileInDefinition
	(self makeSureSuperClassExists: self superclassName) ifFalse:[^self].
	self hasDefinition ifTrue:[
		Transcript newLine; show: 'Applying definition/removal of ', self name.
		self evaluate: self definition].
	self exists ifFalse:[^self].
	self hasComment ifTrue:[self realClass classComment: self comment].! !

