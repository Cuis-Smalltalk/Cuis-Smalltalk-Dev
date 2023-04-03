'From Cuis 6.0 [latest update: #5750] on 3 April 2023 at 4:15:05 pm'!

!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 4/3/2023 16:12:22'!
acceptedStringOrText
	"return the source code that shows in the bottom pane"

	| sel class changeType answer |
	currentClassName ifNil: [^ myChangeSet preambleString ifNil: ['']].
	class := self selectedClassOrMetaClass.
	(sel := currentSelector)
		ifNotNil: [
			changeType := (myChangeSet atSelector: (sel := sel asSymbol) class: class).
			changeType == #remove
				ifTrue: [^'Method has been removed (see versions)'].
			changeType == #addedThenRemoved
				ifTrue: [^'Added then removed (see versions)'].
			changeType == #movedToOtherPackage
				ifTrue: [^'Method was moved to some other package'].
			class ifNil: [^'Method was added, but cannot be found!!'].
			(class includesSelector: sel)
				ifFalse: [^'Method was added, but cannot be found!!'].
			answer :=  (class sourceCodeAt: sel).
			(#(prettyPrint prettyLineDiffs prettyWordDiffs) includes: self contentsSymbol) ifTrue: [
				answer := (class compilerClass new
						format: answer
						in: class 
						notifying: nil)].
			self showingAnyKindOfDiffs
				ifTrue: [ answer := (self diffFromPriorSourceFor: answer) ].
			^ answer ]
		ifNil: [
			^ String streamContents: [ :strm |
				(myChangeSet classChangeAt: currentClassName) do: [ :each |
					each == #remove ifTrue: [strm nextPutAll: 'Entire class was removed.'; newLine].
					each == #addedThenRemoved ifTrue: [strm nextPutAll: 'Class was added then removed.'; newLine].
					each == #rename ifTrue: [strm nextPutAll: 'Class name was changed.'; newLine].
					each == #add ifTrue: [strm nextPutAll: 'Class definition was added.'; newLine].
					each == #change ifTrue: [strm nextPutAll: 'Class definition was changed.'; newLine].
					each == #reorganize ifTrue: [strm nextPutAll: 'Class organization was changed.'; newLine].
					each == #comment ifTrue: [strm nextPutAll: 'New class comment.'; newLine].
					each == #movedToOtherPackage ifTrue: [strm nextPutAll: 'Class was moved to some other package.'; newLine].
				]]].! !

!methodRemoval: SHTextStylerST80 class #unConvert: stamp: 'jmv 4/3/2023 16:13:06'!
SHTextStylerST80 class removeSelector: #unConvert:!
!methodRemoval: PluggableTextModel #isDirty stamp: 'jmv 4/3/2023 16:12:41'!
PluggableTextModel removeSelector: #isDirty!
