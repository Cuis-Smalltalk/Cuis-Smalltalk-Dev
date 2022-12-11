'From Cuis 6.0 [latest update: #5588] on 11 December 2022 at 7:01:08 pm'!

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 15:54:53'!
basicCompose: combiningCodePoints on: aStream
	| continue composition i base |
	combiningCodePoints notEmpty ifTrue: [
		continue _ combiningCodePoints size > 1.
		[continue] whileTrue: [
			continue _ false.
			base _ combiningCodePoints first.
			i _ 2.
			composition _ nil.
			[composition isNil and: [i <= combiningCodePoints size ]] whileTrue: [
				composition _ UnicodeCodePoint cannonicalCompositionOf: base and: (combiningCodePoints at: i).
				composition ifNotNil: [
					combiningCodePoints at: 1 put: composition.
					combiningCodePoints removeAt: i.
					continue _ combiningCodePoints size > 1 ].
				i _ i + 1 ]].
		combiningCodePoints do: [ :e | aStream nextPutCodePoint: e ]].! !

