'From Cuis7.9 [latest update: #8113] on 30 July 2026 at 2:14:56 pm'!
'Description Initial version'!
!provides: 'StreamExtensionsTests' 1 0!
!requires: 'StreamExtensions' 1 0 nil!
SystemOrganization addCategory: 'StreamExtensionsTests'!


!classDefinition: #TranscriptExtensionsTest category: 'StreamExtensionsTests'!
TestCase subclass: #TranscriptExtensionsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StreamExtensionsTests'!
!classDefinition: 'TranscriptExtensionsTest class' category: 'StreamExtensionsTests'!
TranscriptExtensionsTest class
	instanceVariableNames: ''!

!classDefinition: #WriteStreamExtensionsTest category: 'StreamExtensionsTests'!
TestCase subclass: #WriteStreamExtensionsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StreamExtensionsTests'!
!classDefinition: 'WriteStreamExtensionsTest class' category: 'StreamExtensionsTests'!
WriteStreamExtensionsTest class
	instanceVariableNames: ''!


!TranscriptExtensionsTest commentStamp: '<historical>' prior: 0!
Tests cases for Stream messages: 

nextPutAll: when:
nextPut: when: 
print: when:.

Made at Cuis Sprint #1
!

!WriteStreamExtensionsTest commentStamp: '<historical>' prior: 0!
Tests cases for Stream messages: 

nextPutAll: when:
nextPut: when: 
print: when:.

Made at Cuis Sprint #1
!

!TranscriptExtensionsTest methodsFor: 'assertions' stamp: 'jmv 1/Mar/2025, 8:19:12 pm (UTC)'!
assertTranscriptContentsAdded: expectedAddedString after: aBlock
	
	| transcriptContentsBeforeBlockEvaluation |
	"Transcript clear."
	transcriptContentsBeforeBlockEvaluation := Transcript contents.
	aBlock value.
	self assert: Transcript contents equals: transcriptContentsBeforeBlockEvaluation, expectedAddedString.! !

!TranscriptExtensionsTest methodsFor: 'assertions' stamp: 'jmv 1/Mar/2025, 8:19:06 pm (UTC)'!
assertTranscriptContentsAdded2: expectedAddedString after: aBlock
	
	| transcriptContentsBeforeBlockEvaluation |
	"Transcript clear."
	transcriptContentsBeforeBlockEvaluation := Transcript contents.
	aBlock value.
	{transcriptContentsBeforeBlockEvaluation. expectedAddedString.Transcript contents} inspect.
	self assert: Transcript contents equals: transcriptContentsBeforeBlockEvaluation, expectedAddedString.! !

!TranscriptExtensionsTest methodsFor: 'assertions' stamp: 'HAW 20/Mar/2019, 10:15:48 am (UTC)'!
assertTranscriptContentsDoesNotChangeAfter: aBlock
	
	self assertTranscriptContentsAdded: '' after: aBlock! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
testNewLineWhenConditionIsFalseDoesNotPutANewLineInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript newLineIf: false ]
	! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
testNewLineWhenConditionIsTruePutsANewLineInStream

	self assertTranscriptContentsAdded: String newLineString, '.' after: [ Transcript newLineIf: true; nextPut: $. ]! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
testNextPutAllWhenConditionIsFalseDoesNotPutCollectionInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript nextPutAll: 'other' if: false ]
	! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
testNextPutAllWhenConditionIsTruePutsCollectionInStream

	self assertTranscriptContentsAdded: 'other' after: [ Transcript nextPutAll: 'other' if: true ]
	
	! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
testNextPutWhenConditionIsFalseDoesNotPutObjectInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript nextPut: $t if: false ]
! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
testNextPutWhenConditionIsTruePutsObjectInStream

	self assertTranscriptContentsAdded: 't' after: [ Transcript nextPut: $t if: true ]
	
	! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
testPrintWhenConditionIsFalseDoesNotPrintObjectInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript print: Object if: false ]
! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
testPrintWhenConditionIsTruePrintsObjectInStream

	self assertTranscriptContentsAdded: Object printString after: [ Transcript print: Object if: true ]
	! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
testSpaceWhenConditionIsFalseDoesNotPutASpaceInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript spaceIf: false ]
	! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
testSpaceWhenConditionIsTruePutsASpaceInStream

	self assertTranscriptContentsAdded: ' ' after: [ Transcript spaceIf: true ]
	! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
testTabWhenConditionIsFalseDoesNotPutATabInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript tabIf: false ]! !

!TranscriptExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
testTabWhenConditionIsTruePutsATabInStream

	self assertTranscriptContentsAdded: Character tab asString after: [ Transcript tabIf: true ]! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:09 pm (UTC)'!
testNewLineTabWhenConditionIsFalseDoesNotPutANewLineTabInStream

	| stream |

	stream := String writeStream.
	stream newLineTab: 2 if: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:09 pm (UTC)'!
testNewLineTabWhenConditionIsTruePutsANewLineTabInStream

	| stream |

	stream := String writeStream.
	stream newLineTab: 2 if: true.
	
	self assert: stream contents equals: (String streamContents: [ :s | s newLineTab: 2 ]).! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
testNewLineWhenConditionIsFalseDoesNotPutANewLineInStream

	| stream |

	stream := String writeStream.
	stream newLineIf: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
testNewLineWhenConditionIsTruePutsANewLineInStream

	| stream |

	stream := String writeStream.
	stream newLineIf: true.
	
	self assert: stream contents equals: Character newLineCharacter asString.! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
testNextPutAllWhenConditionIsFalseDoesNotPutCollectionInStream

	| stream |

	stream := String writeStream.
	stream nextPutAll: 'other' if: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
testNextPutAllWhenConditionIsTruePutsCollectionInStream

	| stream |
	
	stream := String writeStream.
	stream nextPutAll: 'other' if: true.
	
	self assert: stream contents equals: 'other'.! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
testNextPutWhenConditionIsFalseDoesNotPutObjectInStream

	| stream |
	
	stream := String writeStream.
	stream nextPut: $t if: false.
	
	self assert: stream contents isEmpty
	
	! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
testNextPutWhenConditionIsTruePutsObjectInStream

	| stream |
	
	stream := String writeStream.
	stream nextPut: $t if: true.
	
	self assert: stream contents equals: 't'.! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
testPrintWhenConditionIsFalseDoesNotPrintObjectInStream

	| stream |
	
	stream := String writeStream.
	stream print: Object if: false.
	
	self assert: stream contents isEmpty
	
	! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
testPrintWhenConditionIsTruePrintsObjectInStream

	| stream |
	
	stream := String writeStream.
	stream print: Object if: true.
	
	self assert: stream contents equals: Object printString.! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:15 pm (UTC)'!
testSpaceManyTimesWhenConditionIsFalseDoesNotPutASpaceInStream

	| stream |

	stream := String writeStream.
	stream space: 2 if: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:15 pm (UTC)'!
testSpaceManyTimesWhenConditionIsTruePutSpacesInStream

	| stream |

	stream := String writeStream.
	stream space: 2 if: true.
	
	self assert: stream contents equals: '  '.! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
testSpaceWhenConditionIsFalseDoesNotPutASpaceInStream

	| stream |

	stream := String writeStream.
	stream spaceIf: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
testSpaceWhenConditionIsTruePutsASpaceInStream

	| stream |

	stream := String writeStream.
	stream spaceIf: true.
	
	self assert: stream contents equals: ' '.! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:36 pm (UTC)'!
testTabManyTimesWhenConditionIsFalseDoesNotPutATabInStream

	| stream |

	stream := String writeStream.
	stream tab: 2 if: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:36 pm (UTC)'!
testTabManyTimesWhenConditionIsTruePutsTabsInStream

	| stream |

	stream := String writeStream.
	stream tab: 2 if: true.
	
	self assert: stream contents equals: (String streamContents: [ :s | s tab: 2 ]).! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
testTabWhenConditionIsFalseDoesNotPutATabInStream

	| stream |

	stream := String writeStream.
	stream tabIf: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtensionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
testTabWhenConditionIsTruePutsATabInStream

	| stream |

	stream := String writeStream.
	stream tabIf: true.
	
	self assert: stream contents equals: Character tab asString.! !
