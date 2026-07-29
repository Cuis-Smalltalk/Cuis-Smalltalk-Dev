'From Cuis7.9 [latest update: #8111] on 29 July 2026 at 7:22:54 pm'!
'Description Initial version'!
!provides: 'StreamExtentionsTests' 1 0!
!requires: 'StreamExtentions' 1 1 nil!
SystemOrganization addCategory: 'StreamExtentionsTests'!


!classDefinition: #TranscriptExtentionsTest category: 'StreamExtentionsTests'!
TestCase subclass: #TranscriptExtentionsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StreamExtentionsTests'!
!classDefinition: 'TranscriptExtentionsTest class' category: 'StreamExtentionsTests'!
TranscriptExtentionsTest class
	instanceVariableNames: ''!

!classDefinition: #WriteStreamExtentionsTest category: 'StreamExtentionsTests'!
TestCase subclass: #WriteStreamExtentionsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'StreamExtentionsTests'!
!classDefinition: 'WriteStreamExtentionsTest class' category: 'StreamExtentionsTests'!
WriteStreamExtentionsTest class
	instanceVariableNames: ''!


!TranscriptExtentionsTest commentStamp: '<historical>' prior: 0!
Tests cases for Stream messages: 

nextPutAll: when:
nextPut: when: 
print: when:.

Made at Cuis Sprint #1
!

!WriteStreamExtentionsTest commentStamp: '<historical>' prior: 0!
Tests cases for Stream messages: 

nextPutAll: when:
nextPut: when: 
print: when:.

Made at Cuis Sprint #1
!

!TranscriptExtentionsTest methodsFor: 'assertions' stamp: 'jmv 1/Mar/2025, 8:19:12 pm (UTC)'!
assertTranscriptContentsAdded: expectedAddedString after: aBlock
	
	| transcriptContentsBeforeBlockEvaluation |
	"Transcript clear."
	transcriptContentsBeforeBlockEvaluation := Transcript contents.
	aBlock value.
	self assert: Transcript contents equals: transcriptContentsBeforeBlockEvaluation, expectedAddedString.! !

!TranscriptExtentionsTest methodsFor: 'assertions' stamp: 'jmv 1/Mar/2025, 8:19:06 pm (UTC)'!
assertTranscriptContentsAdded2: expectedAddedString after: aBlock
	
	| transcriptContentsBeforeBlockEvaluation |
	"Transcript clear."
	transcriptContentsBeforeBlockEvaluation := Transcript contents.
	aBlock value.
	{transcriptContentsBeforeBlockEvaluation. expectedAddedString.Transcript contents} inspect.
	self assert: Transcript contents equals: transcriptContentsBeforeBlockEvaluation, expectedAddedString.! !

!TranscriptExtentionsTest methodsFor: 'assertions' stamp: 'HAW 20/Mar/2019, 10:15:48 am (UTC)'!
assertTranscriptContentsDoesNotChangeAfter: aBlock
	
	self assertTranscriptContentsAdded: '' after: aBlock! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
testNewLineWhenConditionIsFalseDoesNotPutANewLineInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript newLineIf: false ]
	! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
testNewLineWhenConditionIsTruePutsANewLineInStream

	self assertTranscriptContentsAdded: String newLineString, '.' after: [ Transcript newLineIf: true; nextPut: $. ]! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
testNextPutAllWhenConditionIsFalseDoesNotPutCollectionInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript nextPutAll: 'other' if: false ]
	! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
testNextPutAllWhenConditionIsTruePutsCollectionInStream

	self assertTranscriptContentsAdded: 'other' after: [ Transcript nextPutAll: 'other' if: true ]
	
	! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
testNextPutWhenConditionIsFalseDoesNotPutObjectInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript nextPut: $t if: false ]
! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
testNextPutWhenConditionIsTruePutsObjectInStream

	self assertTranscriptContentsAdded: 't' after: [ Transcript nextPut: $t if: true ]
	
	! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
testPrintWhenConditionIsFalseDoesNotPrintObjectInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript print: Object if: false ]
! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
testPrintWhenConditionIsTruePrintsObjectInStream

	self assertTranscriptContentsAdded: Object printString after: [ Transcript print: Object if: true ]
	! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
testSpaceWhenConditionIsFalseDoesNotPutASpaceInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript spaceIf: false ]
	! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
testSpaceWhenConditionIsTruePutsASpaceInStream

	self assertTranscriptContentsAdded: ' ' after: [ Transcript spaceIf: true ]
	! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
testTabWhenConditionIsFalseDoesNotPutATabInStream

	self assertTranscriptContentsDoesNotChangeAfter: [ Transcript tabIf: false ]! !

!TranscriptExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
testTabWhenConditionIsTruePutsATabInStream

	self assertTranscriptContentsAdded: Character tab asString after: [ Transcript tabIf: true ]! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:09 pm (UTC)'!
testNewLineTabWhenConditionIsFalseDoesNotPutANewLineTabInStream

	| stream |

	stream := String writeStream.
	stream newLineTab: 2 if: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:09 pm (UTC)'!
testNewLineTabWhenConditionIsTruePutsANewLineTabInStream

	| stream |

	stream := String writeStream.
	stream newLineTab: 2 if: true.
	
	self assert: stream contents equals: (String streamContents: [ :s | s newLineTab: 2 ]).! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
testNewLineWhenConditionIsFalseDoesNotPutANewLineInStream

	| stream |

	stream := String writeStream.
	stream newLineIf: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:27 pm (UTC)'!
testNewLineWhenConditionIsTruePutsANewLineInStream

	| stream |

	stream := String writeStream.
	stream newLineIf: true.
	
	self assert: stream contents equals: Character newLineCharacter asString.! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
testNextPutAllWhenConditionIsFalseDoesNotPutCollectionInStream

	| stream |

	stream := String writeStream.
	stream nextPutAll: 'other' if: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:37 pm (UTC)'!
testNextPutAllWhenConditionIsTruePutsCollectionInStream

	| stream |
	
	stream := String writeStream.
	stream nextPutAll: 'other' if: true.
	
	self assert: stream contents equals: 'other'.! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
testNextPutWhenConditionIsFalseDoesNotPutObjectInStream

	| stream |
	
	stream := String writeStream.
	stream nextPut: $t if: false.
	
	self assert: stream contents isEmpty
	
	! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:48 pm (UTC)'!
testNextPutWhenConditionIsTruePutsObjectInStream

	| stream |
	
	stream := String writeStream.
	stream nextPut: $t if: true.
	
	self assert: stream contents equals: 't'.! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
testPrintWhenConditionIsFalseDoesNotPrintObjectInStream

	| stream |
	
	stream := String writeStream.
	stream print: Object if: false.
	
	self assert: stream contents isEmpty
	
	! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:16:58 pm (UTC)'!
testPrintWhenConditionIsTruePrintsObjectInStream

	| stream |
	
	stream := String writeStream.
	stream print: Object if: true.
	
	self assert: stream contents equals: Object printString.! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:15 pm (UTC)'!
testSpaceManyTimesWhenConditionIsFalseDoesNotPutASpaceInStream

	| stream |

	stream := String writeStream.
	stream space: 2 if: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:15 pm (UTC)'!
testSpaceManyTimesWhenConditionIsTruePutSpacesInStream

	| stream |

	stream := String writeStream.
	stream space: 2 if: true.
	
	self assert: stream contents equals: '  '.! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
testSpaceWhenConditionIsFalseDoesNotPutASpaceInStream

	| stream |

	stream := String writeStream.
	stream spaceIf: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:26 pm (UTC)'!
testSpaceWhenConditionIsTruePutsASpaceInStream

	| stream |

	stream := String writeStream.
	stream spaceIf: true.
	
	self assert: stream contents equals: ' '.! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:36 pm (UTC)'!
testTabManyTimesWhenConditionIsFalseDoesNotPutATabInStream

	| stream |

	stream := String writeStream.
	stream tab: 2 if: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:36 pm (UTC)'!
testTabManyTimesWhenConditionIsTruePutsTabsInStream

	| stream |

	stream := String writeStream.
	stream tab: 2 if: true.
	
	self assert: stream contents equals: (String streamContents: [ :s | s tab: 2 ]).! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
testTabWhenConditionIsFalseDoesNotPutATabInStream

	| stream |

	stream := String writeStream.
	stream tabIf: false.
	
	self assert: stream contents isEmpty
	! !

!WriteStreamExtentionsTest methodsFor: 'tests' stamp: 'HAW 29/Jul/2026, 10:17:45 pm (UTC)'!
testTabWhenConditionIsTruePutsATabInStream

	| stream |

	stream := String writeStream.
	stream tabIf: true.
	
	self assert: stream contents equals: Character tab asString.! !
