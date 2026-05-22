"
Tests cases for Stream messages: 

nextPutAll: when:
nextPut: when: 
print: when:.

Made at Cuis Sprint #1

"
Class {
	#name : 'WriteStreamTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Collections-Streams',
	#package : 'BaseImageTests',
	#tag : 'Collections-Streams'
}

{ #category : 'tests' }
WriteStreamTest >> testIsStream [
	self assert: (String writeStream is: #Stream).
]

{ #category : 'tests' }
WriteStreamTest >> testNewLineTabWhenConditionIsFalseDoesNotPutANewLineTabInStream [

	| stream |

	stream := String writeStream.
	stream newLineTab: 2 when: false.
	
	self assert: stream contents isEmpty
	
]

{ #category : 'tests' }
WriteStreamTest >> testNewLineTabWhenConditionIsTruePutsANewLineTabInStream [

	| stream |

	stream := String writeStream.
	stream newLineTab: 2 when: true.
	
	self assert: stream contents equals: (String streamContents: [ :s | s newLineTab: 2 ]).
]

{ #category : 'tests' }
WriteStreamTest >> testNewLineWhenConditionIsFalseDoesNotPutANewLineInStream [

	| stream |

	stream := String writeStream.
	stream newLineWhen: false.
	
	self assert: stream contents isEmpty
	
]

{ #category : 'tests' }
WriteStreamTest >> testNewLineWhenConditionIsTruePutsANewLineInStream [

	| stream |

	stream := String writeStream.
	stream newLineWhen: true.
	
	self assert: stream contents equals: Character newLineCharacter asString.
]

{ #category : 'tests' }
WriteStreamTest >> testNextPutAllWhenConditionIsFalseDoesNotPutCollectionInStream [

	| stream |

	stream := String writeStream.
	stream nextPutAll: 'other' when: false.
	
	self assert: stream contents isEmpty
	
]

{ #category : 'tests' }
WriteStreamTest >> testNextPutAllWhenConditionIsTruePutsCollectionInStream [

	| stream |
	
	stream := String writeStream.
	stream nextPutAll: 'other' when: true.
	
	self assert: stream contents equals: 'other'.
]

{ #category : 'tests' }
WriteStreamTest >> testNextPutWhenConditionIsFalseDoesNotPutObjectInStream [

	| stream |
	
	stream := String writeStream.
	stream nextPut: $t when: false.
	
	self assert: stream contents isEmpty
	
	
]

{ #category : 'tests' }
WriteStreamTest >> testNextPutWhenConditionIsTruePutsObjectInStream [

	| stream |
	
	stream := String writeStream.
	stream nextPut: $t when: true.
	
	self assert: stream contents equals: 't'.
]

{ #category : 'tests' }
WriteStreamTest >> testPrintWhenConditionIsFalseDoesNotPrintObjectInStream [

	| stream |
	
	stream := String writeStream.
	stream print: Object when: false.
	
	self assert: stream contents isEmpty
	
	
]

{ #category : 'tests' }
WriteStreamTest >> testPrintWhenConditionIsTruePrintsObjectInStream [

	| stream |
	
	stream := String writeStream.
	stream print: Object when: true.
	
	self assert: stream contents equals: Object printString.
]

{ #category : 'tests' }
WriteStreamTest >> testSpaceManyTimesWhenConditionIsFalseDoesNotPutASpaceInStream [

	| stream |

	stream := String writeStream.
	stream space: 2 when: false.
	
	self assert: stream contents isEmpty
	
]

{ #category : 'tests' }
WriteStreamTest >> testSpaceManyTimesWhenConditionIsTruePutSpacesInStream [

	| stream |

	stream := String writeStream.
	stream space: 2 when: true.
	
	self assert: stream contents equals: '  '.
]

{ #category : 'tests' }
WriteStreamTest >> testSpaceWhenConditionIsFalseDoesNotPutASpaceInStream [

	| stream |

	stream := String writeStream.
	stream spaceWhen: false.
	
	self assert: stream contents isEmpty
	
]

{ #category : 'tests' }
WriteStreamTest >> testSpaceWhenConditionIsTruePutsASpaceInStream [

	| stream |

	stream := String writeStream.
	stream spaceWhen: true.
	
	self assert: stream contents equals: ' '.
]

{ #category : 'tests' }
WriteStreamTest >> testTabManyTimesWhenConditionIsFalseDoesNotPutATabInStream [

	| stream |

	stream := String writeStream.
	stream tab: 2 when: false.
	
	self assert: stream contents isEmpty
	
]

{ #category : 'tests' }
WriteStreamTest >> testTabManyTimesWhenConditionIsTruePutsTabsInStream [

	| stream |

	stream := String writeStream.
	stream tab: 2 when: true.
	
	self assert: stream contents equals: (String streamContents: [ :s | s tab: 2 ]).
]

{ #category : 'tests' }
WriteStreamTest >> testTabWhenConditionIsFalseDoesNotPutATabInStream [

	| stream |

	stream := String writeStream.
	stream tabWhen: false.
	
	self assert: stream contents isEmpty
	
]

{ #category : 'tests' }
WriteStreamTest >> testTabWhenConditionIsTruePutsATabInStream [

	| stream |

	stream := String writeStream.
	stream tabWhen: true.
	
	self assert: stream contents equals: Character tab asString.
]
