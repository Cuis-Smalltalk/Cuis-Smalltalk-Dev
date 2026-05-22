Class {
	#name : 'TimeTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Chronology',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Chronology'
}

{ #category : 'tests' }
TimeTest >> testCreation [
	"
	self new testCreation
	"
	| t |
	
	t _ Time seconds: 52262.
	self assert: t hour = 14.
	self assert: t minute = 31.
	self assert: t second = 2.

	t _ Time seconds: 52262 nanoSeconds: 414712000.
	self assert: t hour = 14.
	self assert: t minute = 31.
	self assert: t second = 2.
	self assert: t nanoSecond = 414712000
]

{ #category : 'tests' }
TimeTest >> testEqual [
	"
	self new testEqual
	"
	| t1 t2 |
	
	t1 _ Time seconds: 52262.
	t2 _ Time seconds: 52262 nanoSeconds: 0.
	self assert: t1 = t2.
	t2 _ Time seconds: 52262 nanoSeconds: 1234.
	self deny: t1 = t2.
	t2 _ Time seconds: 52263 nanoSeconds: 0.
	self deny: t1 = t2.
]

{ #category : 'Tests' }
TimeTest >> testSqueakInquiries [
	| timewords |

	timewords := #(0.5 30 62 130 4000 10000 60000 86401) 
		collect: [ :ss | Time humanWordsForSecondsAgo: ss ].
	self assert: 
		timewords = #('a second ago' '30 seconds ago' 'a minute ago' '2 minutes ago' 
			'an hour ago' '2 hours ago' '16 hours ago' 'yesterday').
]

{ #category : 'tests' }
TimeTest >> testTimePartsArePositive [
	self assert: (Time afterMidnight: -10 minutes) minute > 0.
	self assert: (Time afterMidnight: -10 minutes) hour = 23.
	self assert: (Time afterMidnight: -10 minutes) minute = 50.
]
