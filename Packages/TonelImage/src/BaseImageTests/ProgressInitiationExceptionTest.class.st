Class {
	#name : 'ProgressInitiationExceptionTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Exceptions',
	#package : 'BaseImageTests',
	#tag : 'Exceptions'
}

{ #category : 'aux' }
ProgressInitiationExceptionTest >> sampleProgress: countStorage [
"
self new sampleProgress: {0}
"
	| d |
	d := Delay forMilliseconds: 10.
	^'Now here''s some Real Progress'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: 4
		during: [ :barBlock |
			1 to: 4 do: [ :x |
				countStorage at: 1 put: countStorage first + 1.
				barBlock value: x.
				d wait.
				x = 3 ifTrue: [1/0].	"just to make life interesting"
			].
			'done'
		].


]

{ #category : 'testing' }
ProgressInitiationExceptionTest >> test01 [
	| countStorage |
	countStorage := {0}.
	self
		should: [ self sampleProgress: countStorage ]
		raise: ZeroDivide.
	self assert: countStorage first = 3.
	
]

{ #category : 'testing' }
ProgressInitiationExceptionTest >> test02 [
	| countStorage |
	countStorage := {0}.
	self
		shouldnt: [
			[self sampleProgress: countStorage]
				on: ZeroDivide
				do: [ :ex | ex resume]
			]
		raise: Error.
	self assert: countStorage first = 4.
	
]

{ #category : 'testing' }
ProgressInitiationExceptionTest >> test03 [
	| countStorage |
	countStorage := {0}.
	self
		should: [
			[self sampleProgress: countStorage]
				on: ProgressInitiationException
				do: [ :ex | ex sendNotificationsTo: [ :min :max :curr | Transcript show: min printString, '  ', max printString, '  ', curr printString; newLine ]]
			]
		raise: Error.
	self assert: countStorage first = 3.
	
]

{ #category : 'testing' }
ProgressInitiationExceptionTest >> test04 [
	| countStorage |
	countStorage := {0}.
	self
		shouldnt: [
			[
				[self sampleProgress: countStorage]
				on: ZeroDivide
				do: [ :ex | ex resume]
			]
				on: ProgressInitiationException
				do: [ :ex | ex sendNotificationsTo: [ :min :max :curr | Transcript show: min printString, '  ', max printString, '  ', curr printString; newLine ]]
			]
		raise: Error.
	self assert: countStorage first = 4.
	
]

{ #category : 'testing' }
ProgressInitiationExceptionTest >> test05 [
	| countStorage |
	countStorage := {0}.
	self
		shouldnt: [
			[
				[self sampleProgress: countStorage]
				on: ProgressInitiationException
				do: [ :ex | ex sendNotificationsTo: [ :min :max :curr | Transcript show: min printString, '  ', max printString, '  ', curr printString; newLine ]]
			]
				on: ZeroDivide
				do: [ :ex | ex resume]
			]
		raise: Error.
	self assert: countStorage first = 4.
	
]
