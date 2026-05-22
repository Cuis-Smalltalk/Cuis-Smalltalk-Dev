Class {
	#name : 'MessageTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Methods',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Methods'
}

{ #category : 'assertions' }
MessageTest >> assertFullNameOf: aSelector arguments: arguments is: expectedFullName   [

	^ self assert: expectedFullName equals: (Message selector: aSelector arguments: arguments ) fullName
]

{ #category : 'assertions' }
MessageTest >> assertKeywordsOf: aSelector arguments: arguments is: expectedKeyword [

	^ self assert: expectedKeyword equals: (Message selector: aSelector arguments: arguments ) keywords
]

{ #category : 'tests' }
MessageTest >> test01FullName [

	self assertFullNameOf: #printString arguments: { } is: 'printString'. 
	self assertFullNameOf: #+ arguments: { 'a' } is: '+ a'. 
	self assertFullNameOf: #selector: arguments: { 'a' } is: 'selector: a'. 
	self assertFullNameOf: #selector:argument: arguments: { 'a'. 'b'} is: 'selector: a argument: b'. 
]

{ #category : 'tests' }
MessageTest >> test02Keywords [

	self assertKeywordsOf: #printString arguments: {} is: #(printString).
	self assertKeywordsOf: #+ arguments: {'a'} is: #(+).
	self assertKeywordsOf: #selector: arguments: {#printString} is: #(selector:).
	self assertKeywordsOf: #selector:arguments: arguments: {#printString:. 'b'} is: #(selector: arguments:).
	
]

{ #category : 'tests' }
MessageTest >> test03HasNoRightNumberOfArgumentsForUnaryMessageAndNotEmptyArguments [

	self deny: (Message selector: #printString argument: 'a') hasRightNumberOfArguments
]

{ #category : 'tests' }
MessageTest >> test04HasRightNumberOfArgumentsForUnaryMessageAndEmptyArguments [

	self assert: (Message selector: #printString) hasRightNumberOfArguments
]

{ #category : 'tests' }
MessageTest >> test05HasNoRightNumberOfArgumentsForBinaryMessageAndEmptyArguments [

	self deny: (Message selector: #+) hasRightNumberOfArguments
]

{ #category : 'tests' }
MessageTest >> test06HasNoRightNumberOfArgumentsForBinaryMessageAndMoreThanOneArgument [

	self deny: (Message selector: #+ arguments: { 'a'. 'b' }) hasRightNumberOfArguments
]

{ #category : 'tests' }
MessageTest >> test07HasRightNumberOfArgumentsForBinaryMessageAndOneArgument [

	self assert: (Message selector: #+ argument: 'a') hasRightNumberOfArguments
]

{ #category : 'tests' }
MessageTest >> test08HasNoRightNumberOfArgumentsForKeywordMessageAndLessArguments [

	self deny: (Message selector: #selector:) hasRightNumberOfArguments
]

{ #category : 'tests' }
MessageTest >> test09HasNoRightNumberOfArgumentsForKeywordMessageAndMoreArguments [

	self deny: (Message selector: #selector: arguments: { #printString. 'b' }) hasRightNumberOfArguments
]

{ #category : 'tests' }
MessageTest >> test10HasRightNumberOfArgumentsForKeywordMessageWhenSameNumberOfKeywordsAndArguments [

	self assert: (Message selector: #selector:argument: arguments: { #printString:. 'b' }) hasRightNumberOfArguments
]
