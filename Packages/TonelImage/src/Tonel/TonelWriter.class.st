Class {
	#name : 'TonelWriter',
	#superclass : 'Object',
	#category : 'Tonel',
	#package : 'Tonel'
}

{ #category : 'writing' }
TonelWriter class >> methodCategoryOf: selector in: aClassOrMetaclass [
	^ (aClassOrMetaclass organization categoryOfElement: selector) ifNil: [ #'as yet unclassified' ]
]

{ #category : 'writing' }
TonelWriter class >> packageNameForCategory: aCategoryName [
	| category dashIndex |
	category := aCategoryName asString.
	dashIndex := category findString: '-' startingAt: 1.
	dashIndex = 0 ifTrue: [ ^ category ].
	^ category copyFrom: 1 to: dashIndex - 1
]

{ #category : 'writing' }
TonelWriter class >> sourceForClass: aClass [
	^ String streamContents: [ :stream | self writeClass: aClass on: stream ]
]

{ #category : 'writing' }
TonelWriter class >> stonKey: key value: value on: stream last: isLast [
	stream
		tab; nextPut: $#; nextPutAll: key; nextPutAll: ' : ';
		nextPut: $'; nextPutAll: (self stonString: value); nextPut: $'.
	isLast ifFalse: [ stream nextPut: $, ].
	stream newLine
]

{ #category : 'writing' }
TonelWriter class >> stonListKey: key values: values on: stream [
	values isEmpty ifTrue: [ ^ self ].
	stream tab; nextPut: $#; nextPutAll: key; nextPutAll: ' : ['; newLine.
	values do: [ :each |
		stream tab; tab; nextPut: $'; nextPutAll: (self stonString: each); nextPut: $'.
		each == values last ifFalse: [ stream nextPut: $, ].
		stream newLine ].
	stream tab; nextPutAll: '],'; newLine
]

{ #category : 'writing' }
TonelWriter class >> stonString: aString [
	^ aString asString copyReplaceAll: '''' with: ''''''
]

{ #category : 'writing' }
TonelWriter class >> writeClass: aClass on: stream [
	| comment category package tag classVars pools |
	comment := aClass organization classComment.
	comment isEmptyOrNil ifFalse: [
		stream nextPut: $"; newLine; nextPutAll: comment; newLine; nextPut: $"; newLine ].
	category := aClass category asString.
	package := self packageNameForCategory: category.
	tag := category = package ifTrue: [ nil ] ifFalse: [ category copyFrom: package size + 2 to: category size ].
	stream nextPutAll: 'Class {'; newLine.
	self stonKey: 'name' value: aClass name on: stream last: false.
	self stonKey: 'superclass' value: (aClass superclass ifNil: [ nil ] ifNotNil: [ :parentClass | parentClass name ]) on: stream last: false.
	self stonListKey: 'instVars' values: aClass instVarNames on: stream.
	classVars := aClass classPool keys asArray sort.
	self stonListKey: 'classVars' values: classVars on: stream.
	pools := (aClass sharedPools collect: [ :pool | Smalltalk keyAtIdentityValue: pool ]) asArray sort.
	self stonListKey: 'pools' values: pools on: stream.
	self stonListKey: 'classInstVars' values: aClass class instVarNames on: stream.
	self stonKey: 'category' value: category on: stream last: false.
	self stonKey: 'package' value: package on: stream last: tag isNil.
	tag ifNotNil: [ self stonKey: 'tag' value: tag on: stream last: true ].
	stream nextPutAll: '}'; newLine.
	self writeMethodsOf: aClass class classSide: true className: aClass name on: stream.
	self writeMethodsOf: aClass classSide: false className: aClass name on: stream
]

{ #category : 'writing' }
TonelWriter class >> writeMethodsOf: aClassOrMetaclass classSide: classSide className: className on: stream [
	| selectors |
	selectors := aClassOrMetaclass selectors asArray sort.
	selectors do: [ :selector |
		| source category sourceStream declarationLine body |
		source := aClassOrMetaclass sourceCodeAt: selector.
		sourceStream := source readStream.
		declarationLine := sourceStream nextLine.
		body := sourceStream upToEnd.
		category := self methodCategoryOf: selector in: aClassOrMetaclass.
		stream newLine.
		stream nextPutAll: '{ #category : '''; nextPutAll: (self stonString: category); nextPutAll: ''' }'; newLine.
		stream nextPutAll: className.
		classSide ifTrue: [ stream nextPutAll: ' class' ].
		stream nextPutAll: ' >> '; nextPutAll: declarationLine; nextPutAll: ' ['.
		stream newLine; nextPutAll: body; newLine; nextPutAll: ']'; newLine ]
]
