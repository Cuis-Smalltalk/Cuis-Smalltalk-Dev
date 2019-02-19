'From Cuis 5.0 of 7 November 2016 [latest update: #3612] on 19 February 2019 at 11:50:01 am'!

!SHParserST80 methodsFor: 'aux' stamp: 'jmv 2/19/2019 11:49:22'!
namesBeginningWith: aString do: aBlock in: aCollection
	"aCollection is sorted"
	"
	self new namesBeginningWith: 'O' do: [ :each | each print ] in: Smalltalk classNames
	self new namesBeginningWith: 'ObjectExplorer' do: [ :each | each print ] in: Smalltalk classNames
	self new namesBeginningWith: 'ObjectExplorerWrapper' do: [ :each | each print ] in: Smalltalk classNames
	"
	| count |

	"Find the first element  starting with aString"
	count _ aCollection size.
	aCollection
		findBinaryIndex: [ :element |
			element < aString
				ifFalse: [ -1 ]
				ifTrue: [ 1 ] ]
		do: [ :found | "Will never find any" ]
		ifNone: [ :a :b | | i n |
			i _ b.
			[ i <= count and: [ 
				n _ aCollection at: i.
				aString isEmpty or: [
					n beginsWith: aString ]]] whileTrue: [
				aBlock value: n.
				i _ i + 1 ]]! !


!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/19/2019 11:48:10'!
namesBeginningWith: aString do: aBlock
	"Evaluate aBlock for all available names that start with aString"

	self nonGlobalNamesDo: [ :name |
		(name beginsWith: aString) ifTrue: [ aBlock value: name ]].
	self namesBeginningWith: aString do: aBlock in: Smalltalk classNames.
	self namesBeginningWith: aString do: aBlock in: Smalltalk nonClassNames
	
! !

!methodRemoval: SystemDictionary #namesBeginningWith:do:!
SystemDictionary removeSelector: #namesBeginningWith:do:!
!methodRemoval: SystemDictionary #namesBeginningWith:do:in:!
SystemDictionary removeSelector: #namesBeginningWith:do:in:!
