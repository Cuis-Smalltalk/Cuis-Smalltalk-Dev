"
This test documents the source pointer address conversion methods for CuisSourceFileArray.

See class comment there. 

"
Class {
	#name : 'CuisSourceFileArrayTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-Files',
	#package : 'BaseImageTests',
	#tag : 'System-Files'
}

{ #category : 'testing' }
CuisSourceFileArrayTest >> testAddressRange [
	"Test source pointer to file position address translation across the full address range"
	
	| sf |
	sf := CuisSourceFileArray new.
	(16r1000000 to: 16r4FFFFFF by: 811) do: [:e | | i a p |
		i := sf fileIndexFromSourcePointer: e.
		p := sf filePositionFromSourcePointer: e.
		a := sf sourcePointerFromFileIndex: i andPosition: p.
		self assert: a = e]

]

{ #category : 'testing' }
CuisSourceFileArrayTest >> testChangesFileAddressRange [
	"Test file position to source pointer address translation for the changes file"
	
	| sf a e |
	sf := CuisSourceFileArray new.
	(0 to: 16r1FFFFFF by: 811) do: [:ee | | a2 i p |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		i := sf fileIndexFromSourcePointer: a.
		self assert: i = 2.
		p := sf filePositionFromSourcePointer: a.
		self assert: p = e.
		a2 := sf sourcePointerFromFileIndex: 2 andPosition: p.
		self assert: a2 = a].
	(0 to: 16rFFFFFF by: 811) do: [:ee |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r3000000 and: 16r3FFFFFF)].
	(16r1000000 to: 16r1FFFFFF by: 811) do: [:ee |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r3000000 and: 16r4FFFFFF)]



]

{ #category : 'testing' }
CuisSourceFileArrayTest >> testFileIndexFromSourcePointer [
	"Test derivation of file index for sources or changes file from source pointers"

	| sf |
	sf := CuisSourceFileArray new.
	"sources file mapping"
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r1000000).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r1000013).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r1FFFFFF).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r2000000).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r2000013).
	self assert: 1 = (sf fileIndexFromSourcePointer: 16r2FFFFFF).
	(16r1000000 to: 16r1FFFFFF by: 811) do: [:e | self assert: 1 = (sf fileIndexFromSourcePointer: e)].
	(16r2000000 to: 16r2FFFFFF by: 811) do: [:e | self assert: 1 = (sf fileIndexFromSourcePointer: e)].
	"changes file mapping"
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r3000000).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r3000013).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r3FFFFFF).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r4000000).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r4000013).
	self assert: 2 = (sf fileIndexFromSourcePointer: 16r4FFFFFF).
	(16r3000000 to: 16r3FFFFFF by: 811) do: [:e | self assert: 2 = (sf fileIndexFromSourcePointer: e)].
	(16r4000000 to: 16r4FFFFFF by: 811) do: [:e | self assert: 2 = (sf fileIndexFromSourcePointer: e)]



]

{ #category : 'testing' }
CuisSourceFileArrayTest >> testFilePositionFromSourcePointer [
	"Test derivation of file position for sources or changes file from source pointers"

	| sf |
	sf := CuisSourceFileArray new.
	"sources file"
	self assert: 0 = (sf filePositionFromSourcePointer: 16r1000000).
	"changes file"
	self assert: 0 = (sf filePositionFromSourcePointer: 16r3000000).
]

{ #category : 'testing' }
CuisSourceFileArrayTest >> testSourcePointerFromFileIndexAndPosition [
	"Test valid input ranges"

	| sf |
	sf := CuisSourceFileArray new.
	self should: [sf sourcePointerFromFileIndex: 0 andPosition: 0] raise: Error.
	self shouldnt: [sf sourcePointerFromFileIndex: 1 andPosition: 0] raise: Error.
	self shouldnt: [sf sourcePointerFromFileIndex: 2 andPosition: 0] raise: Error.
	self should: [sf sourcePointerFromFileIndex: 0 andPosition: 3] raise: Error.
	self should: [sf sourcePointerFromFileIndex: 1 andPosition: -1] raise: Error.
	self shouldnt: [sf sourcePointerFromFileIndex: 1 andPosition: 16r1FFFFFF] raise: Error.
	self shouldnt: [sf sourcePointerFromFileIndex: 1 andPosition: 16r2000000] raise: Error.
	self should: [sf sourcePointerFromFileIndex: 3 andPosition: 0] raise: Error.
	self should: [sf sourcePointerFromFileIndex: 4 andPosition: 0] raise: Error.
	
	self assert: 16r1000000 = (sf sourcePointerFromFileIndex: 1 andPosition: 0).
	self assert: 16r3000000 = (sf sourcePointerFromFileIndex: 2 andPosition: 0).
]

{ #category : 'testing' }
CuisSourceFileArrayTest >> testSourcesFileAddressRange [
	"Test file position to source pointer address translation for the sources file"
	
	| sf a e |
	sf := CuisSourceFileArray new.
	(0 to: 16r1FFFFFF by: 811) do: [:ee | | a2 p i |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		i := sf fileIndexFromSourcePointer: a.
		self assert: i = 1.
		p := sf filePositionFromSourcePointer: a.
		self assert: p = e.
		a2 := sf sourcePointerFromFileIndex: 1 andPosition: p.
		self assert: a2 = a].
	(0 to: 16rFFFFFF by: 811) do: [:ee |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r1000000 and: 16r1FFFFFF)].
	(16r1000000 to: 16r1FFFFFF by: 811) do: [:ee |
		e _ ee // 32 * 32.
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r1000000 and: 16r2FFFFFF)]


]
