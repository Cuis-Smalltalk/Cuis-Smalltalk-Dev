"
I'm represent an entry in the spaceTally.
"
Class {
	#name : 'SpaceTallyItem',
	#superclass : 'Object',
	#instVars : [
		'analyzedClassName',
		'codeSize',
		'instanceCount',
		'spaceForInstances'
	],
	#category : 'Tools-Profiling',
	#package : 'Tools',
	#tag : 'Profiling'
}

{ #category : 'instance creation' }
SpaceTallyItem class >> analyzedClassName: aClassName [

	^ self new
		analyzedClassName: aClassName ; yourself
		
]

{ #category : 'instance creation' }
SpaceTallyItem class >> analyzedClassName: aClassName codeSize: codeSize instanceCount: instanceCount spaceForInstances: spaceForInstances [

	^ self new
		analyzedClassName: aClassName ;
		codeSize: codeSize ;
		instanceCount: instanceCount ;
		spaceForInstances: spaceForInstances ; yourself
]

{ #category : 'accessing' }
SpaceTallyItem >> analyzedClassName [

	^ analyzedClassName
]

{ #category : 'accessing' }
SpaceTallyItem >> analyzedClassName: aClassName [

	analyzedClassName := aClassName
]

{ #category : 'accessing' }
SpaceTallyItem >> codeSize [

	^ codeSize
]

{ #category : 'accessing' }
SpaceTallyItem >> codeSize: aNumber [

	codeSize := aNumber
]

{ #category : 'accessing' }
SpaceTallyItem >> instanceCount [

	^ instanceCount
]

{ #category : 'accessing' }
SpaceTallyItem >> instanceCount: aNumber [

	instanceCount := aNumber
]

{ #category : 'printing' }
SpaceTallyItem >> printOn: aStream [

	analyzedClassName
		ifNotNil: [ aStream nextPutAll: analyzedClassName asPlainString]. 
	aStream nextPutAll: ' ('.
	codeSize
		ifNotNil: [ aStream nextPutAll: 'code size: ' ;  nextPutAll: codeSize printString]. 
	instanceCount
		ifNotNil: [ aStream nextPutAll: ' instance count: ' ;  nextPutAll: instanceCount printString]. 
	spaceForInstances
		ifNotNil: [ aStream nextPutAll: ' space for instances: ' ;  nextPutAll: spaceForInstances printString]. 
	aStream nextPut: $).
	
]

{ #category : 'accessing' }
SpaceTallyItem >> spaceForInstances [

	^ spaceForInstances
]

{ #category : 'accessing' }
SpaceTallyItem >> spaceForInstances: aNumber [

	spaceForInstances := aNumber
]
