'From Cuis7.7 [latest update: #7779] on 23 December 2025 at 4:35:01 pm'!
'Description '!
!provides: 'ObjectProperties' 1 5!
SystemOrganization addCategory: #ObjectProperties!


!classDefinition: #ObjectWithProperties category: #ObjectProperties!
Object subclass: #ObjectWithProperties
	instanceVariableNames: 'propertyMap'
	classVariableNames: 'PropertyMaps'
	poolDictionaries: ''
	category: 'ObjectProperties'!
!classDefinition: 'ObjectWithProperties class' category: #ObjectProperties!
ObjectWithProperties class
	instanceVariableNames: ''!


!ObjectWithProperties methodsFor: 'accessing' stamp: 'jmv 16/Nov/2023 10:48:19'!
propertyMap
	propertyMap ifNil: [
		propertyMap := IdentityDictionary new ].
	^propertyMap! !

!ObjectWithProperties methodsFor: 'accessing' stamp: 'jmv 16/Nov/2023 10:47:54'!
propertyMapOrNil
	^propertyMap! !

!ObjectWithProperties methodsFor: 'accessing' stamp: 'jmv 16/Nov/2023 10:47:02'!
releasePropertyMap
	propertyMap := nil! !

!ObjectWithProperties methodsFor: 'copying' stamp: 'jmv 16/Nov/2023 10:50:19'!
postCopy
	propertyMap := nil! !

!ObjectWithProperties class methodsFor: 'accessing' stamp: 'jmv 10/Jul/2025 10:29:11'!
propertyMapFor: anObject

	^self propertyMaps
		at: anObject
		ifAbsentOrNilPut: [ IdentityDictionary new ]! !

!ObjectWithProperties class methodsFor: 'accessing' stamp: 'jmv 16/Nov/2023 10:43:04'!
propertyMapOrNilFor: anObject

	^self propertyMaps
		at: anObject
		ifAbsent: nil! !

!ObjectWithProperties class methodsFor: 'accessing' stamp: 'jmv 14/Jan/2025 12:50:40'!
propertyMaps

	PropertyMaps ifNil: [
		PropertyMaps := EphemeronIdentityDictionary new.
		].
	^PropertyMaps! !

!ObjectWithProperties class methodsFor: 'releasing' stamp: 'jmv 10/Jul/2025 10:29:41'!
releasePropertyMapFor: anObject

	self propertyMaps
		at: anObject
		put: nil! !

!Object methodsFor: '*ObjectProperties-querying / testing' stamp: 'jmv 16/Nov/2023 11:02:59'!
hasProperty: aSymbol

	^self propertyMapOrNil
		ifNil: false
		ifNotNil: [ :map | map includesKey: aSymbol ]! !

!Object methodsFor: '*ObjectProperties-querying / testing' stamp: 'jmv 16/Nov/2023 11:09:09'!
propertyAt: aSymbol

	^self propertyMapOrNil ifNotNil: [ :map |
		map at: aSymbol ifAbsent: nil ]! !

!Object methodsFor: '*ObjectProperties-querying / testing' stamp: 'jmv 16/Nov/2023 10:55:25'!
propertyAt: aSymbol ifAbsent: absentBlock

	^self propertyMapOrNil
		ifNil: absentBlock
		ifNotNil: [ :map |
			map at: aSymbol ifAbsent: absentBlock ]! !

!Object methodsFor: '*ObjectProperties-adding / removing' stamp: 'jmv 8/Dec/2023 09:01:05'!
propertyAt: aSymbol ifAbsentPut: aBlock
	^self propertyMap at: aSymbol ifAbsentPut: aBlock! !

!Object methodsFor: '*ObjectProperties-querying / testing' stamp: 'jmv 16/Nov/2023 11:15:47'!
propertyAt: aSymbol ifPresentDo: presentBlock

	^self propertyMapOrNil
		ifNotNil: [ :map |
			map at: aSymbol ifPresent: presentBlock ]! !

!Object methodsFor: '*ObjectProperties-adding / removing' stamp: 'jmv 16/Nov/2023 11:04:24'!
propertyAt: aSymbol put: propertyValue
	^self propertyMap at: aSymbol put: propertyValue! !

!Object methodsFor: '*ObjectProperties-private' stamp: 'jmv 16/Nov/2023 10:45:17'!
propertyMap
	^ObjectWithProperties propertyMapFor: self! !

!Object methodsFor: '*ObjectProperties-private' stamp: 'jmv 16/Nov/2023 10:47:50'!
propertyMapOrNil
	^ObjectWithProperties propertyMapOrNilFor: self! !

!Object methodsFor: '*ObjectProperties-enumerating' stamp: 'jmv 16/Nov/2023 11:23:35'!
propertyNamesAndValuesDo: aBlock

	^self propertyMapOrNil
		ifNotNil: [ :map | map keysAndValuesDo: aBlock ]! !

!Object methodsFor: '*ObjectProperties-enumerating' stamp: 'jmv 16/Nov/2023 11:05:50'!
propertyNamesDo: aBlock

	^self propertyMapOrNil
		ifNotNil: [ :map | map keysDo: aBlock ]! !

!Object methodsFor: '*ObjectProperties-private' stamp: 'jmv 16/Nov/2023 10:49:40'!
releasePropertyMap
	ObjectWithProperties releasePropertyMapFor: self! !

!Object methodsFor: '*ObjectProperties-adding / removing' stamp: 'jmv 16/Nov/2023 11:21:32'!
removeProperty: aSymbol
	"removes the property named aSymbol if it exists"

	| map |
	map := self propertyMapOrNil.
	map ifNil: [ ^self ].
	map removeKey: aSymbol ifAbsent: nil.
	map isEmpty ifTrue: [
		self releasePropertyMap ].! !
